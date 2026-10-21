# =============================================================================
# app_server.R
# Reactive server logic for the FGS Flood Guidance dashboard.
#
# Structure:
#   1. Source data.R so the fetch_* functions are available.
#   2. Define one reactive() per logical chunk of derived data.
#   3. Wire those reactives to output slots declared in app_ui.R.
#
# Two design choices worth knowing about:
#
#   FILTERING IS DONE IN R, NOT SQL.
#   We fetch all polygons / EA areas for the chosen statement and day in one
#   query, then filter in R against the source, matrix, and threshold inputs.
#   The volumes are small (tens of polygons, hundreds of areas per day) so
#   the round trips would dominate; doing filter logic in data.table is also
#   easier to debug than building dynamic IN clauses for the SQL.
#
#   THE MAP USES leafletProxy(), NOT renderLeaflet().
#   The map skeleton (basemap, initial view) is created once via renderLeaflet
#   on session start. Polygons are added and removed via leafletProxy in an
#   observe() block. This preserves the user's pan and zoom state across
#   reactive updates -- a forecaster zoomed into Cumbria stays zoomed in when
#   they tick a matrix cell. The Python ipyleaflet version had to rebuild from
#   scratch; this is leaflet's big advantage.
# =============================================================================


# data.R holds the fetch_* functions. local = TRUE keeps the source isolated
# from the global namespace where possible.
source("data.R", local = TRUE)


server <- function(input, output, session) {
  
  # ---------------------------------------------------------------------------
  # Statement list: fetched on session start and on each Refresh click.
  #
  # recent_rv is a reactiveVal so selected_statement() invalidates whenever
  # the list is refreshed, picking up any new statements Databricks has
  # ingested since the session opened.
  # ---------------------------------------------------------------------------

  # Bumped whenever the underlying data may have changed under the same inputs
  # (a refresh, new data, a demo scenario switch), so the map layers re-read.
  data_version <- reactiveVal(0L)

  # Demo mode keeps its scenario and "pipeline problem" switch in the session,
  # so two people using one deployed demo never affect each other. A restored
  # bookmark supplies the starting scenario.
  if (DEMO_MODE) {
    start <- isolate(input$demo_scenario)
    session$userData$demo_scenario <- if (!is.null(start) && start %in% DEMO_SCENARIOS) start else "developing"
    session$userData$demo_problem  <- isTRUE(isolate(input$demo_problem))
  }

  recent_rv <- reactiveVal(data.table())

  load_statements <- function(keep_selected = FALSE) {
    dt <- fetch_recent_statements(100L)
    recent_rv(dt)
    if (nrow(dt) == 0L) return()
    labels  <- sprintf("%s  (#%d)",
                       fmt_statement_time(dt$issued_at),
                       dt$statement_id)
    choices <- setNames(as.character(dt$statement_id), labels)
    # Keep the current selection if it still exists, with one exception: a
    # user who was looking at the newest statement follows to the new newest
    # when one arrives. A user who picked an older statement stays on it. A
    # restored bookmark (no previous newest known yet) is also left alone.
    current     <- isolate(input$statement_id)
    prev_newest <- isolate(newest_seen())
    newest_seen(choices[1])
    sel <- choose_selected_statement(choices, current, prev_newest, keep_selected)
    updateSelectInput(session, "statement_id", choices = choices, selected = sel)
  }

  newest_seen <- reactiveVal(NULL)

  # keep_selected = TRUE also on first load: a fresh session has no selection
  # (so it takes the newest) and a restored bookmark has one (so it is kept).
  load_statements(keep_selected = TRUE)

  # Refresh also drops the shared query cache, so an amended statement is
  # re-read rather than served from memory.
  # ignoreInit: an actionButton starts at 0, so without this the handler also ran
  # once at session start, wiping the cache and the missing-table gaps that the
  # first loads had only just found.
  observeEvent(input$refresh_data, {
    cache_clear()
    reset_gaps()
    load_statements(keep_selected = TRUE)
    data_version(isolate(data_version()) + 1L)
  }, ignoreInit = TRUE)


  # ---------------------------------------------------------------------------
  # Constituency/MP reference data.
  #
  # ea_area_constituency_lookup and mp_contact_details are static reference
  # tables, not statement-specific, so they're fetched once per session here
  # rather than re-queried on every filter change. Used by the CSV export and
  # by the area-list search box (constituency name as a search key).
  # ---------------------------------------------------------------------------

  ea_constituency_lookup <- fetch_ea_constituency_mp()

  # The nations chosen at the top of the page (see the Nations filter). NULL means all four.
  nation_codes <- reactive(input$nations)

  # The lookup limited to the chosen nations. An older lookup with no constituency_id is used as it is.
  scoped_lookup <- reactive({
    lk <- ea_constituency_lookup
    if (!("constituency_id" %in% names(lk))) return(lk)
    lk[in_nations(constituency_id, nation_codes())]
  })
  area_constituency_map <- reactive({
    scoped_lookup()[
      , .(constituency_names = paste(unique(constituency_name), collapse = ", ")),
      by = ea_area_code
    ]
  })
  
  
  # ===========================================================================
  # REACTIVE DATA LAYER
  # ===========================================================================
  
  # ---------------------------------------------------------------------------
  # selected_statement()
  #
  # Returns metadata for the FGS statement currently picked in the sidebar
  # dropdown. Title banner reads this for issued_at; filters() reads it for
  # statement_id.
  #
  # Rather than refetching from Databricks each time the user changes the
  # picker, we look the chosen statement up in the `recent` data.table we
  # already pulled at session start. That's free; the network round trip
  # would not be.
  # ---------------------------------------------------------------------------
  
  selected_statement <- reactive({
    sid <- as.integer(input$statement_id)
    req(sid)
    row <- recent_rv()[statement_id == sid]
    if (nrow(row) == 0L) return(NULL)
    as.list(row[1L])
  })
  
  
  # ---------------------------------------------------------------------------
  # filters()
  #
  # Collects every user input into a single named list. Downstream reactives
  # depend on this object, so they only invalidate when something that
  # actually matters changes. Without this layer, every reactive would
  # individually depend on input$src_river, input$src_surface, etc., and
  # changing one source checkbox would invalidate everything multiple times.
  #
  # The matrix_cells field is a data.table of the (x, y) pairs the user has
  # ticked. A data.table rather than a list-of-vectors because:
  #   - downstream we'll join it against fetch_risk_polygons() output to
  #     filter, and data.table joins are the fastest path
  #   - explicit columns are clearer than indexing into c("x" = ...) pairs
  # ---------------------------------------------------------------------------
  
  filters <- reactive({
    stmt <- selected_statement()
    req(stmt)                                   # bail if statement isn't yet loaded
    data_version()                              # re-read when the data under the same inputs changes
    
    # Map UI checkboxes to the source codes the database uses. isTRUE() guards
    # against the brief NULL state Shiny inputs can have on first render.
    source_codes <- c(
      if (isTRUE(input$src_river))   "river",
      if (isTRUE(input$src_surface)) "surface",
      if (isTRUE(input$src_coastal)) "coastal",
      if (isTRUE(input$src_ground))  "ground"
    )
    
    # Read every matrix checkbox. Each input id is mtx_{x}_{y}. We iterate
    # RISK_MATRIX (defined in global.R) so the source of truth for which
    # cells exist is in one place.
    ticked <- vapply(seq_len(nrow(RISK_MATRIX)), function(i) {
      id <- paste0("mtx_", RISK_MATRIX$x[i], "_", RISK_MATRIX$y[i])
      isTRUE(input[[id]])
    }, logical(1))
    
    selected_cells <- RISK_MATRIX[ticked, .(x, y)]   # data.table sub-selection
    
    # EA area types -- which of FWA / FAA the user wants on the map and
    # in the right-hand list. Stored as the codes the intersection table
    # uses, so the downstream filter is a direct %in% match.
    ea_types <- c(
      if (isTRUE(input$ea_type_fwa)) "flood_warning",
      if (isTRUE(input$ea_type_faa)) "flood_alert"
    )
    
    list(
      statement_id     = as.integer(stmt$statement_id),
      day_index        = as.integer(input$day_index),
      source_codes     = source_codes,
      selected_cells   = selected_cells,
      ea_types         = ea_types,
      min_intersection = input$intersect_threshold / 100   # convert % to 0-1
    )
  })


  # ---------------------------------------------------------------------------
  # Map busy flags.
  #
  # Drive the spinner overlay shown above the map while the three layer
  # observers are re-fetching from Databricks. Set TRUE whenever filters()
  # changes (the trigger behind nearly every map redraw), then cleared
  # individually by each layer observer once its own draw finishes -- so the
  # spinner clears only when the slowest of the visible layers is done.
  # ---------------------------------------------------------------------------

  # Every map layer runs inside with_layer_guard() (helpers.R), so its flag is
  # cleared however it ends and a real error is shown, not just logged.
  # One box per distinct failure: a repeat replaces the earlier box instead of stacking.
  layer_notify <- function(msg) showNotification(msg, type = "error", duration = 15, id = notification_id(msg, "layer_"))

  busy_polygons <- reactiveVal(FALSE)
  busy_ea       <- reactiveVal(FALSE)
  busy_const    <- reactiveVal(FALSE)

  observeEvent(filters(), {
    busy_polygons(TRUE)
    busy_ea(isTRUE(input$layer_ea_areas))
    busy_const(isTRUE(input$layer_constituencies))
  })

  output$map_loading_overlay <- renderUI({
    if (isTRUE(busy_polygons()) || isTRUE(busy_ea()) || isTRUE(busy_const())) {
      tags$div(class = "map-spinner-overlay", tags$div(class = "map-spinner"))
    }
  })


  # ---------------------------------------------------------------------------
  # polygons()
  #
  # Risk polygons for the current statement + day, after applying source and
  # matrix filters. Returns an sf object for direct use by leaflet.
  #
  # Steps:
  #   1. Fetch all polygons for the statement-day.
  #   2. Filter by source (data.table %in%).
  #   3. Filter by selected matrix cells via a non-equi join on (risk_x, risk_y).
  #
  # The matrix filter uses an inner join rather than a row-by-row apply() --
  # data.table's [i, on = ...] joining syntax is both faster and clearer for
  # this kind of "keep rows whose key is in this other set" operation.
  # ---------------------------------------------------------------------------
  
  polygons <- reactive({
    f <- filters()
    req(f)
    
    shape <- fetch_risk_polygons(f$statement_id, f$day_index)
    if (nrow(shape) == 0L) return(shape)
    
    # Subset rows whose source is in the selected set. sf's [ ] subsetting
    # preserves the geometry column, so the result is still an sf object.
    shape <- shape[tolower(shape$source) %in% f$source_codes, ]
    if (nrow(shape) == 0L) return(shape)
    
    # If the user has unticked every cell, return zero rows (an empty filter
    # means "show nothing").
    if (nrow(f$selected_cells) == 0L) return(shape[0, ])
    
    # data.table inner join on (risk_x, risk_y) -- this is the fast path for
    # "keep rows whose key is in this other table". We pull the join keys out
    # of the sf object, do the join, and use the resulting row indices to
    # subset the sf object so we don't lose the geometry column.
    keys <- as.data.table(sf::st_drop_geometry(shape)[, c("risk_x", "risk_y")])
    keys[, row_idx := .I]                       # remember original row positions
    kept <- keys[f$selected_cells,              # right table: ticked cells
                 on = c("risk_x" = "x", "risk_y" = "y"),
                 nomatch = NULL]                # inner join: drop unmatched rows
    
    shape[kept$row_idx, ]
  })
  
  
  # ---------------------------------------------------------------------------
  # ea_areas()
  #
  # EA flood areas affected by the current statement + day, after the source
  # filter and intersection threshold are applied.
  #
  # Returns a data.table. The risk-coloured swatches in the right panel are
  # driven by the risk_colour column.
  # ---------------------------------------------------------------------------
  
  ea_areas <- reactive({
    f <- filters()
    req(f)
    
    areas <- fetch_ea_areas_for_statement(f$statement_id, f$day_index)
    # Three filters: source must be selected, area type must be ticked,
    # intersection must clear the threshold.
    areas[tolower(source) %in% f$source_codes
          & ea_area_type  %in% f$ea_types
          & clears_threshold(intersection_pct, f$min_intersection)]
  })
  
  
  # ---------------------------------------------------------------------------
  # ea_geometry()
  #
  # EA flood areas with polygon geometry, for drawing the map layer. Same
  # filters as ea_areas() but pulls the shapes via the join in
  # fetch_ea_geometry(). Kept as a separate reactive because:
  #   - the geometry query is more expensive than the metadata query
  #   - it should only fire when the EA layer is toggled on
  # ---------------------------------------------------------------------------
  
  ea_geometry <- reactive({
    f <- filters()
    req(f)
    req(isTRUE(input$layer_ea_areas))   # don't query unless layer is on
    
    shape <- fetch_ea_geometry(f$statement_id, f$day_index)
    if (nrow(shape) == 0L) return(shape)
    
    shape <- shape[tolower(shape$source) %in% f$source_codes, ]
    shape <- shape[shape$ea_area_type  %in% f$ea_types,     ]
    shape[clears_threshold(shape$intersection_pct, f$min_intersection), ]
  })
  
  
  # ---------------------------------------------------------------------------
  # constituencies()
  #
  # Affected parliamentary constituencies after the source, matrix-cell, and
  # threshold filters -- metadata only, no geometry, so it's cheap enough to
  # back the stat card regardless of whether the constituency map layer is
  # switched on. Mirrors ea_areas().
  # ---------------------------------------------------------------------------

  constituencies <- reactive({
    f <- filters()
    req(f)

    areas <- fetch_constituencies_for_statement(f$statement_id, f$day_index)
    areas <- areas[tolower(source) %in% f$source_codes & clears_threshold(intersection_pct, f$min_intersection)
                   & in_nations(constituency_id, nation_codes())]

    if (nrow(f$selected_cells) == 0L) return(areas[0])
    areas[f$selected_cells, on = c("risk_x" = "x", "risk_y" = "y"), nomatch = NULL]
  })


  # ---------------------------------------------------------------------------
  # constituency_geometry()
  #
  # Affected parliamentary constituencies with polygon geometry, after the
  # same source, matrix-cell, and threshold filters as constituencies().
  # Kept separate because the geometry query is more expensive and should
  # only fire when the constituency layer is toggled on.
  # ---------------------------------------------------------------------------

  constituency_geometry <- reactive({
    f <- filters()
    req(f)
    req(isTRUE(input$layer_constituencies))

    shape <- fetch_constituency_geometry(f$statement_id, f$day_index)
    if (nrow(shape) == 0L) return(shape)

    shape <- shape[tolower(shape$source) %in% f$source_codes
                   & clears_threshold(shape$intersection_pct, f$min_intersection)
                   & in_nations(shape$constituency_id, nation_codes()), ]
    if (nrow(shape) == 0L) return(shape)

    if (nrow(f$selected_cells) == 0L) return(shape[0, ])

    # Same sf row_idx workaround as polygons() -- sf's [ ] subsetting can't
    # be driven directly by a data.table i-join, so the join runs on a plain
    # data.table of the keys and the resulting row positions are used to
    # subset the sf object, keeping the geometry column intact.
    keys <- as.data.table(sf::st_drop_geometry(shape)[, c("risk_x", "risk_y")])
    keys[, row_idx := .I]
    kept <- keys[f$selected_cells, on = c("risk_x" = "x", "risk_y" = "y"), nomatch = NULL]

    shape[kept$row_idx, ]
  })
  
  
  # ---------------------------------------------------------------------------
  # counts()
  #
  # Headline counts for the three stat cards. Single query, three integers.
  # Re-runs only when statement_id or day_index changes.
  # ---------------------------------------------------------------------------
  
  counts <- reactive({
    f <- filters()
    req(f)
    fetch_summary_counts(f$statement_id, f$day_index)
  })
  
  
  # ===========================================================================
  # OUTPUT BINDINGS
  # ===========================================================================
  
  # ---------------------------------------------------------------------------
  # Title and subtitle for the map header.
  # ---------------------------------------------------------------------------
  
  output$map_title <- renderText({
    stmt <- selected_statement()
    if (is.null(stmt)) return("No statement loaded")
    
    sprintf("Forecast Day %d - issued %s",
            as.integer(input$day_index),
            fmt_statement_time(stmt$issued_at, "%d %b %Y %H:%M"))
  })
  
  output$map_subtitle <- renderText({
    stmt <- selected_statement()
    sprintf("Statement %s - %d polygons - %d EA Warnings",
            if (is.null(stmt)) "-" else stmt$statement_id,
            nrow(polygons()), uniqueN(ea_areas()$ea_area_code))
  })


  # ---------------------------------------------------------------------------
  # field_or_dash()
  #
  # Small helper shared by the headline notice and the source info boxes:
  # statement text fields can be NA or empty depending on what the FGS
  # pipeline filled in, and we want a calm "-" rather than blank space or an
  # NA-coercion error in either case.
  # ---------------------------------------------------------------------------

  field_or_dash <- function(value) {
    if (is.null(value) || isTRUE(is.na(value)) || !nzchar(value)) "-" else value
  }

  risk_border_colour <- function(value) {
    if (grepl("HIGH",   value, fixed = TRUE)) return(unname(RISK_COLOUR_HEX["red"]))
    if (grepl("MEDIUM", value, fixed = TRUE)) return(unname(RISK_COLOUR_HEX["amber"]))
    if (grepl("(?<!VERY )LOW", value, perl = TRUE)) return(unname(RISK_COLOUR_HEX["yellow"]))
    if (grepl("VERY LOW",      value, fixed = TRUE)) return(unname(RISK_COLOUR_HEX["green"]))
    "#dee2e6"
  }


  # ---------------------------------------------------------------------------
  # Headline notice.
  #
  # Shown above the map for every loaded statement, mirroring the quiet-state
  # notice's styling so the two read as the same family of "context banner
  # above the map" rather than competing visual languages.
  # ---------------------------------------------------------------------------

  output$headline_notice <- renderUI({
    stmt <- selected_statement()
    if (is.null(stmt)) return(NULL)

    headline <- field_or_dash(stmt$headline)
    if (headline == "-") return(NULL)

    tags$div(
      class = "headline-notice",
      style = paste0("border-left-color: ", risk_border_colour(headline), ";"),
      tags$strong("Headline: "),
      headline
    )
  })


  # ---------------------------------------------------------------------------
  # Statement info boxes (per-source forecast text, England-wide forecast,
  # PDF link).
  #
  # All sourced from the statement row, not the filtered polygon data --
  # these are the FGS pipeline's own narrative fields for the statement as a
  # whole, not derived from what's currently ticked in the sidebar.
  # ---------------------------------------------------------------------------

  output$statement_info <- renderUI({
    stmt <- selected_statement()
    if (is.null(stmt)) return(NULL)

    source_box <- function(label, value, colour = NULL) {
      display <- field_or_dash(value)
      border  <- if (!is.null(colour)) colour else risk_border_colour(display)
      tags$div(
        class = "source-info-card",
        style = paste0("border-left-color: ", border, ";"),
        tags$div(label, class = "source-info-label"),
        tags$div(display, class = "source-info-value")
      )
    }

    pdf_url <- field_or_dash(stmt$pdf_url)

    tagList(
      tags$div(
        class = "source-info-stack mt-2",
        source_box("England forecast", stmt$england_forecast, colour = "#1f6aa5"),
        source_box("River",   stmt$source_river),
        source_box("Coastal", stmt$source_coastal),
        source_box("Surface", stmt$source_surface),
        source_box("Ground",  stmt$source_ground)
      ),
      if (pdf_url != "-") {
        tags$a(href = pdf_url, target = "_blank", rel = "noopener noreferrer",
               class = "btn btn-sm btn-outline-secondary mt-2",
               "View FGS PDF")
      }
    )
  })


  # ---------------------------------------------------------------------------
  # Stat cards.
  #
  # All three counts come from the filtered reactives so the headline number
  # matches what's actually on the map -- not the raw statement-day total.
  #
  # format() with big.mark = "," puts a comma thousand separator on the
  # counts once they get big enough to need one. Cheap, and avoids the
  # dashboard looking amateur with "1247" instead of "1,247".
  # ---------------------------------------------------------------------------

  output$count_polygons       <- renderText(format(nrow(polygons()), big.mark = ","))
  output$count_ea_areas       <- renderText(format(uniqueN(ea_areas()$ea_area_code), big.mark = ","))
  output$count_constituencies <- renderText(format(uniqueN(constituencies()$constituency_id), big.mark = ","))
  
  
  # ---------------------------------------------------------------------------
  # Quiet-state notice.
  #
  # Surfaced above the map when there are no polygons for the current
  # statement-day. We deliberately distinguish two cases:
  #   - polygon_count == 0: FGS is quiet, no risk reported at all
  #   - polygon_count > 0 but filters hide all: user's controls excluding
  #     them
  # The first is an operational fact about the statement, worth flagging.
  # The second is the user's own doing, no need to flag.
  # ---------------------------------------------------------------------------
  
  output$quiet_notice <- renderUI({
    c <- counts()
    if (c$polygon_count == 0L) {
      tags$div(
        class = "quiet-notice",
        tags$strong("Quiet FGS."),
        " No risk polygons issued for the selected statement and day. ",
        "Try another forecast day, or pick an earlier statement."
      )
    }
    # otherwise return nothing (no notice rendered)
  })
  
  
  # ---------------------------------------------------------------------------
  # Affected-areas list.
  #
  # Built as raw HTML via htmltools, not a DataTable -- the row layout (4px
  # coloured bar + two lines of text) isn't a natural fit for a tabular
  # widget. We cap visible rows at 20 and show a "+ N more" footer so the
  # panel doesn't run the page to the floor in a major event.
  #
  # Each row carries an onclick that sets input$area_row_click to the area's
  # code, picked up by the observeEvent() near the map code to fly there and
  # open a popup. constituency_names is joined in purely for this output --
  # the search box matches against it, but it's not part of ea_areas() since
  # the count/subtitle outputs don't need it.
  # ---------------------------------------------------------------------------

  output$ea_area_list <- renderUI({
    areas <- ea_areas()

    if (is.null(areas) || nrow(areas) == 0L) {
      return(tags$p("No EA Warnings above threshold.", class = "text-muted small"))
    }

    areas <- area_constituency_map()[areas, on = "ea_area_code",
                                    .(ea_area_code     = i.ea_area_code,
                                      ea_area_name     = i.ea_area_name,
                                      ea_area_type     = i.ea_area_type,
                                      source           = i.source,
                                      risk_level       = i.risk_level,
                                      risk_colour      = i.risk_colour,
                                      intersection_pct = i.intersection_pct,
                                      constituency_names)]
    areas[is.na(constituency_names), constituency_names := ""]

    search <- trimws(if (is.null(input$area_search)) "" else input$area_search)
    if (nzchar(search)) {
      # fixed = TRUE ignores ignore.case, so compare lower-cased text instead.
      search_lc <- tolower(search)
      areas <- areas[grepl(search_lc, tolower(ea_area_name), fixed = TRUE) |
                       grepl(search_lc, tolower(constituency_names), fixed = TRUE)]
    }

    if (nrow(areas) == 0L) {
      return(tags$p(sprintf("No EA Warnings match \"%s\".", search),
                     class = "text-muted small"))
    }

    n_visible <- min(nrow(areas), 20L)

    rows <- lapply(seq_len(n_visible), function(i) {
      r <- areas[i]                             # one-row data.table
      tags$div(
        class   = "area-row",
        onclick = sprintf("Shiny.setInputValue('area_row_click', '%s', {priority: 'event'})",
                           r$ea_area_code),
        tags$div(class = paste("risk-bar", tolower(r$risk_colour))),
        tags$div(
          tags$strong(r$ea_area_name),
          tags$br(),
          tags$small(
            sprintf("%s - %s - %s",
                    r$ea_area_type, r$source,
                    intersection_label(r$intersection_pct)),
            class = "text-muted"
          )
        )
      )
    })

    # Footer when there are more rows than we're showing.
    if (nrow(areas) > n_visible) {
      rows[[length(rows) + 1L]] <- tags$small(
        sprintf("+ %d more", nrow(areas) - n_visible),
        class = "text-muted d-block text-center mt-2"
      )
    }

    do.call(tagList, rows)
  })


  # ---------------------------------------------------------------------------
  # Affected-areas CSV export.
  #
  # Joins the currently filtered ea_areas() against the constituency/MP
  # lookup (fetched once per session, see ea_constituency_lookup above) so
  # forecasters can hand the affected-area list to colleagues with MP
  # contact details attached.
  #
  # allow.cartesian = TRUE because an EA area can overlap more than one
  # constituency -- the export should show one row per area/constituency
  # pair, not silently drop the extra overlaps.
  # ---------------------------------------------------------------------------

  output$download_ea_areas <- downloadHandler(
    filename = function() {
      f <- filters()
      sprintf("ea_warnings_statement_%d_day_%d.csv", f$statement_id, f$day_index)
    },
    content = function(file) {
      areas  <- ea_areas()
      lookup <- scoped_lookup()

      # lookup[areas, on=...] looks up each area's matching lookup rows
      # (expanded per match when an area spans more than one constituency).
      # The j expression pulls area columns via the i. prefix and lookup
      # columns directly, so both sides survive the join.
      export <- lookup[areas, on = "ea_area_code", allow.cartesian = TRUE,
                        .(ea_area_code     = i.ea_area_code,
                          ea_area_name     = i.ea_area_name,
                          ea_area_type     = i.ea_area_type,
                          source           = i.source,
                          risk_level       = i.risk_level,
                          risk_colour      = i.risk_colour,
                          intersection_pct = i.intersection_pct,
                          constituency_name,
                          constituency_overlap_pct,
                          mp_name, mp_party, mp_email, mp_phone)]

      fwrite(export, file)
    }
  )


  # ---------------------------------------------------------------------------
  # Map.
  #
  # Two-stage rendering:
  #   1. renderLeaflet runs once per session and creates the skeleton: basemap,
  #      initial view centred on the UK midlands at zoom 6.
  #   2. observe() watches the polygons() reactive and the layer toggle. When
  #      either changes, it clears the "polygons" group on the existing map
  #      and adds the current set. The user's pan/zoom is preserved because
  #      the map widget itself is not torn down.
  #
  # Group naming matters: clearGroup("polygons") only removes layers added
  # with the same group name. If we later add EA area markers as a separate
  # layer, they'd go in a different group and be managed independently.
  # ---------------------------------------------------------------------------
  
  output$live_map <- leaflet::renderLeaflet({
    m <- leaflet::leaflet()

    # Four base layers as named groups, switched via the layer control --
    # only one base group is shown at a time, leaflet handles that natively.
    # Street map default; satellite and terrain give forecasters context
    # (river courses, contours) the street map doesn't show.
    m <- leaflet::addProviderTiles(m, leaflet::providers$OpenStreetMap,    group = "Street")
    m <- leaflet::addProviderTiles(m, leaflet::providers$CartoDB.Positron, group = "Light")
    m <- leaflet::addProviderTiles(m, leaflet::providers$Esri.WorldImagery, group = "Satellite")
    m <- leaflet::addProviderTiles(m, leaflet::providers$OpenTopoMap,      group = "Terrain")

    m <- leaflet::addLayersControl(
      m,
      baseGroups    = c("Street", "Light", "Satellite", "Terrain"),
      options       = leaflet::layersControlOptions(collapsed = TRUE),
      position      = "topright"
    )

    m <- leaflet::setView(m, lng = -2.0, lat = 53.2, zoom = 6)
    m
  })
  
  observe(with_layer_guard("FGS risk polygons", busy_polygons, notify = layer_notify, expr = {
    shape <- polygons()

    # Get a handle to the existing map and clear last render's polygons.
    proxy <- leaflet::leafletProxy("live_map")
    proxy <- leaflet::clearGroup(proxy, "polygons")

    # If polygons should be hidden or there's nothing to draw, we're done.
    if (!isTRUE(input$layer_polygons))       { busy_polygons(FALSE); return() }
    if (is.null(shape) || nrow(shape) == 0L) { busy_polygons(FALSE); return() }

    # Look up hex colours for each row's risk_colour band.
    fill_hex <- unname(RISK_COLOUR_HEX[trimws(tolower(shape$risk_colour))])
    fill_hex[is.na(fill_hex)] <- "#ff00ff"   # magenta flags any unmatched colour values

    # Build a popup string per polygon. paste0() over the vector columns
    # produces a vector of length nrow(shape), which leaflet maps row-by-row
    # to each polygon.
    popup_html <- paste0(
      "<strong>", shape$source, " - ", shape$risk_level, "</strong><br>",
      "Likelihood: ", shape$likelihood_label, "<br>",
      "Impact: ",     shape$impact_label
    )
    
    # Coastal geometries are lines. addPolygons() would close each into a filled
    # shape, so lines are drawn as lines, in the same colour, and areas as before.
    is_line <- line_rows(shape)

    if (any(!is_line)) {
      leaflet::addPolygons(
        proxy,
        data        = shape[!is_line, ],
        group       = "polygons",
        color       = fill_hex[!is_line],
        weight      = 3,
        fillColor   = fill_hex[!is_line],
        fillOpacity = 0.60,
        popup       = popup_html[!is_line],
        highlightOptions = leaflet::highlightOptions(
          weight       = 3,
          fillOpacity  = 0.65,
          bringToFront = TRUE
        )
      )
    }
    if (any(is_line)) {
      leaflet::addPolylines(
        proxy,
        data    = shape[is_line, ],
        group   = "polygons",
        color   = fill_hex[is_line],
        weight  = 6,
        opacity = 0.9,
        popup   = popup_html[is_line],
        highlightOptions = leaflet::highlightOptions(weight = 8, bringToFront = TRUE)
      )
    }

    busy_polygons(FALSE)
  }))
  
  
  # ---------------------------------------------------------------------------
  # EA flood areas layer.
  #
  # Drawn at lower opacity than the FGS polygons so they read as background
  # context, with the FGS polygons sitting clearly on top. The layer group
  # is named separately so clearGroup() can drop it without disturbing the
  # FGS polygons.
  #
  # Colour comes from ea_area_type (fixed FWA/FAA palette), not risk_colour --
  # the EA layer's job is to show which areas exist, not to repeat the risk
  # signal the FGS layer already carries. Using risk_colour here made FWAs
  # and FAAs indistinguishable from FGS polygons on the map.
  # ---------------------------------------------------------------------------

  observe(with_layer_guard("EA flood areas", busy_ea, notify = layer_notify, expr = {
    proxy <- leaflet::leafletProxy("live_map")
    proxy <- leaflet::clearGroup(proxy, "ea_areas")

    if (!isTRUE(input$layer_ea_areas)) { busy_ea(FALSE); return() }

    shape <- ea_geometry()
    if (is.null(shape) || nrow(shape) == 0L) { busy_ea(FALSE); return() }

    fill_hex <- unname(EA_AREA_TYPE_HEX[trimws(tolower(shape$ea_area_type))])
    fill_hex[is.na(fill_hex)] <- "#808080"   # grey fallback for any unmatched type

    popup_html <- paste0(
      "<strong>", shape$ea_area_name, "</strong><br>",
      "<small>", shape$ea_area_type, " &middot; ", shape$source, "</small><br>",
      "Target Area ID: ", shape$ea_area_code, "<br>",
      "Risk: ", shape$risk_level, "<br>",
      "Intersection: ", intersection_label(shape$intersection_pct)
    )
    
    leaflet::addPolygons(
      proxy,
      data        = shape,
      group       = "ea_areas",
      color       = "#000000",
      weight      = 1,
      fillColor   = fill_hex,
      fillOpacity = 0.35,           # still below the FGS layer's 0.6
      popup       = popup_html,
      highlightOptions = leaflet::highlightOptions(
        weight       = 2,
        fillOpacity  = 0.5,
        bringToFront = FALSE        # keep FGS polygons clickable on top
      )
    )

    busy_ea(FALSE)
  }))
  
  
  # ---------------------------------------------------------------------------
  # Constituency layer.
  #
  # Outline only, no fill -- the FGS and EA layers carry the colour load and
  # adding a third coloured layer is too noisy. The outlines give enough
  # constituency context for an MP-notification eyeball check.
  # ---------------------------------------------------------------------------
  
  observe(with_layer_guard("constituency outlines", busy_const, notify = layer_notify, expr = {
    proxy <- leaflet::leafletProxy("live_map")
    proxy <- leaflet::clearGroup(proxy, "constituencies")

    if (!isTRUE(input$layer_constituencies)) { busy_const(FALSE); return() }

    shape <- constituency_geometry()
    if (is.null(shape) || nrow(shape) == 0L) { busy_const(FALSE); return() }

    popup_html <- paste0(
      "<strong>", shape$constituency_name, "</strong><br>",
      "Risk: ", shape$risk_level
    )

    leaflet::addPolygons(
      proxy,
      data        = shape,
      group       = "constituencies",
      color       = "#2c3e50",      # dark grey outline
      weight      = 1,
      # fill        = FALSE,
      fillColor   = "#2c3e50",
      fillOpacity = 0.40,           # still below the FGS layer's 0.60
      popup       = popup_html,
      highlightOptions = leaflet::highlightOptions(
        weight       = 2.5,
        fillOpacity  = 0.30,
        fillColor    = "#2c3e50",
        bringToFront = FALSE
      )
    )

    busy_const(FALSE)
  }))


  # ---------------------------------------------------------------------------
  # Area-list row click -> map.
  #
  # Clicking a row in the Affected Target Areas panel flies the map to that
  # area's geometry and opens a popup at its centroid, turning on the EA
  # layer first if it's currently off so the polygon is visible to fly to.
  # Re-fetches geometry directly (rather than reusing ea_geometry(), which
  # is gated on the layer toggle) since the clicked row may arrive before
  # the layer is switched on.
  # ---------------------------------------------------------------------------

  observeEvent(input$area_row_click, {
    f <- filters()
    req(f)

    if (!isTRUE(input$layer_ea_areas)) {
      updateCheckboxInput(session, "layer_ea_areas", value = TRUE)
    }

    shape <- fetch_ea_geometry(f$statement_id, f$day_index)
    matched <- shape[shape$ea_area_code == input$area_row_click, ]
    if (nrow(matched) == 0L) return()

    # Popup position: bbox centre rather than a true geometric centroid.
    # st_union()/st_centroid() on this geometry hit s2 validity errors
    # (degenerate rings, self-intersecting edges) on some areas' source
    # GeoJSON, and a precise centroid isn't needed here -- the popup just
    # needs a point inside the area's general vicinity.
    bbox     <- sf::st_bbox(matched)
    centroid <- c((bbox[["xmin"]] + bbox[["xmax"]]) / 2,
                  (bbox[["ymin"]] + bbox[["ymax"]]) / 2)

    popup_html <- paste0(
      "<strong>", matched$ea_area_name[1], "</strong><br>",
      "<small>", matched$ea_area_type[1], " &middot; ", matched$source[1], "</small><br>",
      "Target Area ID: ", matched$ea_area_code[1], "<br>",
      "Risk: ", matched$risk_level[1], "<br>",
      "Intersection: ", intersection_label(matched$intersection_pct[1])
    )

    proxy <- leaflet::leafletProxy("live_map")
    proxy <- leaflet::flyToBounds(proxy,
                                   lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
                                   lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]])
    leaflet::addPopups(proxy,
                        lng     = centroid[1],
                        lat     = centroid[2],
                        popup   = popup_html,
                        layerId = "area_click_popup")
  })


  # ===========================================================================
  # PIPELINE PRODUCTS (FGS_Notebooks jobs 18 to 22)
  #
  # Every fetch below is wrapped so a missing or stale table degrades to an
  # empty view with a message, never a crashed session. The Interrogator map
  # does not depend on any of these.
  # ===========================================================================

  # Tables the pipeline has not built yet (name -> notebook number). Filled by
  # safe_fetch() when the warehouse says a table does not exist, cleared on
  # Refresh, and shown on screen so a missing table is never a silent gap.
  # One reactiveVal holding a named list, not reactiveValues: a reactiveValues
  # entry cannot be deleted (assigning NULL leaves the key listed), so a cleared
  # gap would still show.
  data_gaps <- reactiveVal(list())
  reset_gaps <- function() data_gaps(list())
  has_gap <- function(table) table %in% names(isolate(data_gaps()))

  logged_failures <- new.env()    # one console line per distinct failure, not one per click

  # Runs one fetch. Never throws: returns NULL on failure, after saying why in
  # the console (naming the fetch) and, for the two failures we can explain, on
  # screen: a table that does not exist yet, and a warehouse that cannot be reached.
  safe_fetch <- function(fn, ...) {
    label <- deparse(substitute(fn))
    tryCatch(fn(...), error = function(e) {
      msg <- conditionMessage(e)
      key <- paste(label, substr(msg, 1L, 80L))
      # A dropped connection is logged by the database layer (db_query), which knows how
      # long it has lasted. Logging it here too would repeat it.
      if (!is_connection_error(msg) && is.null(logged_failures[[key]])) {
        assign(key, TRUE, envir = logged_failures)
        message("[", format(Sys.time(), "%H:%M:%S"), "] ", label, " failed: ", msg)
      }
      gap <- missing_table_info(msg)
      if (!is.null(gap)) {
        g <- isolate(data_gaps())
        if (!(gap$table %in% names(g))) { g[[gap$table]] <- gap$job; data_gaps(g) }
      } else if (is_connection_error(msg)) {
        showNotification("Could not reach the Databricks warehouse. The app is showing the last data it has and will keep trying.",
                         type = "warning", id = "warehouse_unreachable", duration = 12)
      }
      NULL
    })
  }

  # Shown above every tab when a pipeline table is missing.
  output$data_gaps_banner <- renderUI({
    g <- data_gaps()
    if (length(g) == 0L) return(NULL)
    tags$div(class = "gaps-banner", role = "status", gaps_message(g, location = paste0(CAT, ".", SCH)))
  })

  gap_text <- function(table, what) {
    j <- PIPELINE_TABLE_JOBS[table]
    sprintf("%s unavailable: the table %s was not found%s.", what, table,
            if (is.na(j)) "" else sprintf(" (built by pipeline notebook %d)", j))
  }

  # Rank number -> label, for display. Out-of-range or NA becomes "None".
  rank_label <- function(r) {
    r <- suppressWarnings(as.integer(r))
    r[is.na(r) | r < 0L | r > 4L] <- 0L
    RANK_LABELS[r + 1L]
  }

  # ISO timestamp string -> "YYYY-MM-DD HH:MM" without timezone arithmetic.
  short_ts <- function(x) sub("T", " ", substr(as.character(x), 1L, 16L), fixed = TRUE)

  # Loaded at session start and again on each Refresh click, alongside the
  # statement list. NULL means "could not load".
  health_rv    <- reactiveVal(NULL)
  situation_rv <- reactiveVal(NULL)
  briefing_rv  <- reactiveVal(NULL)
  exposure_rv  <- reactiveVal(NULL)
  history_rv   <- reactiveVal(NULL)
  exposure_scoped <- reactive({
    e <- exposure_rv()
    if (is.null(e)) NULL else e[in_nations(constituency_id, nation_codes())]
  })

  load_products <- function() {
    health_rv(safe_fetch(fetch_pipeline_health))
    situation_rv(safe_fetch(fetch_situation))
    briefing_rv(safe_fetch(fetch_briefing_drafts))
    exposure_rv(safe_fetch(fetch_exposure, 12L))
    history_rv(safe_fetch(fetch_warning_history))
  }

  load_products()
  observeEvent(input$refresh_data, load_products(), ignoreInit = TRUE)


  # ---------------------------------------------------------------------------
  # Briefing review state.
  #
  # Empty in the live app: briefings are read-only there until someone decides
  # who may record a review. In demo mode the buttons below change this state,
  # which lives in the session only. There is no code path from here to any
  # warehouse: the only writers are these two reactive values.
  # ---------------------------------------------------------------------------

  review_status <- reactiveVal(character())            # constituency_id -> status
  review_log    <- reactiveVal(data.table(
    at = character(), statement = integer(), constituency = character(), mp = character(),
    action = character(), by = character()))

  briefing_effective <- reactive({
    b  <- briefing_rv()
    if (!is.null(b)) b <- b[in_nations(constituency_id, nation_codes())]
    st <- review_status()
    if (is.null(b) || length(st) == 0L) return(b)
    b <- copy(b)
    idx <- match(b$constituency_id, names(st))
    hit <- !is.na(idx)
    b[hit, `:=`(status = unname(st[idx[hit]]), reviewed_by = "Demo forecaster")]
    b[]
  })

  reset_review <- function() {
    review_status(character())
    review_log(review_log()[0L])
  }

  record_review <- function(action) {
    row <- input$situation_table_rows_selected
    req(row, is.null(poc_past_id()))        # reviews are recorded against the live batch only
    d <- situation_filtered()
    cid <- d$constituency_id[row]
    b <- briefing_rv()
    req(!is.null(b), cid %in% b$constituency_id)
    st <- review_status(); st[[cid]] <- action; review_status(st)
    review_log(rbind(review_log(), data.table(
      at = format(Sys.time(), "%H:%M:%S"), statement = b$statement_id[b$constituency_id == cid][1L],
      constituency = d$constituency_name[row], mp = d$mp_name[row], action = action, by = "Demo forecaster")))
    showNotification(sprintf("%s: marked %s (demonstration only, not saved).", d$constituency_name[row], action),
                     type = "message", duration = 4)
  }

  if (DEMO_MODE) {
    observeEvent(input$rv_reviewed, record_review("reviewed"))
    observeEvent(input$rv_sent,     record_review("sent"))
    observeEvent(input$rv_rejected, record_review("rejected"))
    observeEvent(input$demo_reset,  { reset_review(); showNotification("Demo actions cleared.", duration = 3) })

    observeEvent(input$demo_scenario, {
      req(input$demo_scenario %in% DEMO_SCENARIOS)
      if (identical(session$userData$demo_scenario, input$demo_scenario)) return()
      session$userData$demo_scenario <- input$demo_scenario
      withProgress(message = "Building the scenario", value = 0.3, {
        reset_review()
        load_statements(keep_selected = TRUE)
        load_products()
        data_version(isolate(data_version()) + 1L)
      })
    }, ignoreInit = TRUE)

    observeEvent(input$demo_problem, {
      session$userData$demo_problem <- isTRUE(input$demo_problem)
      health_rv(safe_fetch(fetch_pipeline_health))
    }, ignoreInit = TRUE)

    output$audit_table <- DT::renderDataTable({
      lg <- review_log()
      DT::datatable(
        setnames(copy(lg), c("Time", "Statement", "Constituency", "MP", "Action", "By")),
        rownames = FALSE, selection = "none",
        options = list(dom = "tip", pageLength = 15, ordering = TRUE,
                       language = list(emptyTable = "No actions yet. Select a constituency on Constituency situation and mark its briefing.")))
    })
  }


  # ---------------------------------------------------------------------------
  # Overview (senior forecasters and managers).
  #
  # One screen from tables the app already holds: nothing here runs a query of
  # its own beyond what the other tabs use.
  # ---------------------------------------------------------------------------

  observeEvent(input$nav_to, {
    req(input$nav_to %in% c("Overview", "FGS Interrogator", "Constituency situation", "Exposure history", "Guide"))
    updateTabsetPanel(session, "main_tabs", selected = input$nav_to)
  })

  nav_link <- function(label, to) {
    tags$a(href = "#", onclick = sprintf("Shiny.setInputValue('nav_to','%s',{priority:'event'});return false;", to), label)
  }

  ov_data <- reactive({
    sit <- tryCatch(situation_latest_clean(), error = function(e) NULL)
    list(sit = sit, stmt = { r <- recent_rv(); if (nrow(r) > 0L) as.list(r[1L]) else NULL })
  })

  output$ov_headline <- renderUI({
    d <- ov_data()
    if (is.null(d$stmt) || is.null(d$sit)) {
      return(tags$div(class = "alert alert-warning", "The latest statement could not be read. Check the pipeline status above."))
    }
    worst <- max(d$sit$fgs_worst_rank, 0L, na.rm = TRUE)
    colour <- if (worst == 0L) APP_PALETTE$bright else RANK_HEX[worst + 1L]
    tags$div(
      class = "ov-headline", style = paste0("border-left-color:", colour, ";"),
      tags$div(class = "ov-kicker",
               sprintf("Flood Guidance Statement #%s, issued %s", d$stmt$statement_id, fmt_statement_time(d$stmt$issued_at, "%Y-%m-%d %H:%M"))),
      tags$div(class = "ov-text", field_or_dash(d$stmt$headline)),
      tags$div(class = "mt-2", "Highest forecast risk in any constituency: ", rank_chip(worst))
    )
  })

  output$ov_kpis <- renderUI({
    d <- ov_data()$sit
    if (is.null(d)) return(NULL)
    b <- briefing_effective()
    n_draft <- if (is.null(b)) 0L else sum(b$status == "draft")
    n_done  <- if (is.null(b)) 0L else sum(b$status != "draft")
    card <- function(n, label, accent = FALSE, sub = NULL) {
      tags$div(class = "stat-card",
               tags$div(class = paste("stat-num", if (accent) "accent"), n),
               tags$div(class = "stat-label", label), if (!is.null(sub)) tags$div(class = "demo-note", sub))
    }
    tags$div(class = "ov-grid",
      card(sum(d$fgs_worst_rank >= 3L, na.rm = TRUE), "Constituencies at Medium or above", TRUE,
           sprintf("of which High: %d", sum(d$fgs_worst_rank >= 4L, na.rm = TRUE))),
      card(sum(d$fgs_escalated, na.rm = TRUE), "Escalated since the last statement", sub = "any rise, on any day or source"),
      card(sum(d$has_active_warning, na.rm = TRUE), "Constituencies with a warning or alert in force"),
      card(n_draft, "MP briefings awaiting review", sub = if (DEMO_MODE && n_done > 0L) sprintf("%d actioned in this demo", n_done)))
  })

  output$ov_outlook <- renderUI({
    d <- ov_data()
    if (is.null(d$sit)) return(NULL)
    base <- tryCatch(as.Date(substr(d$sit$fgs_issued_at[1L], 1L, 10L)), error = function(e) NA)
    cells <- lapply(1:5, function(k) {
      r <- d$sit[[paste0("fgs_rank_day", k)]]
      worst <- max(r, 0L, na.rm = TRUE)
      day_label <- paste0("Day ", k, if (!is.na(base)) paste0(": ", format(base + k - 1L, "%a %d %b")) else "")
      tags$div(class = "ov-day",
        tags$div(class = "d", day_label),
        tags$div(rank_chip(worst)),
        tags$div(class = "n", sprintf("%d at Medium or above", sum(r >= 3L, na.rm = TRUE))))
    })
    tags$div(class = "ov-outlook", cells)
  })

  output$ov_watch <- renderUI({
    d <- ov_data()$sit
    if (is.null(d)) return(NULL)
    top <- d[fgs_worst_rank >= 2L][order(-fgs_worst_rank, -severe_warnings, -warnings, -fgs_escalated, constituency_name)]
    n_all <- nrow(top); top <- head(top, 8L)
    if (nrow(top) == 0L) return(tags$p(class = "text-muted", "No constituency is forecast at Low risk or above."))
    rows <- lapply(seq_len(nrow(top)), function(i) {
      r <- top[i]
      flags <- c(if (isTRUE(r$fgs_escalated)) "Escalated",
                 if (r$severe_warnings > 0L) sprintf("%d severe warning%s", r$severe_warnings, if (r$severe_warnings > 1L) "s" else ""),
                 if (r$warnings > 0L) sprintf("%d warning%s", r$warnings, if (r$warnings > 1L) "s" else ""),
                 if (r$alerts > 0L) sprintf("%d alert%s", r$alerts, if (r$alerts > 1L) "s" else ""))
      tags$tr(class = "clickable", onclick = goto_js(r$constituency_id),
        tags$td(tags$strong(r$constituency_name), tags$br(),
                tags$span(class = "demo-note", if (is.na(r$mp_name)) "Seat vacant" else r$mp_name)),
        tags$td(rank_chip(r$fgs_worst_rank), tags$br(),
                tags$span(class = "demo-note", format(as.Date(r$fgs_worst_date), "%a %d %b"))),
        tags$td(if (length(flags)) paste(flags, collapse = "; ") else tags$span(class = "demo-note", "None")))
    })
    tagList(
      tags$table(class = "ov-table",
                 tags$thead(tags$tr(tags$th("Constituency"), tags$th("Worst risk"), tags$th("In force or changed"))),
                 tags$tbody(rows)),
      tags$div(class = "ov-foot", sprintf("Showing %d of %d at Low or above. Click a row to see it on the map. ", nrow(top), n_all),
               nav_link("Open the full table", "Constituency situation"))
    )
  })

  output$ov_changes <- renderUI({
    ch <- changes()
    if (is.null(ch)) {
      return(tags$p(class = "text-muted",
        if (has_gap("fgs_constituency_changes")) gap_text("fgs_constituency_changes", "Change tracking")
        else "Changes against the previous statement could not be loaded."))
    }
    per <- summarise_changes(ch)
    headline <- headline_counts_message(changes_log(), nrow(per))
    if (nrow(per) == 0L) {
      return(tagList(
        tags$p(class = "text-muted", no_change_message(recent_rv(), selected_statement()$statement_id, changes_log())),
        if (!is.null(headline)) tags$p(class = "text-muted", headline)))
    }
    n_up <- sum(per$up); n_down <- sum(!per$up)
    show <- rbind(head(per[up == TRUE], 5L), head(per[up == FALSE], 3L))
    rows <- lapply(seq_len(nrow(show)), function(i) {
      r <- show[i]
      tags$tr(class = "clickable", onclick = goto_js(r$constituency_id),
        tags$td(r$constituency_name),
        tags$td(rank_chip(r$previous_rank), HTML(" &rarr; "), rank_chip(r$new_rank)),
        tags$td(class = "demo-note", format(as.Date(r$forecast_date), "%a %d")))
    })
    tagList(
      tags$p(tags$strong(n_up), " up, ", tags$strong(n_down), " down (constituencies)."),
      tags$table(class = "ov-table", tags$tbody(rows)),
      if (!is.null(headline)) tags$p(class = "text-muted mt-2 mb-0", headline),
      tags$div(class = "ov-foot", "Rises first. ", nav_link("Open the map", "FGS Interrogator"))
    )
  })

  output$ov_foot <- renderUI({
    tags$div(class = "ov-foot mt-3",
             "Forecast and warnings are separate measures and are not combined. ",
             "Counts use the pipeline's latest complete statement, whichever statement is picked on the map. ",
             nav_link("How each figure is calculated", "Guide"), ".")
  })

  # The Guide (guide.R): how each product is calculated. The text is static; the
  # worked example uses the latest situation table, and a card notes any table
  # the pipeline has not built yet.
  output$guide_body <- renderUI({
    sit <- tryCatch(situation_latest_clean(), error = function(e) NULL)
    guide_body_ui(guide_settings(), gaps = data_gaps(), sit = sit)
  })


  # ---------------------------------------------------------------------------
  # Auto-refresh.
  #
  # The pipeline publishes every 30 to 60 minutes. Every
  # FGS_AUTO_REFRESH_SECONDS this runs one cheap query (a signature of the
  # newest statement and the latest health check) and reloads only when it
  # has changed, so an idle tab costs almost nothing and a long-open tab
  # does not go stale. Set the variable to 0 to turn it off.
  # ---------------------------------------------------------------------------

  last_signature <- reactiveVal(NULL)

  if (AUTO_REFRESH_SECONDS > 0) {
    observe({
      invalidateLater(AUTO_REFRESH_SECONDS * 1000, session)
      sig <- safe_fetch(fetch_data_signature)
      if (is.null(sig) || is.na(sig)) return()
      old <- isolate(last_signature())
      last_signature(sig)
      if (!is.null(old) && !identical(old, sig)) {
        cache_clear()
        reset_gaps()
        load_statements(keep_selected = TRUE)
        load_products()
        data_version(isolate(data_version()) + 1L)
        showNotification("New pipeline data loaded.", type = "message", duration = 6, id = "new_data")
      }
    })
  }


  # ---------------------------------------------------------------------------
  # Shareable views.
  #
  # The URL follows the current tab, statement, day and filters, so "copy the
  # address bar" (or the header button) shares exactly this view. Inputs that
  # are actions or selections, not view state, are excluded.
  # ---------------------------------------------------------------------------

  setBookmarkExclude(c(
    "refresh_data", "show_health", "goto_constituency", "exp_pick", "nav_to",
    "rv_reviewed", "rv_sent", "rv_rejected", "demo_reset",
    "area_row_click", "download_ea_areas", "download_situation",
    "situation_table_rows_selected", "situation_table_row_last_clicked",
    "situation_table_cell_clicked", "situation_table_rows_current",
    "situation_table_rows_all", "situation_table_state", "situation_table_search",
    "live_map_bounds", "live_map_center", "live_map_zoom", "live_map_click",
    "live_map_shape_click", "live_map_shape_mouseover", "live_map_shape_mouseout",
    "live_map_groups", "live_map_marker_click"
  ))
  view_state <- debounce(reactive(reactiveValuesToList(input)), 600)
  observeEvent(view_state(), session$doBookmark(), ignoreInit = TRUE)
  onBookmarked(function(url) updateQueryString(url, mode = "replace"))


  # ---------------------------------------------------------------------------
  # Stale-data banner.
  #
  # Shown above every tab when the pipeline is failing, needs attention, has
  # stopped updating its own health table, or cannot be read. The decision
  # lives in stale_status() (helpers.R). Re-evaluated every minute so the
  # "stopped updating" case appears without anyone clicking anything.
  # ---------------------------------------------------------------------------

  output$stale_banner <- renderUI({
    invalidateLater(60000, session)
    st <- stale_status(health_rv(), stale_hours = STALE_HOURS)
    if (st$level == "ok") return(NULL)
    tags$div(
      class = paste("stale-banner", if (st$level == "warn") "stale-warn" else "stale-bad"),
      role  = "alert",
      tags$span(st$message),
      tags$button(type = "button", class = "btn btn-link",
                  onclick = "Shiny.setInputValue('show_health', Math.random());", "Details")
    )
  })


  # ---------------------------------------------------------------------------
  # Pipeline health badge (job 22).
  #
  # Reads the `overall` row. Click opens the full list of checks. "fail" or
  # "warn" means the data behind every view may be stale, so the badge sits
  # in the header on every tab.
  # ---------------------------------------------------------------------------

  output$health_badge <- renderUI({
    h <- health_rv()
    status <- "unknown"
    text   <- "Pipeline status unavailable"
    if (!is.null(h) && nrow(h) > 0L) {
      overall <- h[check_name == "overall"]
      checks  <- h[check_name != "overall"]
      worst <- if (nrow(overall) > 0L) overall$status[1L] else
        c("fail", "warn", "info", "ok")[which(c("fail", "warn", "info", "ok") %in% checks$status)[1L]]
      if (!is.na(worst)) {
        status <- if (worst == "info") "ok" else worst
        n_bad  <- sum(checks$status %in% c("warn", "fail"))
        text <- switch(status,
          ok   = "Pipeline healthy",
          warn = sprintf("Pipeline: %d check%s attention", n_bad, if (n_bad == 1L) " needs" else "s need"),
          fail = sprintf("Pipeline: %d check%s failing", n_bad, if (n_bad == 1L) "" else "s"),
          "Pipeline status unavailable")
      }
    }
    tags$button(
      type = "button",
      class = paste("health-badge", paste0("health-", status)),
      onclick = "Shiny.setInputValue('show_health', Math.random());",
      tags$span(class = "dot"), text
    )
  })

  observeEvent(input$show_health, {
    h <- health_rv()
    body <- if (is.null(h) || nrow(h) == 0L) {
      tags$p("No health information could be read from ", tags$code("pipeline_health"), ".")
    } else {
      h <- h[check_name != "overall"]
      h <- h[order(match(status, c("fail", "warn", "info", "ok")), check_name)]
      tags$table(
        class = "table table-sm",
        tags$thead(tags$tr(tags$th("Check"), tags$th("Status"), tags$th("Value"), tags$th("Detail"))),
        tags$tbody(lapply(seq_len(nrow(h)), function(i) {
          tags$tr(tags$td(h$check_name[i]),
                  tags$td(tags$span(class = paste("health-badge", paste0("health-",
                          if (h$status[i] == "info") "ok" else h$status[i])),
                          tags$span(class = "dot"), h$status[i])),
                  tags$td(h$value[i]), tags$td(h$detail[i]))
        }))
      )
    }
    showModal(modalDialog(title = "Pipeline health", body, size = "l", easyClose = TRUE,
                          footer = modalButton("Close")))
  })


  # ---------------------------------------------------------------------------
  # Changes since the previous statement (job 18).
  #
  # Follows the statement picker. The pipeline only compares days both
  # statements cover and stores differences only, so an absent row means
  # "no change", and a statement with no predecessor has no rows at all.
  # ---------------------------------------------------------------------------

  changes <- reactive({
    stmt <- selected_statement()
    req(stmt)
    x <- safe_fetch(fetch_changes, stmt$statement_id)
    if (is.null(x) || !("constituency_id" %in% names(x))) return(x)
    x[in_nations(constituency_id, nation_codes())]
  })

  # Did job 18 really compare this statement? NULL when its log table is not
  # there (older pipeline), which falls back to trusting an empty result. Never
  # a data gap: the panel still works without it.
  changes_log <- reactive({
    stmt <- selected_statement()
    req(stmt)
    tryCatch(fetch_changes_log(stmt$statement_id), error = function(e) NULL)
  })

  # Shared helpers for the views below.
  rank_chip <- function(r) {
    r <- suppressWarnings(as.integer(r)); r[is.na(r) | r < 0L | r > 4L] <- 0L
    tags$span(class = "rank-chip",
              style = sprintf("background:%s;color:%s;", RANK_HEX[r + 1L],
                              if (r %in% c(1L, 4L)) "#ffffff" else "#212529"),
              RANK_LABELS[r + 1L])
  }

  # Safe inline handler: jump to a constituency on the map. The id is reduced
  # to safe characters because it lands inside an HTML attribute.
  goto_js <- function(cid) {
    sprintf("Shiny.setInputValue('goto_constituency','%s',{priority:'event'});",
            gsub("[^A-Za-z0-9_]", "", as.character(cid)))
  }

  # ---------------------------------------------------------------------------
  # "Data as of" in the header: when the pipeline last checked itself and
  # which statement the constituency views are built on.
  # ---------------------------------------------------------------------------

  output$data_asof <- renderUI({
    h <- health_rv(); sit <- situation_rv()
    bits <- character()
    if (!is.null(h) && nrow(h) > 0L && "overall" %in% h$check_name) {
      bits <- c(bits, paste0("Pipeline checked ", substr(short_ts(h[check_name == "overall"]$checked_at[1L]), 12L, 16L), " UTC"))
    }
    if (!is.null(sit) && nrow(sit) > 0L) {
      bits <- c(bits, paste0("Latest statement #", sit$fgs_statement_id[1L], ", ",
                             fmt_statement_time(sit$fgs_issued_at[1L], "%Y-%m-%d %H:%M")))
    }
    if (length(bits) == 0L) return(NULL)
    tags$span(class = "asof", paste(bits, collapse = " | "))
  })


  # ---------------------------------------------------------------------------
  # Changes since the previous statement, one row per constituency.
  #
  # The pipeline stores one row per constituency, day and source. People talk
  # about a constituency ("Carlisle went from Medium to High on Friday"), so
  # each is reduced to its highest new risk, with a count of other changes.
  # Collapsed by default and drawn over the map when opened, so it never
  # pushes the map down. A row jumps to the constituency on the map.
  # ---------------------------------------------------------------------------

  output$changes_strip <- renderUI({
    ch <- changes()
    if (is.null(ch)) {
      # No data, not "no changes": say which, instead of showing nothing.
      return(tags$div(class = "changes-wrap",
        tags$div(class = "changes-strip text-muted",
          if (has_gap("fgs_constituency_changes")) gap_text("fgs_constituency_changes", "Change tracking")
          else "Changes against the previous statement could not be loaded.")))
    }
    if (nrow(ch) == 0L) {
      return(tags$div(class = "changes-wrap",
        tags$div(class = "changes-strip text-muted",
          no_change_message(recent_rv(), selected_statement()$statement_id, changes_log()),
          { hl <- headline_counts_message(changes_log(), 0L); if (!is.null(hl)) tags$div(class = "mt-1", hl) })))
    }
    per <- summarise_changes(ch)
    n_up <- sum(per$up); n_down <- sum(!per$up)

    row_html <- function(r) {
      tags$tr(
        class = "chg-row", onclick = goto_js(r$constituency_id),
        tags$td(r$constituency_name),
        tags$td(rank_chip(r$previous_rank), HTML(" &rarr; "), rank_chip(r$new_rank)),
        tags$td(format(as.Date(r$forecast_date), "%a %d %b"), " ", tags$span(class = "text-muted", r$source)),
        tags$td(if (r$n_cells > 1L) tags$span(class = "chg-more", sprintf("+%d more", r$n_cells - 1L)))
      )
    }
    group_rows <- function(d, title) {
      if (nrow(d) == 0L) return(NULL)
      c(list(tags$tr(class = "chg-group", tags$td(colspan = 4, title))),
        lapply(seq_len(nrow(d)), function(i) row_html(d[i])))
    }

    tags$div(class = "changes-wrap",
      tags$details(
        class = "changes-strip",
        tags$summary(
          tags$span("Since the previous statement: "),
          tags$span(class = "chg-count chg-up",   sprintf("%d up", n_up)),
          tags$span(HTML(" &middot; ")),
          tags$span(class = "chg-count chg-down", sprintf("%d down", n_down)),
          tags$span(class = "text-muted", " (constituencies)")
        ),
        tags$div(class = "chg-scroll",
          tags$table(tags$tbody(group_rows(per[up == TRUE], "Risk up"),
                                group_rows(per[up == FALSE], "Risk down"))),
          tags$div(class = "chg-hint", "Click a row to see the constituency on the map.")
        )
      )
    )
  })


  # ---------------------------------------------------------------------------
  # Constituency forecast risk layer.
  #
  # Colours every constituency with a forecast risk by its worst rank over the
  # five days, from constituency_flood_situation. It follows the pipeline's
  # latest complete statement, not the statement or day pickers, and the
  # legend says so. Separate from the "Constituencies" outline layer above,
  # which follows the pickers and the matrix filter.
  # ---------------------------------------------------------------------------

  observe(with_layer_guard("constituency forecast risk", function(x) invisible(NULL), notify = layer_notify, expr = {
    proxy <- leaflet::leafletProxy("live_map")
    proxy <- leaflet::clearGroup(proxy, "const_risk")
    proxy <- leaflet::removeControl(proxy, "const_risk_legend")
    if (!isTRUE(input$layer_const_risk)) return()

    shape <- safe_fetch(fetch_constituency_risk_geometry)
    if (is.null(shape)) {
      showNotification("Constituency risk could not be loaded.", type = "warning", id = "const_risk_failed")
      return()
    }
    shape <- shape[in_nations(shape$constituency_id, nation_codes()), ]
    if (nrow(shape) == 0L) {
      showNotification("No constituency has a forecast risk in the latest statement.", type = "message", id = "const_risk_none")
      return()
    }

    proxy <- leaflet::addPolygons(
      proxy,
      data        = shape,
      group       = "const_risk",
      color       = "#495057",
      weight      = 1,
      fillColor   = rank_fill(shape$fgs_worst_rank),
      fillOpacity = 0.55,
      label       = shape$constituency_name,
      popup       = risk_layer_popup(shape),
      highlightOptions = leaflet::highlightOptions(weight = 2, fillOpacity = 0.7, bringToFront = FALSE)
    )
    leaflet::addLegend(
      proxy, position = "bottomright",
      colors  = RANK_HEX[2:5], labels = RANK_LABELS[2:5],
      title   = "Forecast risk by constituency<br>(worst of 5 days, latest statement)",
      layerId = "const_risk_legend", opacity = 0.85
    )
  }))


  # ---------------------------------------------------------------------------
  # Jump to a constituency on the map (from the changes strip, the situation
  # table or the exposure history). Switches to the Interrogator, draws the
  # boundary and flies to it. Messages reach the browser in order, so the tab
  # is showing before the map moves.
  # ---------------------------------------------------------------------------

  observeEvent(input$goto_constituency, {
    removeModal()
    shape <- safe_fetch(fetch_constituency_shape, input$goto_constituency)
    if (is.null(shape) || nrow(shape) == 0L) {
      showNotification("That constituency's boundary could not be loaded.", type = "warning", id = "goto_failed")
      return()
    }
    updateTabsetPanel(session, "main_tabs", selected = "FGS Interrogator")
    bbox  <- sf::st_bbox(shape)
    proxy <- leaflet::leafletProxy("live_map")
    proxy <- leaflet::clearGroup(proxy, "goto")
    proxy <- leaflet::addPolygons(proxy, data = shape, group = "goto",
                                  color = APP_PALETTE$blue, weight = 3, fill = FALSE,
                                  label = shape$constituency_name[1L])
    leaflet::flyToBounds(proxy,
                         lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
                         lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]])
  })


  # ---------------------------------------------------------------------------
  # Constituency situation tab (jobs 19 and 20).
  # ---------------------------------------------------------------------------

  clean_situation <- function(d) {
    req(d)
    d <- copy(d)
    d[, `:=`(fgs_worst_rank     = as.integer(fgs_worst_rank),
             fgs_escalated      = as.logical(fgs_escalated),
             has_active_warning = as.logical(has_active_warning))]
    d[in_nations(constituency_id, nation_codes())]
  }

  # The Overview always shows the latest statement.
  situation_latest_clean <- reactive(clean_situation(situation_rv()))

  # POC-ONLY: statement look-back ------------------------------------------------
  # The Constituency situation tab can show an earlier statement, rebuilt from the
  # stored intersections. poc_past_id() is NULL for the live (newest) table.
  # Remove with the other POC-ONLY blocks when the app goes live.
  poc_past_id <- reactive({
    if (!POC_FEATURES) return(NULL)
    id <- input$sit_statement
    r  <- recent_rv()
    if (is.null(id) || identical(id, "latest") || nrow(r) == 0L) return(NULL)
    if (!(id %in% as.character(r$statement_id)) || as.integer(id) == r$statement_id[1L]) return(NULL)
    as.integer(id)
  })

  if (POC_FEATURES) {                                          # POC-ONLY
    observe({
      r <- recent_rv()
      req(nrow(r) > 0L)
      older <- r[-1L]
      choices <- c(list("Latest (live table)" = "latest"), group_statement_choices(older))
      cur <- isolate(input$sit_statement)
      updateSelectInput(session, "sit_statement", choices = choices,
                        selected = if (!is.null(cur) && cur %in% unlist(choices)) cur else "latest")
    })
  }

  situation_view <- reactive({
    id <- poc_past_id()
    if (is.null(id)) return(situation_rv())
    r <- recent_rv()
    safe_fetch(fetch_situation_for_statement, id, as.character(r$issued_at[r$statement_id == id][1L]))
  })

  briefing_view <- reactive({
    id <- poc_past_id()
    if (is.null(id)) return(briefing_effective())
    b <- safe_fetch(fetch_briefing_drafts, id)
    if (is.null(b)) b else b[in_nations(constituency_id, nation_codes())]
  })

  # Potential warnings and alerts: flood warning and alert areas under the
  # statement's risk polygons, per constituency (fetch_potential_areas). NULL
  # when they cannot be read, which shows as n/a and never blocks the table.
  potential_areas <- reactive({
    sid <- poc_past_id()
    if (is.null(sid)) {
      s <- situation_rv()
      if (is.null(s) || nrow(s) == 0L) return(NULL)
      sid <- as.integer(s$fgs_statement_id[1L])
    }
    mr <- suppressWarnings(as.integer(input$sit_min_rank)); if (length(mr) != 1L || is.na(mr)) mr <- 0L
    srcs <- input$sit_sources
    safe_fetch(fetch_potential_areas, sid, mr, srcs)
  })

  # The statement's constituency intersections, which give the source filter and the
  # impact and likelihood columns. NULL when they cannot be read: the columns then show
  # n/a and the source filter is ignored (the table says so), and nothing else breaks.
  situation_inter <- reactive({
    sid <- poc_past_id()
    if (is.null(sid)) {
      s <- situation_rv()
      if (is.null(s) || nrow(s) == 0L) return(NULL)
      sid <- as.integer(s$fgs_statement_id[1L])
    }
    safe_fetch(fetch_constituency_intersections, sid)
  })

  situation_clean <- reactive({
    d <- clean_situation(situation_view())
    p <- potential_areas()
    if (is.null(p)) {
      d[, `:=`(pot_warning_areas = NA_integer_, pot_alert_areas = NA_integer_)]
    } else if (nrow(p) == 0L) {                                # none under any polygon
      d[, `:=`(pot_warning_areas = 0L, pot_alert_areas = 0L)]
    } else {
      m <- match(d$constituency_id, p$constituency_id)
      d[, `:=`(pot_warning_areas = data.table::fcoalesce(as.integer(p$warning_areas[m]), 0L),
               pot_alert_areas   = data.table::fcoalesce(as.integer(p$alert_areas[m]), 0L))]
    }
    srcs  <- input$sit_sources
    inter <- situation_inter()
    detail <- if (is.null(inter)) NULL else situation_detail(inter, srcs, CONTACT_MIN_PCT)
    apply_situation_detail(d, detail, narrow = length(srcs) > 0L && !is.null(detail))
  })

  situation_filtered <- reactive({
    d <- situation_clean()
    keep <- d$fgs_worst_rank >= as.integer(input$sit_min_rank)
    # With "Any" risk every constituency is listed, quiet ones too. Choosing a flood source means
    # "those with risk from it", so constituencies with nothing under those sources drop out.
    if (length(input$sit_sources) > 0L && !is.null(situation_inter())) keep <- keep & d$fgs_worst_rank >= 1L
    mi <- suppressWarnings(as.integer(input$sit_min_impact)); if (length(mi) != 1L || is.na(mi)) mi <- 0L
    if (mi > 0L) keep <- keep & !is.na(d$fgs_worst_impact_x) & d$fgs_worst_impact_x >= mi
    if (isTRUE(input$sit_escalated)) keep <- keep & d$fgs_escalated
    if (isTRUE(input$sit_warning))   keep <- keep & d$has_active_warning
    q <- trimws(if (is.null(input$sit_search)) "" else input$sit_search)
    if (nzchar(q)) {
      # fixed = TRUE ignores ignore.case, so compare lower-cased text instead.
      q <- tolower(q)
      keep <- keep & (grepl(q, tolower(d$constituency_name), fixed = TRUE) |
                      grepl(q, tolower(fifelse(is.na(d$mp_name), "", d$mp_name)), fixed = TRUE))
    }
    d[keep][order(-fgs_worst_rank, -fgs_worst_impact_x, -severe_warnings, -warnings, constituency_name)]
  })

  # One line saying what the source filter did, or that it could not be applied.
  source_note <- reactive({
    srcs <- input$sit_sources
    if (length(srcs) == 0L) return(NULL)
    names_ <- c(river = "River", coastal = "Coastal", surface = "Surface water", ground = "Groundwater")
    if (is.null(situation_inter())) {
      return(tags$div(class = "text-warning mb-2",
        "The source filter could not be applied: fgs_constituency_intersections could not be read."))
    }
    tags$div(class = "text-muted mb-2",
      sprintf("Showing %s only: the risks, days, impact and likelihood below, and the warning and alert areas under the forecast, are for those sources. ",
              paste(names_[srcs], collapse = ", ")),
      "Escalated, and warnings and alerts in force, are across all sources.")
  })

  output$situation_note <- renderUI({
    tagList(situation_note_main(), source_note())
  })

  situation_note_main <- reactive({
    id <- poc_past_id()
    d  <- if (is.null(id)) situation_rv() else situation_view()
    if (is.null(d) || nrow(d) == 0L) {
      return(tags$div(class = "alert alert-warning",
        if (is.null(id)) "constituency_flood_situation could not be read. Check the pipeline health badge."
        else sprintf("Statement #%s could not be rebuilt. Check the pipeline health badge.", id)))
    }
    if (!is.null(id)) {                                        # POC-ONLY
      return(tags$div(class = "alert alert-secondary mb-2",
        tags$strong("Look-back (proof of concept). "),
        sprintf("Statement #%s, issued %s. ", d$fgs_statement_id[1L], fmt_statement_time(d$fgs_issued_at[1L], "%Y-%m-%d %H:%M")),
        "Risks, escalation and the warning and alert areas under the forecast are rebuilt from the stored ",
        "intersections. Warnings and alerts in force are not kept for past statements, so they show n/a. MP details are today's."))
    }
    tags$div(class = "text-muted mb-2",
      sprintf("Latest complete statement: id %s, issued %s. ",
              d$fgs_statement_id[1L], fmt_statement_time(d$fgs_issued_at[1L], "%Y-%m-%d %H:%M")),
      "Forecast and warnings are separate measures and are not combined. ",
      "Warning and alert areas under the forecast are potential: places where one could be issued. ",
      if (isTRUE(suppressWarnings(as.integer(input$sit_min_rank)) > 0L))
        sprintf("They are counted under polygons at %s risk or above, following the minimum forecast risk filter.", RANK_LABELS[as.integer(input$sit_min_rank) + 1L]))
  })

  output$situation_kpis <- renderUI({
    d <- tryCatch(situation_clean(), error = function(e) NULL)
    if (is.null(d)) return(NULL)
    card <- function(n, label, accent = FALSE) {
      tags$div(class = "stat-card",
               tags$div(class = paste("stat-num", if (accent) "accent"), n),
               tags$div(class = "stat-label", label))
    }
    tags$div(class = "kpi-row",
      card(sum(d$fgs_worst_rank >= 3L, na.rm = TRUE), "Forecast Medium or above", TRUE),
      card(if (all(is.na(d$fgs_escalated))) "n/a" else sum(d$fgs_escalated, na.rm = TRUE), "Escalated since last statement"),
      card(if (is.null(poc_past_id())) sum(d$has_active_warning, na.rm = TRUE) else "n/a", "Warning or alert in force"))
  })

  output$rank_legend <- renderUI({
    tags$div(class = "legend-row", tags$span("Forecast risk:"),
             lapply(0:4, rank_chip),
             tags$span("The five day columns are forecast days, counted from the statement date."))
  })

  output$situation_table <- DT::renderDataTable({
    d <- situation_filtered()
    base_date <- tryCatch(as.Date(substr(situation_clean()$fgs_issued_at[1L], 1L, 10L)), error = function(e) NA)
    day_names <- if (is.na(base_date)) paste0("D", 1:5) else format(base_date + 0:4, "%a %d")
    tbl <- d[, .(
      Constituency = constituency_name,
      MP           = fifelse(is.na(mp_name), "(vacant or unmatched)", mp_name),
      Party        = mp_party,
      Worst        = fgs_worst_rank,
      On           = format(as.Date(fgs_worst_date), "%a %d %b"),
      Sources      = fgs_worst_sources,
      Impact       = fifelse(is.na(fgs_worst_impact),     "n/a", fgs_worst_impact),
      Likelihood   = fifelse(is.na(fgs_worst_likelihood), "n/a", fgs_worst_likelihood),
      d1 = fgs_rank_day1, d2 = fgs_rank_day2, d3 = fgs_rank_day3,
      d4 = fgs_rank_day4, d5 = fgs_rank_day5,
      Escalated    = fifelse(is.na(fgs_escalated), "n/a", fifelse(as.logical(fgs_escalated), "Yes", "")),
      Severe       = severe_warnings,
      Warnings     = warnings,
      Alerts       = alerts,
      WarnAreas    = fifelse(is.na(pot_warning_areas), "n/a", as.character(pot_warning_areas)),
      AlertAreas   = fifelse(is.na(pot_alert_areas),   "n/a", as.character(pot_alert_areas))
    )]
    setnames(tbl, c("d1", "d2", "d3", "d4", "d5"), day_names)
    setnames(tbl, c("Impact", "Likelihood"), c("Impact (worst day)", "Likelihood (worst day)"))
    setnames(tbl, c("Severe", "Warnings", "Alerts", "WarnAreas", "AlertAreas"),
             c("Severe in force", "Warnings in force", "Alerts in force", "Warning areas under forecast", "Alert areas under forecast"))
    if (!is.null(poc_past_id())) {                             # POC-ONLY: in force is not held for past statements
      for (cn in c("Severe in force", "Warnings in force", "Alerts in force")) tbl[, (cn) := rep("n/a", .N)]
    }
    rank_cols <- c("Worst", day_names)
    labels_js <- jsonlite::toJSON(RANK_LABELS, auto_unbox = FALSE)
    render_js <- DT::JS(sprintf(
      "function(data, type) { var L = %s; return type === 'display' ? L[data] || L[0] : data; }",
      labels_js))
    targets <- match(rank_cols, names(tbl)) - 1L   # columnDefs targets are 0-based

    DT::datatable(
      tbl, rownames = FALSE, selection = "single",
      options = list(pageLength = 25, scrollX = TRUE, dom = "tip", ordering = TRUE,
                     language = list(info = "Showing _START_ to _END_ of _TOTAL_ constituencies",
                                     emptyTable = empty_situation_message(
                                       situation_clean()$fgs_worst_rank, as.integer(input$sit_min_rank),
                                       other_filters = isTRUE(input$sit_escalated) || isTRUE(input$sit_warning) ||
                                                       length(input$sit_sources) > 0L || !identical(input$sit_min_impact, "0") ||
                                                       nzchar(trimws(if (is.null(input$sit_search)) "" else input$sit_search)))),
                     columnDefs = list(list(targets = targets, render = render_js)))
    ) |>
      DT::formatStyle(rank_cols,
                      backgroundColor = DT::styleEqual(0:4, RANK_HEX),
                      color           = DT::styleEqual(0:4, c("#212529", "#ffffff", "#212529", "#212529", "#ffffff")))
  })

  # The export has the same columns, in the same order and with the same names, as the table,
  # with the risk as words instead of a coloured cell, and the MP's email and phone added.
  output$download_situation <- downloadHandler(
    filename = function() sprintf("constituency_situation_%s.csv", format(Sys.time(), "%Y%m%d_%H%M")),
    content  = function(file) {
      d <- situation_filtered()
      base_date <- tryCatch(as.Date(substr(situation_clean()$fgs_issued_at[1L], 1L, 10L)), error = function(e) NA)
      day_names <- if (is.na(base_date)) paste0("D", 1:5) else format(base_date + 0:4, "%a %d")
      level <- function(r) RANK_LABELS[pmin(pmax(as.integer(r), 0L, na.rm = TRUE), 4L) + 1L]
      out <- d[, .(
        Constituency = constituency_name,
        MP           = fifelse(is.na(mp_name), "(vacant or unmatched)", mp_name),
        Party        = mp_party,
        `MP email`   = mp_email,
        `MP phone`   = mp_phone,
        Worst        = level(fgs_worst_rank),
        On           = format(as.Date(fgs_worst_date), "%a %d %b"),
        Sources      = fgs_worst_sources,
        `Impact (worst day)`     = fifelse(is.na(fgs_worst_impact),     "n/a", fgs_worst_impact),
        `Likelihood (worst day)` = fifelse(is.na(fgs_worst_likelihood), "n/a", fgs_worst_likelihood),
        d1 = level(fgs_rank_day1), d2 = level(fgs_rank_day2), d3 = level(fgs_rank_day3),
        d4 = level(fgs_rank_day4), d5 = level(fgs_rank_day5),
        Escalated    = fifelse(is.na(fgs_escalated), "n/a", fifelse(as.logical(fgs_escalated), "Yes", "")),
        `Severe in force`   = severe_warnings,
        `Warnings in force` = warnings,
        `Alerts in force`   = alerts,
        `Warning areas under forecast` = fifelse(is.na(pot_warning_areas), "n/a", as.character(pot_warning_areas)),
        `Alert areas under forecast`   = fifelse(is.na(pot_alert_areas),   "n/a", as.character(pot_alert_areas))
      )]
      setnames(out, c("d1", "d2", "d3", "d4", "d5"), day_names)
      data.table::fwrite(out, file)
    }
  )

  # ---------------------------------------------------------------------------
  # Warning history tab (job 25).
  #
  # Filtering is in R: the history arrives once per load as counts by day of
  # issue, so the slider and the Nations filter cost no further queries.
  # ---------------------------------------------------------------------------

  history_scoped <- reactive({
    h <- history_rv()
    if (is.null(h)) NULL else h[in_nations(constituency_id, nation_codes())]
  })

  observeEvent(history_rv(), {
    h <- history_rv()
    if (is.null(h) || nrow(h) == 0L) return()
    days <- as.Date(h$issued_date)
    updateSliderInput(session, "wh_range", min = min(days), max = max(days), value = c(min(days), max(days)))
  }, ignoreNULL = TRUE)

  history_window <- reactive({
    h <- history_scoped()
    req(h)
    r <- input$wh_range
    if (is.null(r)) h else warning_history_window(h, r[1L], r[2L])
  })

  wh_names <- reactive({
    lk <- scoped_lookup()
    if (!("constituency_id" %in% names(lk))) return(data.table(constituency_id = character(), constituency_name = character(), mp_name = character()))
    unique(lk[, .(constituency_id, constituency_name, mp_name)], by = "constituency_id")
  })

  wh_summary <- reactive({
    counts <- warning_constituency_counts(history_window())
    nm <- wh_names()
    out <- merge(counts, nm, by = "constituency_id", all.x = TRUE, sort = FALSE)
    out[is.na(constituency_name), constituency_name := constituency_id]
    q <- trimws(if (is.null(input$wh_search)) "" else input$wh_search)
    if (nzchar(q)) {
      q <- tolower(q)
      out <- out[grepl(q, tolower(constituency_name), fixed = TRUE) |
                 grepl(q, tolower(fifelse(is.na(mp_name), "", mp_name)), fixed = TRUE)]
    }
    out[order(-total, constituency_name)]
  })

  output$wh_note <- renderUI({
    if (has_gap("ea_warning_episodes")) {
      return(tags$p(class = "text-muted", gap_text("ea_warning_episodes", "Warning history")))
    }
    if (is.null(history_rv())) return(tags$p(class = "text-muted", "Warning history could not be loaded."))
    tags$small(class = "text-muted d-block mb-2",
      "An issue is one alert or warning, from the time it was switched on to the time it ended. A warning that rises ",
      "to severe and falls back is one issue. Warnings includes severe ones, so the total is alerts plus warnings. ",
      "Areas are linked to a constituency when at least ", sprintf("%g%%", 100 * TARGET_AREA_MIN_PCT),
      " of the area lies inside it. Constituencies with no issue in the period are not listed. Select a row for its areas.")
  })

  output$wh_table <- DT::renderDataTable({
    req(history_scoped())
    s <- wh_summary()
    tbl <- s[, .(Constituency = constituency_name,
                 MP = fifelse(is.na(mp_name), "(vacant or unmatched)", mp_name),
                 Alerts = alerts, Warnings = warnings, `of which severe` = severe, `Total issues` = total)]
    DT::datatable(tbl, rownames = FALSE, selection = "single",
                  options = list(pageLength = 25, scrollX = TRUE, dom = "tip",
                                 order = list(list(5L, "desc")),
                                 language = list(info = "Showing _START_ to _END_ of _TOTAL_ constituencies",
                                                 emptyTable = "No alerts or warnings issued in this period.")))
  })

  wh_selected <- reactive({
    row <- input$wh_table_rows_selected
    if (is.null(row)) return(NULL)
    s <- wh_summary()
    if (row > nrow(s)) NULL else s[row]
  })

  wh_area_table <- reactive({
    sel <- wh_selected()
    req(sel)
    warning_area_counts(history_window(), sel$constituency_id)
  })

  output$wh_detail_title <- renderUI({
    sel <- wh_selected()
    if (is.null(sel)) return(tags$p(class = "text-muted", "Select a constituency to see its alert and warning areas."))
    tags$h6(sprintf("%s: %d issue%s (%d alert%s, %d warning%s)", sel$constituency_name,
                    sel$total, if (sel$total == 1L) "" else "s",
                    sel$alerts, if (sel$alerts == 1L) "" else "s",
                    sel$warnings, if (sel$warnings == 1L) "" else "s"))
  })

  wh_area_display <- function(a) {
    a[, .(Area = ea_area_name, Code = ea_area_code,
          Type = fifelse(ea_area_type == "flood_alert", "Flood alert", "Flood warning"),
          Issued = issued, `of which severe` = severe, `Last issued` = format(last_issued, "%d %b %Y"))]
  }

  # Every episode for the selected constituency (the Detailed table and the graph). Fetched on
  # selection, not for every constituency.
  wh_episodes <- reactive({
    sel <- wh_selected()
    req(sel)
    safe_fetch(fetch_warning_episodes, sel$constituency_id)
  })

  wh_episode_table <- reactive({
    r <- input$wh_range
    ep <- wh_episodes()
    if (is.null(r)) r <- c(as.Date("1900-01-01"), Sys.Date())
    warning_episode_rows(ep, r[1L], r[2L])
  })

  # What the second table shows: the areas with their counts, or, when Detailed is ticked, one row per episode.
  wh_second_table <- reactive({
    if (isTRUE(input$wh_detailed)) wh_episode_table() else wh_area_display(wh_area_table())
  })

  output$wh_areas <- DT::renderDataTable({
    req(wh_selected())
    detailed <- isTRUE(input$wh_detailed)
    tbl <- wh_second_table()
    if (detailed && nrow(tbl) == 0L) {
      return(DT::datatable(data.frame(Note = if (is.null(wh_episodes())) "The episodes could not be loaded." else "No alerts or warnings were issued in this period."),
                           rownames = FALSE, selection = "none", options = list(dom = "t")))
    }
    DT::datatable(tbl, rownames = FALSE, selection = "none",
                  options = list(pageLength = 15, scrollX = TRUE, dom = "tip",
                                 order = if (detailed) list() else list(list(3L, "desc"))))
  })

  output$wh_concurrent_note <- renderUI({
    if (is.null(wh_selected())) return(NULL)
    tags$div(
      tags$h6("Alerts and warnings in force at the same time"),
      tags$small(class = "text-muted d-block mb-2",
        "Each step up is an alert or warning switched on in an area linked to this constituency; each step down is one ending. ",
        "The line counts areas, so two warnings on different rivers at once read as two. Episodes open for over 30 days are left out, ",
        "because they most likely never received an end message. The period slider sets the span shown."))
  })

  if (requireNamespace("plotly", quietly = TRUE)) {
    output$wh_concurrent <- plotly::renderPlotly({
      sel <- wh_selected()
      req(sel)
      ep <- wh_episodes()
      req(ep, nrow(ep) > 0L)
      now <- as.POSIXct(Sys.time(), tz = "UTC")
      cc <- concurrent_counts(ep, now)
      req(nrow(cc) > 0L)
      r <- input$wh_range
      from <- if (is.null(r)) min(cc$time) else as.POSIXct(paste(r[1L], "00:00:00"), tz = "UTC")
      to   <- if (is.null(r)) now else min(now, as.POSIXct(paste(r[2L], "23:59:59"), tz = "UTC"))
      # Carry the count in force at the start of the span, and close the line at its end.
      before <- cc[time <= from]
      start_row <- if (nrow(before)) before[.N, .(time = from, alerts, warnings)] else data.table(time = from, alerts = 0L, warnings = 0L)
      inside <- cc[time > from & time <= to]
      last <- if (nrow(inside)) inside[.N] else start_row
      cc <- rbind(start_row, inside, data.table(time = to, alerts = last$alerts, warnings = last$warnings))
      fig <- plotly::plot_ly(cc, x = ~time)
      fig <- plotly::add_lines(fig, y = ~alerts, name = "Flood alerts", line = list(color = APP_ALERT_HEX, width = 1.5, shape = "hv"),
                               hovertemplate = "%{y} in force<extra>Flood alerts</extra>")
      fig <- plotly::add_lines(fig, y = ~warnings, name = "Flood warnings", line = list(color = APP_WARNING_HEX, width = 1.5, shape = "hv"),
                               hovertemplate = "%{y} in force<extra>Flood warnings</extra>")
      top <- max(cc$alerts, cc$warnings, 1L)
      app_plot_style(fig, height = 380, hovermode = "x unified",
                     xaxis = app_plot_axis(range = format(c(from, to), "%Y-%m-%d %H:%M:%S", tz = "UTC"), type = "date",
                                           rangeslider = list(visible = TRUE, thickness = 0.07, bgcolor = APP_PALETTE$tint,
                                                              bordercolor = APP_PALETTE$line, borderwidth = 1)),
                     yaxis = app_plot_axis("Areas in force", grid = TRUE, rangemode = "tozero", tickformat = "d",
                                           dtick = nice_dtick(top), range = c(0, top * 1.08)),
                     title = list(text = sel$constituency_name, x = 0.01, xanchor = "left",
                                  font = list(size = 13, color = APP_PALETTE$navy)),
                     margin = list(t = 50, l = 60, r = 20))
    })
  }

  output$download_warning_history <- downloadHandler(
    filename = function() sprintf("warning_history_by_constituency_%s.csv", format(Sys.time(), "%Y%m%d_%H%M")),
    content  = function(file) {
      s <- wh_summary()
      data.table::fwrite(s[, .(Constituency = constituency_name,
                               MP = fifelse(is.na(mp_name), "(vacant or unmatched)", mp_name),
                               Alerts = alerts, Warnings = warnings, `of which severe` = severe,
                               `Total issues` = total)], file)
    }
  )

  output$download_wh_areas <- downloadHandler(
    filename = function() sprintf("warning_history_areas_%s.csv", format(Sys.time(), "%Y%m%d_%H%M")),
    content  = function(file) {
      sel <- wh_selected()
      tbl <- if (is.null(sel)) data.table() else wh_second_table()
      out <- if (nrow(tbl) == 0L) data.table() else cbind(Constituency = sel$constituency_name, tbl)
      data.table::fwrite(out, file)
    }
  )

  # ---------------------------------------------------------------------------
  # Hydrometrics tab (port of explore/Explore_live_readings).
  #
  # The measure register loads the first time the tab is opened, not at start-up.
  # The four drop-downs narrow each other; readings are fetched only for the
  # chosen measures and days, and only when those change or Refresh is pressed.
  # ---------------------------------------------------------------------------

  rt_reg <- reactiveVal(NULL)
  rt_started <- reactiveVal(FALSE)
  rt_or_all <- function(x) if (is.null(x) || length(x) == 0L || !nzchar(x[1L])) RT_ALL else x[1L]

  rt_sync <- function(reg, area, river, param, stn) {
    o <- rt_options(reg, area, river, param)
    keep <- function(v, choices) if (v %in% choices) v else RT_ALL
    area  <- keep(area, o$areas);  river <- keep(river, o$rivers)
    param <- keep(param, o$parameters); stn <- keep(stn, o$stations)
    updateSelectInput(session, "rt_area",    choices = o$areas,      selected = area)
    updateSelectInput(session, "rt_river",   choices = o$rivers,     selected = river)
    updateSelectInput(session, "rt_param",   choices = o$parameters, selected = param)
    updateSelectInput(session, "rt_station", choices = o$stations,   selected = stn)
    rows <- rt_filter_register(reg, area, river, param, stn)
    rows <- rows[order(tolower(measure_label))][seq_len(min(.N, 500L))]
    # A single station: preselect up to four of its measures so the chart appears at once.
    pick <- if (!identical(stn, RT_ALL)) head(rows$measure_uri, 4L) else character()
    updateSelectizeInput(session, "rt_measures", choices = stats::setNames(rows$measure_uri, rows$measure_label),
                         selected = pick, server = TRUE)
  }

  observeEvent(input$main_tabs, {
    if (!identical(input$main_tabs, "Hydrometrics") || rt_started()) return()
    rt_started(TRUE)
    r <- safe_fetch(fetch_rt_register)
    if (is.null(r)) return()
    reg <- rt_prepare_register(r)
    rt_reg(reg)
    rt_sync(reg, RT_ALL, RT_ALL, RT_ALL, RT_ALL)
  })

  observeEvent(list(input$rt_area, input$rt_river, input$rt_param, input$rt_station), {
    reg <- rt_reg()
    req(reg)
    rt_sync(reg, rt_or_all(input$rt_area), rt_or_all(input$rt_river), rt_or_all(input$rt_param), rt_or_all(input$rt_station))
  }, ignoreInit = TRUE)

  rt_data <- eventReactive(list(input$rt_measures, input$rt_days, input$rt_refresh, rt_reg()), {
    reg <- rt_reg()
    if (is.null(reg)) return(list(message = if (has_gap("ea_rt_measure_register")) gap_text("ea_rt_measure_register", "Hydrometrics")
                                            else if (rt_started()) "The measure register could not be loaded." else "Loading the measure register."))
    uris <- input$rt_measures
    if (length(uris) == 0L) return(list(message = "Choose one or more measures. Pick a station to start, or narrow by river or area."))
    if (length(uris) > RT_MAX_MEASURES) return(list(message = sprintf("%d measures chosen. Pick %d or fewer so the chart stays readable.", length(uris), RT_MAX_MEASURES)))
    days <- as.integer(input$rt_days)
    if (is.na(days)) days <- 3L
    now <- as.POSIXct(Sys.time(), tz = "UTC")
    if (days == 0L) {                                           # all available: from the earliest reading held
      first <- safe_fetch(fetch_rt_span, uris)
      days <- if (is.null(first)) 30L else max(1L, as.integer(ceiling(as.numeric(difftime(now, first, units = "days")))))
    }
    sum_uris <- reg[measure_uri %in% uris & parameter %in% "rainfall"]$measure_uri
    readings <- safe_fetch(fetch_rt_readings, uris, days, sum_uris)
    if (is.null(readings)) {
      return(list(message = if (has_gap("ea_rt_readings_all")) gap_text("ea_rt_readings_all", "Readings")
                            else "Could not read the readings. Has notebook 23 run? It creates ea_rt_readings_all once the archive exists."))
    }
    if (nrow(readings) == 0L) return(list(message = "No readings for these measures in that window."))
    boundary <- safe_fetch(fetch_rt_boundary)
    list(readings = readings, chosen = reg[measure_uri %in% uris], days = days, boundary = boundary, now = now)
  }, ignoreNULL = FALSE)

  output$rt_message <- renderUI({
    m <- rt_data()$message
    if (is.null(m)) NULL else tags$p(class = "text-muted", m)
  })

  output$rt_summary <- renderUI({
    d <- rt_data()
    if (is.null(d$readings)) return(NULL)
    s <- rt_summary(d$readings, d$boundary, d$now)
    bl <- rt_bucket_label(rt_bucket_seconds(d$days))
    tags$p(class = "text-muted mb-0",
           sprintf("%s %s: %s archived, %s live (right of the dashed line). Newest %s UTC, %s minutes ago.%s%s",
                   format(s$readings, big.mark = ","), if (nzchar(bl)) "points" else "readings",
                   format(s$archive, big.mark = ","), format(s$live, big.mark = ","),
                   format(s$newest, "%a %d %b %H:%M", tz = "UTC"), format(s$age_minutes),
                   if (nzchar(bl)) sprintf(" Over this span each point is the %s mean of the readings in it (the total for rainfall).", bl) else "",
                   if (is.null(d$boundary)) " Nothing is archived yet, so there is no archive | live line." else ""))
  })

  if (requireNamespace("plotly", quietly = TRUE)) {
    output$rt_plot <- plotly::renderPlotly({
      d <- rt_data()
      req(d$readings)
      panels <- rt_panels(d$readings, d$chosen)
      start <- d$now - d$days * 86400
      cols <- APP_SERIES
      uris <- unique(d$readings$measure_uri)
      plots <- lapply(panels$units, function(u) {
        p <- plotly::plot_ly()
        for (uri in uris[panels$unit_of[uris] == u]) {
          g <- d$readings[measure_uri == uri]
          p <- plotly::add_lines(p, x = g$date_time, y = g$value, name = panels$label_of[[uri]],
                                 line = list(color = cols[(match(uri, uris) - 1L) %% length(cols) + 1L], width = 1.6),
                                 hovertemplate = paste0("%{y} ", u, "<extra>", panels$label_of[[uri]], "</extra>"))
        }
        plotly::layout(p, yaxis = app_plot_axis(u, grid = TRUE, tickformat = ".3~g", nticks = 6), showlegend = TRUE)
      })
      fig <- plotly::subplot(plots, nrows = length(plots), shareX = TRUE, titleY = TRUE, margin = 0.04)
      app_plot_style(fig, height = 280 * length(plots) + 90, hovermode = "x unified",
                     xaxis = app_plot_axis(range = format(c(start, d$now), "%Y-%m-%d %H:%M:%S", tz = "UTC"), type = "date", nticks = 8),
                     shapes = rt_live_shapes(d$boundary, start, d$now),
                     title = list(text = sprintf("Last %d day%s, times in UTC", d$days, if (d$days == 1L) "" else "s"),
                                  x = 0.01, xanchor = "left", font = list(size = 13, color = APP_PALETTE$navy)),
                     margin = list(t = 50, l = 60, r = 20))
    })
  }

  # Briefing draft for the selected row. Text only: the pipeline writes the
  # draft and a person sends it by their own route. Nothing here sends mail.
  output$briefing_panel <- renderUI({
    row <- input$situation_table_rows_selected
    if (is.null(row)) {
      return(tags$p(class = "text-muted", "Select a constituency to see its MP briefing draft."))
    }
    d  <- situation_filtered()
    cid <- d$constituency_id[row]
    cname <- d$constituency_name[row]
    b  <- briefing_view()
    draft <- if (is.null(b)) NULL else b[constituency_id == cid]
    map_btn <- tags$button(type = "button", class = "btn btn-sm btn-outline-secondary",
                           onclick = goto_js(cid), icon("map-location-dot"), " Show on map")
    if (is.null(b)) {                                        # the drafts could not be read at all
      return(tags$div(
        tags$div(class = "d-flex justify-content-between align-items-center", tags$h6(cname), map_btn),
        tags$p(class = "text-muted",
               if (has_gap("mp_briefing_drafts")) gap_text("mp_briefing_drafts", "Briefing drafts")
               else "Briefing drafts could not be loaded.")))
    }

    if (is.null(draft) || nrow(draft) == 0L) {
      return(tags$div(
        tags$div(class = "d-flex justify-content-between align-items-center", tags$h6(cname), map_btn),
        tags$p(class = "text-muted",
               "No briefing draft. The constituency is below the pipeline's briefing ",
               "threshold, or the seat has no matched MP.")))
    }
    draft <- draft[1L]
    tags$div(
      tags$div(class = "d-flex justify-content-between align-items-center",
               tags$h6(sprintf("%s: briefing draft for %s", cname, draft$mp_name)), map_btn),
      tags$p(class = "text-muted mb-1",
             sprintf("Status: %s%s. To: %s. %s",
                     draft$status,
                     if (!is.na(draft$reviewed_by) && nzchar(draft$reviewed_by))
                       paste0(" (by ", draft$reviewed_by, ")") else "",
                     if (is.na(draft$mp_email)) "no email held" else draft$mp_email,
                     if (identical(draft$status, "draft")) "DRAFT ONLY, nothing has been sent."
                     else if (DEMO_MODE) "Recorded in this demonstration only. Nothing was sent."
                     else "This app sends nothing. The status was set elsewhere.")),
      tags$p(tags$strong("Subject: "), draft$subject),
      tags$div(class = "briefing-body", draft$body),
      if (DEMO_MODE && is.null(poc_past_id())) tags$div(
        class = "mt-2 d-flex align-items-center gap-2 flex-wrap",
        actionButton("rv_reviewed", "Mark reviewed", class = "btn-sm btn-outline-secondary"),
        actionButton("rv_sent",     "Mark sent",     class = "btn-sm btn-outline-secondary"),
        actionButton("rv_rejected", "Reject",        class = "btn-sm btn-outline-secondary"),
        tags$span(class = "demo-note", "Demonstration: held in this session only. Nothing is saved or sent."))
    )
  })


  # ---------------------------------------------------------------------------
  # Exposure history tab (job 21).
  #
  # Heat grid: constituencies (rows) by calendar month (columns). The table is
  # sparse, so absent months are zero days, and days_covered (the denominator)
  # sits in each column header's tooltip. "Share" divides by days_covered so a
  # thin month is not mistaken for a quiet one. A row click opens that
  # constituency's history.
  # ---------------------------------------------------------------------------

  exposure_prepared <- reactive({
    e <- exposure_scoped()
    req(e)
    e <- e[source == input$exp_source]
    if (nrow(e) == 0L) return(NULL)
    e <- copy(e)
    e[, covered := as.numeric(days_covered)]
    e[, base := as.numeric(if (input$exp_metric == "days_flagged") days_flagged else days_medium_or_above)]
    e[, value := if (input$exp_metric == "share") 100 * base / pmax(covered, 1) else base]
    this_month <- as.Date(format(Sys.Date(), "%Y-%m-01"))
    months <- rev(format(seq(this_month, by = "-1 month", length.out = 12L), "%Y-%m"))
    list(e = e, months = months)
  })

  output$exposure_legend <- renderUI({
    p <- tryCatch(exposure_prepared(), error = function(e) NULL)
    if (is.null(p)) return(NULL)
    top <- max(p$e$value, 1, na.rm = TRUE)
    unit <- if (input$exp_metric == "share") "%" else " days"
    tags$div(class = "legend-row", tags$span("0"), tags$span(class = "legend-bar"),
             tags$span(paste0(round(top), unit)), tags$span("(darkest cell shown)"))
  })

  output$exposure_grid <- renderUI({
    e0 <- exposure_scoped()
    if (is.null(e0)) {
      return(tags$div(class = "alert alert-warning",
        "fgs_constituency_exposure could not be read. Check the pipeline health badge."))
    }
    p <- exposure_prepared()
    if (is.null(p)) {
      return(tags$div(class = "empty-state",
        "No flagged days in the last 12 months for this source."))
    }
    e <- p$e; months <- p$months
    share <- input$exp_metric == "share"

    cov_of <- e[, .(c = max(covered, na.rm = TRUE)), by = month]
    cov_of <- setNames(cov_of$c, cov_of$month)

    totals <- e[, .(base = sum(base, na.rm = TRUE)), by = .(constituency_id, constituency_name)]
    all_cov <- sum(cov_of[months], na.rm = TRUE)
    totals[, total := if (share) 100 * base / pmax(all_cov, 1) else base]
    totals <- head(totals[order(-total, constituency_name)], input$exp_top_n)
    totals <- totals[total > 0]
    if (nrow(totals) == 0L) {
      return(tags$div(class = "empty-state", "No constituency reached this measure in the last 12 months."))
    }

    vmax <- max(e[constituency_id %in% totals$constituency_id, value], 1, na.rm = TRUE)
    ramp <- grDevices::colorRampPalette(c("#fff3bf", "#e08020", "#b02020"))(100L)
    cell_idx <- function(v) max(1L, ceiling(100 * v / vmax))
    cell_bg  <- function(v) if (is.na(v) || v <= 0) "#f1f3f5" else ramp[cell_idx(v)]
    cell_fg  <- function(v) if (!is.na(v) && v > 0 && cell_idx(v) > 60L) "#ffffff" else "#212529"
    fmt <- function(v) if (share) sprintf("%d%%", round(v)) else as.character(round(v))

    header <- tags$tr(
      tags$th(""),
      lapply(months, function(m) {
        tags$th(title = if (!is.na(cov_of[m])) sprintf("%s days with a statement", cov_of[m]) else "No statement history",
                format(as.Date(paste0(m, "-01")), "%b %y"))
      }),
      tags$th(if (share) "Overall" else "Total")
    )
    rows <- lapply(seq_len(nrow(totals)), function(i) {
      cid <- totals$constituency_id[i]
      cell_rows <- e[constituency_id == cid]
      tags$tr(
        onclick = sprintf("Shiny.setInputValue('exp_pick','%s',{priority:'event'});", gsub("[^A-Za-z0-9_]", "", cid)),
        tags$td(class = "name", totals$constituency_name[i]),
        lapply(months, function(m) {
          v <- cell_rows[month == m, sum(value, na.rm = TRUE)]
          tags$td(class = "cell", style = paste0("background:", cell_bg(v), ";color:", cell_fg(v), ";"),
                  if (v > 0) fmt(v) else "")
        }),
        tags$td(class = "total", fmt(totals$total[i]))
      )
    })
    tags$div(style = "overflow-x: auto;",
             tags$table(class = "exposure-grid", tags$thead(header), tags$tbody(rows)))
  })

  # Row click: that constituency's month-by-month history in a pop-up.
  observeEvent(input$exp_pick, {
    p <- exposure_prepared()
    req(p)
    e <- exposure_scoped()[source == input$exp_source & constituency_id == input$exp_pick]
    if (nrow(e) == 0L) return()
    cname <- e$constituency_name[1L]
    cov <- as.numeric(e$days_covered); names(cov) <- e$month
    hist <- data.table(month = p$months)
    hist <- merge(hist, e[, .(month, med = as.numeric(days_medium_or_above), any = as.numeric(days_flagged), cov = as.numeric(days_covered))],
                  by = "month", all.x = TRUE, sort = FALSE)
    hist <- hist[match(p$months, month)]
    hist[is.na(med), `:=`(med = 0, any = 0)]
    rows <- lapply(seq_len(nrow(hist)), function(i) {
      h <- hist[i]
      pct <- if (!is.na(h$cov) && h$cov > 0) 100 * h$med / h$cov else NA_real_
      tags$tr(tags$td(format(as.Date(paste0(h$month, "-01")), "%b %Y")),
              tags$td(h$med), tags$td(h$any),
              tags$td(if (is.na(h$cov)) "no statements" else h$cov),
              tags$td(style = "width:35%;",
                      if (!is.na(pct)) tags$div(class = "exp-bar", style = sprintf("width:%d%%;", round(pct)))))
    })
    showModal(modalDialog(
      title = paste0(cname, ": exposure, last 12 months"), size = "l", easyClose = TRUE,
      tags$table(class = "table table-sm",
        tags$thead(tags$tr(tags$th("Month"), tags$th("Medium or above"), tags$th("Any risk"),
                           tags$th("Days covered"), tags$th("Share at Medium or above"))),
        tags$tbody(rows)),
      footer = tagList(
        tags$button(type = "button", class = "btn btn-outline-secondary",
                    onclick = goto_js(input$exp_pick), "Show on map"),
        modalButton("Close"))))
  })
}
