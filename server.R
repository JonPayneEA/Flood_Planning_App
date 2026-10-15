# =============================================================================
# server.R
# Reactive server logic for the FGS Flood Guidance dashboard.
#
# Structure:
#   1. Source data.R so the fetch_* functions are available.
#   2. Define one reactive() per logical chunk of derived data.
#   3. Wire those reactives to output slots declared in ui.R.
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

  recent_rv <- reactiveVal(data.table())

  load_statements <- function(keep_selected = FALSE) {
    dt <- fetch_recent_statements(100L)
    recent_rv(dt)
    if (nrow(dt) == 0L) return()
    labels  <- sprintf("%s  (id %d)",
                       format(as.POSIXct(dt$issued_at), "%a %d %b %H:%M"),
                       dt$statement_id)
    choices <- setNames(as.character(dt$statement_id), labels)
    # On refresh keep the currently selected statement if it still exists,
    # otherwise default to the newest.
    current <- isolate(input$statement_id)
    sel <- if (keep_selected && !is.null(current) && current %in% choices) current else choices[1]
    updateSelectInput(session, "statement_id", choices = choices, selected = sel)
  }

  load_statements()

  observeEvent(input$refresh_data, {
    load_statements(keep_selected = TRUE)
  })


  # ---------------------------------------------------------------------------
  # Constituency/MP reference data.
  #
  # ea_area_constituency_lookup and mp_contact_details are static reference
  # tables, not statement-specific, so they're fetched once per session here
  # rather than re-queried on every filter change. Used by the CSV export and
  # by the area-list search box (constituency name as a search key).
  # ---------------------------------------------------------------------------

  ea_constituency_lookup <- fetch_ea_constituency_mp()
  area_constituency_map <- ea_constituency_lookup[
    , .(constituency_names = paste(unique(constituency_name), collapse = ", ")),
    by = ea_area_code
  ]
  
  
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
          & intersection_pct >= f$min_intersection]
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
    shape[shape$intersection_pct >= f$min_intersection, ]
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
    areas <- areas[tolower(source) %in% f$source_codes & intersection_pct >= f$min_intersection]

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
                   & shape$intersection_pct >= f$min_intersection, ]
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
            format(as.POSIXct(stmt$issued_at), "%d %b %Y %H:%M"))
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

    areas <- area_constituency_map[areas, on = "ea_area_code",
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
      areas <- areas[grepl(search, ea_area_name, ignore.case = TRUE, fixed = TRUE) |
                       grepl(search, constituency_names, ignore.case = TRUE, fixed = TRUE)]
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
            sprintf("%s - %s - %d%%",
                    r$ea_area_type, r$source,
                    round(r$intersection_pct * 100)),
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
      lookup <- ea_constituency_lookup

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
  
  observe({
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
    
    leaflet::addPolygons(
      proxy,
      data        = shape,
      group       = "polygons",
      color       = fill_hex,
      weight      = 3,
      fillColor   = fill_hex,
      fillOpacity = 0.60,
      popup       = popup_html,
      highlightOptions = leaflet::highlightOptions(
        weight       = 3,
        fillOpacity  = 0.65,
        bringToFront = TRUE
      )
    )

    busy_polygons(FALSE)
  })
  
  
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

  observe({
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
      "Intersection: ", round(shape$intersection_pct * 100), "%"
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
  })
  
  
  # ---------------------------------------------------------------------------
  # Constituency layer.
  #
  # Outline only, no fill -- the FGS and EA layers carry the colour load and
  # adding a third coloured layer is too noisy. The outlines give enough
  # constituency context for an MP-notification eyeball check.
  # ---------------------------------------------------------------------------
  
  observe({
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
  })


  # ---------------------------------------------------------------------------
  # Area-list row click -> map.
  #
  # Clicking a row in the Affected EA Warnings panel flies the map to that
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
      "Intersection: ", round(matched$intersection_pct[1] * 100), "%"
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

  safe_fetch <- function(fn, ...) {
    tryCatch(fn(...), error = function(e) {
      message("[", format(Sys.time(), "%H:%M:%S"), "] ", deparse(substitute(fn)),
              " failed: ", conditionMessage(e))
      NULL
    })
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

  load_products <- function() {
    health_rv(safe_fetch(fetch_pipeline_health))
    situation_rv(safe_fetch(fetch_situation))
    briefing_rv(safe_fetch(fetch_briefing_drafts))
    exposure_rv(safe_fetch(fetch_exposure, 12L))
  }

  load_products()
  observeEvent(input$refresh_data, load_products(), ignoreInit = TRUE)


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
          warn = sprintf("Pipeline: %d check%s need attention", n_bad, if (n_bad == 1L) "" else "s"),
          fail = sprintf("Pipeline: %d check%s failing", n_bad, if (n_bad == 1L) "" else "s"),
          "Pipeline status unavailable")
        if (nrow(overall) > 0L && !is.na(overall$checked_at[1L])) {
          text <- paste0(text, " (", short_ts(overall$checked_at[1L]), " UTC)")
        }
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
    safe_fetch(fetch_changes, stmt$statement_id)
  })

  output$changes_strip <- renderUI({
    ch <- changes()
    if (is.null(ch)) return(NULL)
    if (nrow(ch) == 0L) {
      return(tags$div(class = "changes-strip text-muted",
        "No changes recorded against the previous statement ",
        "(or this is the first complete statement)."))
    }
    n_by <- function(types) ch[change_type %in% types, uniqueN(constituency_id)]
    up   <- n_by(c("new", "escalated"))
    down <- n_by(c("de_escalated", "cleared"))

    shown <- head(ch[order(-new_rank, -rank_change, constituency_name)], 150L)
    rows <- lapply(seq_len(nrow(shown)), function(i) {
      r <- shown[i]
      tags$tr(
        tags$td(r$constituency_name),
        tags$td(format(as.Date(r$forecast_date), "%a %d %b")),
        tags$td(r$source),
        tags$td(sprintf("%s → %s", rank_label(r$previous_rank), rank_label(r$new_rank))),
        tags$td(sub("_", "-", r$change_type, fixed = TRUE))
      )
    })

    tags$details(
      class = "changes-strip",
      tags$summary(
        tags$span("Since the previous statement: "),
        tags$span(class = "chg-count chg-up",   sprintf("%d up", up)),
        tags$span(" · "),
        tags$span(class = "chg-count chg-down", sprintf("%d down", down)),
        tags$span(class = "text-muted", " (constituencies)")
      ),
      tags$div(class = "chg-scroll",
        tags$table(
          tags$thead(tags$tr(tags$th("Constituency"), tags$th("Day"), tags$th("Source"),
                             tags$th("Risk"), tags$th("Change"))),
          tags$tbody(rows)
        ),
        if (nrow(ch) > nrow(shown))
          tags$small(class = "text-muted",
                     sprintf("Showing the first %d of %d rows.", nrow(shown), nrow(ch)))
      )
    )
  })


  # ---------------------------------------------------------------------------
  # Constituency situation tab (jobs 19 and 20).
  # ---------------------------------------------------------------------------

  situation_filtered <- reactive({
    d <- situation_rv()
    req(d)
    d <- copy(d)
    d[, `:=`(fgs_worst_rank = as.integer(fgs_worst_rank),
             fgs_escalated  = as.logical(fgs_escalated),
             has_active_warning = as.logical(has_active_warning))]
    min_rank <- as.integer(input$sit_min_rank)
    keep <- d$fgs_worst_rank >= min_rank
    if (isTRUE(input$sit_escalated)) keep <- keep & d$fgs_escalated
    if (isTRUE(input$sit_warning))   keep <- keep & d$has_active_warning
    d[keep][order(-fgs_worst_rank, -severe_warnings, -warnings, constituency_name)]
  })

  output$situation_note <- renderUI({
    d <- situation_rv()
    if (is.null(d) || nrow(d) == 0L) {
      return(tags$div(class = "alert alert-warning",
        "constituency_flood_situation could not be read. Check the pipeline health badge."))
    }
    tags$div(class = "text-muted mb-2",
      sprintf("Latest complete statement: id %s, issued %s UTC. This view does not follow the statement picker. ",
              d$fgs_statement_id[1L], short_ts(d$fgs_issued_at[1L])),
      "Forecast and warnings are separate measures and are not combined. ",
      "Set the minimum risk to Any to see constituencies with only a warning or alert in force.")
  })

  output$situation_table <- DT::renderDataTable({
    d <- situation_filtered()
    tbl <- d[, .(
      Constituency = constituency_name,
      MP           = fifelse(is.na(mp_name), "(vacant or unmatched)", mp_name),
      Party        = mp_party,
      Worst        = fgs_worst_rank,
      On           = as.character(fgs_worst_date),
      Sources      = fgs_worst_sources,
      D1 = fgs_rank_day1, D2 = fgs_rank_day2, D3 = fgs_rank_day3,
      D4 = fgs_rank_day4, D5 = fgs_rank_day5,
      Escalated    = fifelse(as.logical(fgs_escalated), "Yes", ""),
      Severe       = severe_warnings,
      Warnings     = warnings,
      Alerts       = alerts
    )]
    rank_cols <- c("Worst", "D1", "D2", "D3", "D4", "D5")
    labels_js <- jsonlite::toJSON(RANK_LABELS, auto_unbox = FALSE)
    render_js <- DT::JS(sprintf(
      "function(data, type) { var L = %s; return type === 'display' ? L[data] || L[0] : data; }",
      labels_js))
    # columnDefs targets are 0-based.
    targets <- match(rank_cols, names(tbl)) - 1L

    DT::datatable(
      tbl, rownames = FALSE, selection = "single",
      options = list(pageLength = 25, scrollX = TRUE,
                     columnDefs = list(list(targets = targets, render = render_js))),
      caption = sprintf("%d constituencies", nrow(tbl))
    ) |>
      DT::formatStyle(rank_cols, backgroundColor = DT::styleEqual(0:4, RANK_HEX))
  })

  output$download_situation <- downloadHandler(
    filename = function() sprintf("constituency_situation_%s.csv", format(Sys.time(), "%Y%m%d_%H%M")),
    content  = function(file) {
      d <- situation_filtered()
      out <- d[, .(constituency_name, mp_name, mp_party, mp_email, mp_phone,
                   fgs_worst_level, fgs_worst_date, fgs_worst_sources,
                   fgs_rank_day1, fgs_rank_day2, fgs_rank_day3, fgs_rank_day4, fgs_rank_day5,
                   fgs_escalated, severe_warnings, warnings, alerts)]
      data.table::fwrite(out, file)
    }
  )

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
    b  <- briefing_rv()
    draft <- if (is.null(b)) NULL else b[constituency_id == cid]

    if (is.null(draft) || nrow(draft) == 0L) {
      return(tags$div(
        tags$h6(cname),
        tags$p(class = "text-muted",
               "No briefing draft. The constituency is below the pipeline's briefing ",
               "threshold, or the seat has no matched MP.")))
    }
    draft <- draft[1L]
    tags$div(
      tags$h6(sprintf("%s: briefing draft for %s", cname, draft$mp_name)),
      tags$p(class = "text-muted mb-1",
             sprintf("Status: %s%s. To: %s. DRAFT ONLY, nothing has been sent.",
                     draft$status,
                     if (!is.na(draft$reviewed_by) && nzchar(draft$reviewed_by))
                       paste0(" (reviewed by ", draft$reviewed_by, ")") else "",
                     if (is.na(draft$mp_email)) "no email held" else draft$mp_email)),
      tags$p(tags$strong("Subject: "), draft$subject),
      tags$div(class = "briefing-body", draft$body)
    )
  })


  # ---------------------------------------------------------------------------
  # Exposure history tab (job 21).
  #
  # Heat grid: constituencies (rows) by calendar month (columns), coloured by
  # the chosen measure. The table is sparse, so absent months are zero days,
  # while days_covered (the denominator) is shown in each column header's
  # tooltip so a thin month is not mistaken for a quiet one.
  # ---------------------------------------------------------------------------

  output$exposure_grid <- renderUI({
    e <- exposure_rv()
    if (is.null(e)) {
      return(tags$div(class = "alert alert-warning",
        "fgs_constituency_exposure could not be read. Check the pipeline health badge."))
    }
    metric <- input$exp_metric
    e <- e[source == input$exp_source]
    if (nrow(e) == 0L) {
      return(tags$p(class = "text-muted", "No flagged days in the last 12 months for this source."))
    }
    e[, value := as.numeric(get(metric))]

    # Last 12 calendar months, oldest first, so empty months still show.
    this_month <- as.Date(format(Sys.Date(), "%Y-%m-01"))
    months <- format(seq(this_month, by = "-1 month", length.out = 12L), "%Y-%m")
    months <- rev(months)

    totals <- e[, .(total = sum(value, na.rm = TRUE)), by = .(constituency_id, constituency_name)]
    totals <- head(totals[order(-total, constituency_name)], input$exp_top_n)
    totals <- totals[total > 0]
    if (nrow(totals) == 0L) {
      return(tags$p(class = "text-muted", "No constituency reached this measure in the last 12 months."))
    }

    covered <- e[, .(days_covered = max(as.numeric(days_covered), na.rm = TRUE)), by = month]
    cov_of  <- setNames(covered$days_covered, covered$month)
    vmax    <- max(e[constituency_id %in% totals$constituency_id, value], 1, na.rm = TRUE)
    ramp    <- grDevices::colorRampPalette(c("#fff3bf", "#e08020", "#b02020"))(100L)
    cell_bg <- function(v) if (is.na(v) || v <= 0) "#f1f3f5" else ramp[max(1L, ceiling(100 * v / vmax))]

    header <- tags$tr(
      tags$th(""),
      lapply(months, function(m) {
        tags$th(title = if (!is.na(cov_of[m])) sprintf("%s days with a statement", cov_of[m]) else "No statement history",
                format(as.Date(paste0(m, "-01")), "%b %y"))
      }),
      tags$th("Total")
    )
    rows <- lapply(seq_len(nrow(totals)), function(i) {
      cid <- totals$constituency_id[i]
      cell_rows <- e[constituency_id == cid]
      tags$tr(
        tags$td(class = "name", totals$constituency_name[i]),
        lapply(months, function(m) {
          v <- cell_rows[month == m, sum(value, na.rm = TRUE)]
          tags$td(class = "cell", style = paste0("background:", cell_bg(v), ";"),
                  if (v > 0) v else "")
        }),
        tags$td(class = "total", totals$total[i])
      )
    })

    tags$div(style = "overflow-x: auto;",
             tags$table(class = "exposure-grid", tags$thead(header), tags$tbody(rows)))
  })
}
