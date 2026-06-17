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
}
