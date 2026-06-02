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


  # ===========================================================================
  # REACTIVE DATA LAYER
  # ===========================================================================

  # ---------------------------------------------------------------------------
  # latest_statement()
  #
  # The most recently issued FGS statement, as a named list. Every other
  # reactive depends on this for the statement_id, so we cache it implicitly:
  # Shiny's reactive() only re-runs when its dependencies change, and this
  # one has no inputs -- it runs once per session start, then the cached
  # value is returned to subsequent callers.
  #
  # If the user wants a fresh statement_id mid-session, they reload the
  # browser. For an operational dashboard refreshed every few hours, this is
  # fine. If we needed sub-session refresh later we'd wrap it in
  # reactivePoll() with a small interval.
  # ---------------------------------------------------------------------------

  latest_statement <- reactive({
    fetch_latest_statement()
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
    stmt <- latest_statement()
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

    list(
      statement_id     = as.integer(stmt$statement_id),
      day_index        = as.integer(input$day_index),
      source_codes     = source_codes,
      selected_cells   = selected_cells,
      min_intersection = input$intersect_threshold / 100   # convert % to 0-1
    )
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
    shape <- shape[shape$source %in% f$source_codes, ]
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
    # Two filters: source must be selected, intersection must clear threshold.
    # data.table's [i] syntax does both in one expression, no copies.
    areas[source %in% f$source_codes & intersection_pct >= f$min_intersection]
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
    stmt <- latest_statement()
    if (is.null(stmt)) return("No statement loaded")

    sprintf("Forecast Day %d - issued %s",
            as.integer(input$day_index),
            format(as.POSIXct(stmt$issued_at), "%d %b %Y %H:%M"))
  })

  output$map_subtitle <- renderText({
    c    <- counts()
    stmt <- latest_statement()
    sprintf("Statement %s - %d polygons - %d EA areas",
            if (is.null(stmt)) "-" else stmt$statement_id,
            c$polygon_count, c$ea_area_count)
  })


  # ---------------------------------------------------------------------------
  # Stat cards.
  #
  # format() with big.mark = "," puts a comma thousand separator on the
  # constituency count once it gets big enough to need one. Cheap, and
  # avoids the dashboard looking amateur with "1247" instead of "1,247".
  # ---------------------------------------------------------------------------

  output$count_polygons       <- renderText(format(counts()$polygon_count,      big.mark = ","))
  output$count_ea_areas       <- renderText(format(counts()$ea_area_count,      big.mark = ","))
  output$count_constituencies <- renderText(format(counts()$constituency_count, big.mark = ","))


  # ---------------------------------------------------------------------------
  # Affected-areas list.
  #
  # Built as raw HTML via htmltools, not a DataTable -- the row layout (4px
  # coloured bar + two lines of text) isn't a natural fit for a tabular
  # widget. We cap visible rows at 20 and show a "+ N more" footer so the
  # panel doesn't run the page to the floor in a major event.
  # ---------------------------------------------------------------------------

  output$ea_area_list <- renderUI({
    areas <- ea_areas()

    if (is.null(areas) || nrow(areas) == 0L) {
      return(tags$p("No EA areas above threshold.", class = "text-muted small"))
    }

    n_visible <- min(nrow(areas), 20L)

    rows <- lapply(seq_len(n_visible), function(i) {
      r <- areas[i]                             # one-row data.table
      tags$div(
        class = "area-row",
        tags$div(class = paste("risk-bar", r$risk_colour)),
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
    m <- leaflet::addProviderTiles(m, leaflet::providers$OpenStreetMap)
    m <- leaflet::setView(m, lng = -2.0, lat = 53.2, zoom = 6)
    m
  })

  observe({
    shape <- polygons()

    # Get a handle to the existing map and clear last render's polygons.
    proxy <- leaflet::leafletProxy("live_map")
    proxy <- leaflet::clearGroup(proxy, "polygons")

    # If polygons should be hidden or there's nothing to draw, we're done.
    if (!isTRUE(input$layer_polygons))       return()
    if (is.null(shape) || nrow(shape) == 0L) return()

    # Look up hex colours for each row's risk_colour band. Vectorised lookup
    # via named-vector indexing -- no loop, no apply().
    fill_hex <- RISK_COLOUR_HEX[shape$risk_colour]

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
      weight      = 1.5,
      fillColor   = fill_hex,
      fillOpacity = 0.45,
      popup       = popup_html,
      highlightOptions = leaflet::highlightOptions(
        weight       = 3,
        fillOpacity  = 0.65,
        bringToFront = TRUE
      )
    )
  })
}
