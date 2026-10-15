# =============================================================================
# ui.R
# Declarative UI for the Flood Intelligence Tool dashboard.
#
# Three-column layout for the Live view tab: a left sidebar of controls, a
# centre column holding the map and a small title block, and a right column
# of count cards and the affected-areas list. The other five tabs are
# placeholders we'll fill in later.
#
# This file produces no reactive output itself -- every textOutput, uiOutput,
# and leafletOutput slot is filled by callbacks in server.R.
# =============================================================================


# -----------------------------------------------------------------------------
# matrix_filter_ui()
#
# Builds the 4x4 FGS risk matrix as a CSS grid of styled cells, each holding
# a checkbox. Each checkbox is given inputId 'mtx_{x}_{y}' so server.R can
# loop over the same x/y coordinates and read which cells are selected.
#
# Two default tactics:
#   - Amber and Red cells default ticked, because forecasters care about
#     elevated risk first. The default view shows what matters most.
#   - Green and Yellow cells default unticked; the operator opts in when
#     they want to see lower-band polygons too.
#
# We loop over RISK_MATRIX (defined in global.R) rather than hard-coding the
# sixteen cells so the matrix is defined in exactly one place.
# -----------------------------------------------------------------------------

matrix_filter_ui <- function() {

  # Grid layout (columns: likelihood-label | row-labels | 4 cell columns;
  # rows: column-labels | 4 cell rows | impact-label). Cell column/row is
  # cell$x + 2 / 6 - cell$y so y = 4 (High) lands in the top data row,
  # matching the FGS User Guide's matrix orientation.
  cells <- lapply(seq_len(nrow(RISK_MATRIX)), function(i) {

    cell <- RISK_MATRIX[i]                          # one-row data.table
    # default_ticked <- cell$colour %in% c("Amber", "Red")
    default_ticked <- cell$x >1

    tags$div(
      class    = paste("matrix-cell", tolower(cell$colour)),  # e.g. "matrix-cell red"
      style    = sprintf("grid-column: %d; grid-row: %d;", cell$x + 2, 6 - cell$y),
      `data-x` = cell$x,                             # data-* attrs for any
      `data-y` = cell$y,                             # future JS hook
      checkboxInput(
        inputId = paste0("mtx_", cell$x, "_", cell$y),
        label   = NULL,
        value   = default_ticked
      )
    )
  })

  col_headers <- lapply(seq_len(4), function(x) {
    tags$div(
      class = "matrix-col-header",
      style = sprintf("grid-column: %d; grid-row: 1;", x + 2),
      IMPACT_LABELS[x]
    )
  })

  row_headers <- lapply(seq_len(4), function(y) {
    tags$div(
      class = "matrix-row-header",
      style = sprintf("grid-column: 2; grid-row: %d;", 6 - y),
      LIKELIHOOD_LABELS[y]
    )
  })

  likelihood_label <- tags$div(
    class = "matrix-likelihood-label",
    style = "grid-column: 1; grid-row: 2 / 6;",
    "Likelihood"
  )

  impact_label <- tags$div(
    class = "matrix-impact-label",
    style = "grid-column: 3 / 7; grid-row: 6;",
    "Impact"
  )

  grid_children <- c(col_headers, row_headers, cells, list(likelihood_label, impact_label))

  # tagList holds a label, a hint, and the grid itself. do.call(tags$div, ...)
  # is the standard way to splat a list of children into a single parent tag.
  tagList(
    h6("Risk matrix position"),
    tags$small("Tick cells to include - counts update live",
               class = "text-muted d-block mb-2"),
    do.call(tags$div, c(list(class = "risk-matrix-wrap"), grid_children))
  )
}


# -----------------------------------------------------------------------------
# Custom CSS.
#
# Inline rather than a separate www/styles.css file so the deploy is a single
# bundle. The volume is small enough that putting it in a file doesn't save
# much, and inline keeps the CSS adjacent to the UI it styles.
#
# Three groups of rules:
#   1. Layout chrome -- sidebar background, panel sections, headings
#   2. Stat cards -- the count tiles at the top of the right column
#   3. Risk matrix -- grid layout and per-band cell colours
# -----------------------------------------------------------------------------

custom_css <- "
/* --- 1. Layout chrome ----------------------------------------------------- */

body { background: #f8f9fa; font-size: 14px; }

.shiny-sidebar { background: #e9ecef; padding: 20px; height: 100%; }

.shiny-sidebar h6 {
  font-size: 11px; text-transform: uppercase; letter-spacing: 0.05em;
  color: #495057; margin-top: 16px; margin-bottom: 8px;
}
.shiny-sidebar h6:first-child { margin-top: 0; }

.panel-section {
  background: #fff; border: 1px solid #dee2e6; border-radius: 4px;
  padding: 14px; margin-bottom: 12px;
}
.panel-h {
  font-size: 12px; text-transform: uppercase; letter-spacing: 0.05em;
  color: #495057; padding-bottom: 6px; border-bottom: 1px solid #e9ecef;
}

/* --- 2. Stat cards -------------------------------------------------------- */

div.stat-card {
  background: #fff;
  border: 1px solid #dee2e6;
  border-radius: 4px;
  padding: 12px;
  text-align: center;
  /* Explicit display block so this can't ever be picked up as an inline-block
     or button by an over-eager Bootstrap rule. */
  display: block;
  cursor: default;
}
div.stat-card .stat-num { font-size: 24px; font-weight: 700; line-height: 1; color: #212529; }
div.stat-card .stat-num.accent { color: #c8581f; }
div.stat-card .stat-label {
  font-size: 10px; text-transform: uppercase; letter-spacing: 0.05em;
  color: #6c757d; margin-top: 4px;
}

/* --- Affected-area list rows --------------------------------------------- */

.area-row {
  display: flex; align-items: center; padding: 6px 0;
  border-bottom: 1px solid #f1f3f5; font-size: 13px;
  cursor: pointer;
}
.area-row:last-child { border-bottom: none; }
.area-row:hover { background: #f8f9fa; }
.risk-bar { width: 4px; height: 22px; margin-right: 10px; border-radius: 1px; }
.risk-bar.red    { background: #b02020; }
.risk-bar.amber  { background: #e08020; }
.risk-bar.yellow { background: #f0d040; }
.risk-bar.green  { background: #4f9d4f; }

/* --- 3. Risk matrix grid -------------------------------------------------- */

.risk-matrix-wrap {
  display: grid !important;
  grid-template-columns: 16px 64px repeat(4, 36px) !important;
  grid-template-rows: 40px repeat(4, 36px) 18px !important;
  gap: 2px;
  margin-top: 8px;
  margin-bottom: 4px;
  width: max-content !important;
  align-items: center;
}
.matrix-cell {
  width: 36px !important;
  height: 36px !important;
  position: relative;
  display: flex;
  align-items: center;
  justify-content: center;
  flex: none !important;
}
.matrix-cell .form-group     { margin: 0 !important; }
.matrix-cell .form-check     { margin: 0 !important; padding: 0 !important; min-height: 0 !important; }
.matrix-cell .form-check-input {
  margin: 0;
  cursor: pointer;
  float: none;          /* Bootstrap 5 floats checkboxes by default; we don't want that */
}
.matrix-cell .form-check-label { display: none; }  /* hide the empty label that Shiny adds */
.matrix-cell.green  { background: #4f9d4f; }
.matrix-cell.yellow { background: #f0d040; }
.matrix-cell.amber  { background: #e08020; }
.matrix-cell.red    { background: #b02020; }

.matrix-col-header {
  font-size: 10px; color: #495057; text-align: left;
  transform: rotate(35deg);
  transform-origin: bottom left;
  white-space: nowrap;
  position: relative;
  z-index: 10;
  align-self: start;
}
.matrix-row-header { font-size: 10px; color: #495057; text-align: right; padding-right: 4px; }

.matrix-likelihood-label {
  writing-mode: vertical-rl;
  transform: rotate(180deg);
  text-align: center;
  font-size: 10px;
  font-weight: 600;
  color: #495057;
}
.matrix-impact-label {
  text-align: center;
  font-size: 10px;
  font-weight: 600;
  color: #495057;
}

/* --- 4. Quiet-state notice ------------------------------------------------ */

/* Shown above the map when the current statement-day has no risk polygons.
   A neutral light-blue rather than warning-amber, because this is a normal
   operational state. */
.quiet-notice {
  background: #e7f1f9;
  border: 1px solid #b6d4ec;
  color: #1f4f7a;
  border-radius: 4px;
  padding: 10px 14px;
  margin-bottom: 12px;
  font-size: 13px;
}
.quiet-notice strong { font-weight: 600; }

/* --- 5. Map loading overlay ------------------------------------------------ */

/* Shown over the map while a layer redraw is fetching from Databricks.
   Pointer-events none so the map stays interactive (pan/zoom) while it's
   loading, rather than blocking input underneath. */
.map-spinner-overlay {
  position: absolute;
  top: 0; left: 0; right: 0; bottom: 0;
  background: rgba(255, 255, 255, 0.5);
  display: flex;
  align-items: center;
  justify-content: center;
  z-index: 1000;
  pointer-events: none;
}
.map-spinner {
  width: 36px;
  height: 36px;
  border: 4px solid #dee2e6;
  border-top-color: #c8581f;
  border-radius: 50%;
  animation: map-spin 0.8s linear infinite;
}
@keyframes map-spin {
  to { transform: rotate(360deg); }
}

/* --- 6. Headline notice & statement info boxes ----------------------------- */

.headline-notice {
  background: #fff;
  border: 1px solid #dee2e6;
  border-left: 4px solid #dee2e6;  /* colour overridden inline from risk keyword */
  border-radius: 4px;
  padding: 10px 14px;
  margin-bottom: 12px;
  font-size: 13px;
}
.headline-notice strong { font-weight: 600; }

.source-info-stack {
  display: flex;
  flex-direction: column;
  gap: 8px;
}
.source-info-card {
  background: #fff;
  border: 1px solid #dee2e6;
  border-left: 4px solid #dee2e6;  /* width fixed; colour overridden inline per card */
  border-radius: 4px;
  padding: 10px 14px;
}
.source-info-label {
  font-size: 10px;
  text-transform: uppercase;
  letter-spacing: 0.05em;
  color: #6c757d;
  margin-bottom: 4px;
}
.source-info-value {
  font-size: 13px;
  color: #212529;
  line-height: 1.4;
}

/* --- 5. Header, health badge, changes strip ------------------------------ */

.app-header {
  display: flex; align-items: center; justify-content: space-between;
  padding: 16px 15px 8px 15px;
}
.app-header h2 { margin: 0; font-size: 1.6rem; }
.health-badge {
  display: inline-flex; align-items: center; gap: 6px;
  padding: 3px 10px; border-radius: 12px; font-size: 12px;
  border: 1px solid transparent; background: none; cursor: pointer;
}
.health-badge .dot { width: 9px; height: 9px; border-radius: 50%; }
.health-ok   { color: #1e6b1e; border-color: #4f9d4f; background: #eaf5ea; }
.health-ok   .dot { background: #4f9d4f; }
.health-warn { color: #8a5a00; border-color: #e08020; background: #fdf1e2; }
.health-warn .dot { background: #e08020; }
.health-fail { color: #8b1a1a; border-color: #b02020; background: #f9e5e5; }
.health-fail .dot { background: #b02020; }
.health-unknown { color: #495057; border-color: #adb5bd; background: #f1f3f5; }
.health-unknown .dot { background: #adb5bd; }

.changes-strip {
  background: #fff; border: 1px solid #dee2e6; border-radius: 6px;
  padding: 8px 12px; margin-bottom: 12px; font-size: 13px;
}
.changes-strip summary { cursor: pointer; }
.changes-strip .chg-count { font-weight: 600; }
.changes-strip .chg-up   { color: #b02020; }
.changes-strip .chg-down { color: #1e6b1e; }
.changes-strip table { width: 100%; margin-top: 8px; font-size: 12px; }
.changes-strip td, .changes-strip th { padding: 2px 6px; }
.changes-strip .chg-scroll { max-height: 220px; overflow-y: auto; }

/* --- 6. Rank chips, exposure grid, briefing text ------------------------ */

.rank-chip {
  display: inline-block; min-width: 54px; text-align: center;
  padding: 1px 6px; border-radius: 3px; font-size: 11px; color: #212529;
}
.exposure-grid { border-collapse: collapse; font-size: 12px; }
.exposure-grid th { font-weight: 500; padding: 2px 6px; text-align: center; }
.exposure-grid td.name { text-align: left; padding: 2px 10px 2px 0; white-space: nowrap; }
.exposure-grid td.cell { width: 44px; height: 24px; text-align: center; border: 2px solid #f8f9fa; }
.exposure-grid td.total { padding-left: 10px; font-weight: 600; }
.briefing-body {
  white-space: pre-wrap; background: #fff; border: 1px solid #dee2e6;
  border-radius: 6px; padding: 12px; font-size: 13px;
}
"


# -----------------------------------------------------------------------------
# Sidebar (left column).
#
# Controls grouped by purpose: layers, source, matrix, threshold, day. The
# order matters -- forecasters tend to set layers first, then narrow by
# source, then matrix cell, then numeric thresholds. The UI reads top-down
# in that order of decreasing frequency-of-change.
# -----------------------------------------------------------------------------

sidebar <- tags$div(
  class = "shiny-sidebar",
  
  h6("FGS statement"),
  # Choices populated server-side once fetch_recent_statements() has run.
  # Display labels show issue date/time; values are statement_ids so the
  # filters() reactive in server.R picks the right rows.
  selectInput("statement_id", NULL,
              choices = NULL,         # populated by updateSelectInput in server
              selectize = FALSE),
  actionButton("refresh_data", "Refresh statements",
               class = "btn-sm btn-outline-secondary w-100 mb-2",
               icon  = icon("rotate-right")),
  
  h6("Map layers"),
  checkboxInput("layer_polygons", "FGS risk polygons", value = TRUE),
  checkboxInput("layer_ea_areas", "EA Warning Areas",  value = TRUE),

  # Sub-control for the EA layer: pick which area types appear. Indented
  # visually so it reads as a child of the EA layer toggle, and placed
  # directly under it (not after the constituencies toggle) so it reads
  # as belonging to EA areas rather than constituencies.
  conditionalPanel(
    condition = "input.layer_ea_areas == true",
    tags$div(
      style = "margin-left: 16px; margin-top: -4px;",
      checkboxInput("ea_type_fwa", "Flood Warning Areas", value = TRUE),
      checkboxInput("ea_type_faa", "Flood Alert Areas",   value = TRUE)
    )
  ),

  checkboxInput("layer_constituencies", "Constituencies", value = FALSE),

  h6("Risk source"),
  checkboxInput("src_river",   "River",       value = TRUE),
  checkboxInput("src_surface", "Surface",     value = TRUE),
  checkboxInput("src_coastal", "Coastal",     value = TRUE),
  checkboxInput("src_ground",  "Groundwater", value = FALSE),
  
  matrix_filter_ui(),
  
  h6("Intersection threshold"),
  sliderInput("intersect_threshold", NULL,
              min = 0, max = 100, value = 25, post = "%"),
  
  h6("Forecast day"),
  selectInput("day_index", NULL,
              choices  = c("Day 1" = "1", "Day 2" = "2",
                           "Day 3" = "3", "Day 4" = "4", "Day 5" = "5"),
              selected = "1")
)


# -----------------------------------------------------------------------------
# Centre column (title + map).
#
# leafletOutput renders the map. height is fixed at 500px which works for
# most laptop screens; on bigger displays the column resizes laterally but
# the map height stays the same to keep aspect ratios readable.
# -----------------------------------------------------------------------------

centre <- tags$div(
  class = "p-3",
  tags$div(
    class = "mb-3",
    h5(textOutput("map_title", inline = TRUE)),
    tags$small(textOutput("map_subtitle", inline = TRUE), class = "text-muted")
  ),
  # Headline notice. Shown for every loaded statement, above the quiet-state
  # notice -- statement-level context first, then the day-specific risk
  # state, then the map itself.
  uiOutput("headline_notice"),
  # What changed against the previous statement (pipeline job 18).
  uiOutput("changes_strip"),
  # Quiet-state notice. Shown when there are no FGS polygons for the current
  # statement-day. A quiet FGS is operationally normal -- this is an
  # expected state, not an error.
  uiOutput("quiet_notice"),
  # position: relative so the loading overlay (absolutely positioned) covers
  # exactly the map area, not the whole centre column.
  tags$div(
    style = "position: relative;",
    leaflet::leafletOutput("live_map", height = "500px"),
    uiOutput("map_loading_overlay")
  ),
  # Per-source forecast text, England-wide forecast, and PDF link.
  uiOutput("statement_info")
)


# -----------------------------------------------------------------------------
# Right column (counts + affected-areas list).
#
# Three stat cards stacked, then a panel showing affected EA areas with
# coloured risk bars. The area list is rendered as raw HTML rather than a
# DataTable because the visual format (coloured bar + two-line label) is
# easier to build with htmltools than to coerce out of DT.
# -----------------------------------------------------------------------------

right_panel <- tags$div(
  class = "p-3",
  
  # Stat cards.
  tags$div(
    class = "mb-3",
    tags$div(class = "stat-card",
             tags$div(textOutput("count_polygons", inline = TRUE),
                      class = "stat-num accent"),
             tags$div("FGS polygons", class = "stat-label")),
    
    tags$div(class = "stat-card mt-2",
             tags$div(textOutput("count_ea_areas", inline = TRUE),
                      class = "stat-num"),
             tags$div("Affected EA Warnings", class = "stat-label")),
    
    tags$div(class = "stat-card mt-2",
             tags$div(textOutput("count_constituencies", inline = TRUE),
                      class = "stat-num"),
             tags$div("Constituencies", class = "stat-label"))
  ),
  
  # Affected-areas list (server.R builds the row markup).
  tags$div(
    class = "panel-section",
    tags$div(
      class = "d-flex justify-content-between align-items-center",
      h6("Affected EA Warnings", class = "panel-h"),
      downloadButton("download_ea_areas", "Export CSV",
                     class = "btn-sm btn-outline-secondary")
    ),
    textInput("area_search", NULL,
              placeholder = "Search by area or constituency..."),
    uiOutput("ea_area_list")
  )
)


# -----------------------------------------------------------------------------
# Constituency situation tab.
#
# One row per constituency from constituency_flood_situation (job 19). The
# table always reflects the pipeline's latest complete statement, not the
# statement picked on the Interrogator tab; the note under the filters says
# which statement that is. Selecting a row shows the MP briefing draft, if
# one exists. The drafts are text only: nothing is sent from here.
# -----------------------------------------------------------------------------

situation_tab <- tags$div(
  class = "p-3",
  uiOutput("situation_note"),
  fluidRow(
    column(3, selectInput("sit_min_rank", "Minimum forecast risk",
                          choices  = c("Any" = "0", "Very Low" = "1", "Low" = "2",
                                       "Medium" = "3", "High" = "4"),
                          selected = "3")),
    column(3, tags$div(style = "margin-top: 30px;",
                       checkboxInput("sit_escalated", "Escalated only", FALSE))),
    column(3, tags$div(style = "margin-top: 30px;",
                       checkboxInput("sit_warning", "Warning or alert in force", FALSE))),
    column(3, tags$div(style = "margin-top: 25px;",
                       downloadButton("download_situation", "Export CSV",
                                      class = "btn-sm btn-outline-secondary")))
  ),
  DT::dataTableOutput("situation_table"),
  tags$hr(),
  uiOutput("briefing_panel")
)


# -----------------------------------------------------------------------------
# Exposure history tab.
#
# Heat grid from fgs_constituency_exposure (job 21): the constituencies with
# most days at or above a chosen risk over the last 12 months, by month.
# -----------------------------------------------------------------------------

exposure_tab <- tags$div(
  class = "p-3",
  fluidRow(
    column(3, selectInput("exp_source", "Flood source",
                          choices  = c("Any source" = "any", "River" = "river",
                                       "Surface" = "surface", "Coastal" = "coastal",
                                       "Groundwater" = "ground"),
                          selected = "any")),
    column(3, selectInput("exp_metric", "Measure",
                          choices  = c("Days at Medium or above" = "days_medium_or_above",
                                       "Days at any flagged risk" = "days_flagged"),
                          selected = "days_medium_or_above")),
    column(3, sliderInput("exp_top_n", "Constituencies shown",
                          min = 10, max = 50, value = 25, step = 5))
  ),
  tags$small(class = "text-muted d-block mb-2",
             "Counts one day per date (day 1 of the last statement issued that date). ",
             "A blank cell means no flagged day that month, not missing data. ",
             "Days with no statement are not counted as quiet."),
  uiOutput("exposure_grid")
)


# -----------------------------------------------------------------------------
# Top-level UI.
#
# tabsetPanel with the live view active; the five analytics tabs are
# placeholder panels we'll fill in later. The tab order matches the wireframe
# we agreed on (live first, then analytics in order of operational usefulness).
# -----------------------------------------------------------------------------

ui <- bslib::page_fluid(
  theme = bslib::bs_theme(version = 5),
  tags$head(tags$style(HTML(custom_css))),
  tags$div(
    class = "app-header",
    tags$h2("Flood Intelligence Tool"),
    uiOutput("health_badge")
  ),
  
  tabsetPanel(
    id = "main_tabs",
    
    tabPanel("FGS Interrogator",
             fluidRow(
               column(2, sidebar),
               column(7, centre),
               column(3, right_panel)
             )
    ),
    
    tabPanel("Constituency situation", situation_tab),
    tabPanel("Exposure history",       exposure_tab),

    # Placeholders. These need data the pipeline does not hold yet
    # (observed gauge levels, the Met Office licence decision).
    tabPanel("Warning issue times",   tags$p("Coming soon.")),
    tabPanel("Forecast verification", tags$p("Coming soon.")),
    tabPanel("Event playback",        tags$p("Coming soon.")),
    tabPanel("MP audit log",          tags$p("Coming soon."))
  )
)
