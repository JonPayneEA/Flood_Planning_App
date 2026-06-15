# =============================================================================
# ui.R
# Declarative UI for the FGS Flood Guidance dashboard.
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
  
  # Build a list of styled div+checkbox elements, one per cell in the matrix.
  # lapply over row indices because data.table's row iteration is by-index;
  # this stays explicit about what we're iterating over.
  cells <- lapply(seq_len(nrow(RISK_MATRIX)), function(i) {
    
    cell <- RISK_MATRIX[i]                          # one-row data.table
    default_ticked <- cell$colour %in% c("amber", "red")
    
    tags$div(
      class    = paste("matrix-cell", cell$colour),  # e.g. "matrix-cell red"
      `data-x` = cell$x,                             # data-* attrs for any
      `data-y` = cell$y,                             # future JS hook
      checkboxInput(
        inputId = paste0("mtx_", cell$x, "_", cell$y),
        label   = NULL,
        value   = default_ticked
      )
    )
  })
  
  # tagList holds a label, a hint, the grid itself, and an axis caption.
  # do.call(tags$div, ...) is the standard way to splat a list of children
  # into a single parent tag.
  tagList(
    h6("Risk matrix position"),
    tags$small("Tick cells to include - counts update live",
               class = "text-muted d-block mb-2"),
    do.call(tags$div, c(list(class = "risk-matrix-grid"), cells)),
    tags$div("up = Likelihood   .   Impact ->", class = "matrix-axis-label")
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
}
.area-row:last-child { border-bottom: none; }
.risk-bar { width: 4px; height: 22px; margin-right: 10px; border-radius: 1px; }
.risk-bar.Red    { background: #b02020; }
.risk-bar.Amber  { background: #e08020; }
.risk-bar.Yellow { background: #f0d040; }
.risk-bar.Green  { background: #4f9d4f; }

/* --- 3. Risk matrix grid -------------------------------------------------- */

.risk-matrix-grid {
  display: grid !important;
  grid-template-columns: repeat(4, 36px) !important;
  grid-auto-rows: 36px !important;
  gap: 2px;
  margin-bottom: 4px;
  width: max-content !important;
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

.matrix-axis-label {
  font-size: 10px; color: #495057; text-align: center; margin-top: 4px;
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
  
  h6("Map layers"),
  checkboxInput("layer_polygons",       "FGS risk polygons", value = TRUE),
  checkboxInput("layer_ea_areas",       "EA flood areas",    value = TRUE),
  checkboxInput("layer_constituencies", "Constituencies",    value = FALSE),
  
  # Sub-control for the EA layer: pick which area types appear. Indented
  # visually so it reads as a child of the EA layer toggle.
  conditionalPanel(
    condition = "input.layer_ea_areas == true",
    tags$div(
      style = "margin-left: 16px; margin-top: -4px;",
      checkboxInput("ea_type_fwa", "Flood Warning Areas", value = TRUE),
      checkboxInput("ea_type_faa", "Flood Alert Areas",   value = TRUE)
    )
  ),
  
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
  # Quiet-state notice. Shown when there are no FGS polygons for the current
  # statement-day. A quiet FGS is operationally normal -- this is an
  # expected state, not an error.
  uiOutput("quiet_notice"),
  leaflet::leafletOutput("live_map", height = "500px")
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
             tags$div("EA areas affected", class = "stat-label")),
    
    tags$div(class = "stat-card mt-2",
             tags$div(textOutput("count_constituencies", inline = TRUE),
                      class = "stat-num"),
             tags$div("Constituencies", class = "stat-label"))
  ),
  
  # Affected-areas list (server.R builds the row markup).
  tags$div(
    class = "panel-section",
    h6("EA areas affected", class = "panel-h"),
    uiOutput("ea_area_list")
  )
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
  titlePanel("FGS Flood Guidance"),
  
  tabsetPanel(
    id = "main_tabs",
    
    tabPanel("Live view",
             fluidRow(
               column(2, sidebar),
               column(7, centre),
               column(3, right_panel)
             )
    ),
    
    # Placeholders.
    tabPanel("Warning issue times",   tags$p("Coming soon.")),
    tabPanel("Forecast verification", tags$p("Coming soon.")),
    tabPanel("Event playback",        tags$p("Coming soon.")),
    tabPanel("Coverage gaps",         tags$p("Coming soon.")),
    tabPanel("MP audit log",          tags$p("Coming soon."))
  )
)
