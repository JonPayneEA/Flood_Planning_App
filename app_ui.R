# =============================================================================
# app_ui.R
# Declarative UI for the Flood Intelligence Tool dashboard.
#
# Three-column layout for the Live view tab: a left sidebar of controls, a
# centre column holding the map and a small title block, and a right column
# of count cards and the affected-areas list. The other five tabs are
# placeholders we'll fill in later.
#
# This file produces no reactive output itself -- every textOutput, uiOutput,
# and leafletOutput slot is filled by callbacks in app_server.R.
# =============================================================================


# -----------------------------------------------------------------------------
# matrix_filter_ui()
#
# Builds the 4x4 FGS risk matrix as a CSS grid of styled cells, each holding
# a checkbox. Each checkbox is given inputId 'mtx_{x}_{y}' so app_server.R can
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
      # Hover text spells the cell out: both axes, the band and the risk level.
      title    = sprintf("Impact: %s | Likelihood: %s (%s) | Risk: %s",
                         IMPACT_LABELS[cell$x], LIKELIHOOD_LABELS[cell$y],
                         LIKELIHOOD_BANDS[cell$y], RISK_LEVEL_BY_COLOUR[[cell$colour]]),
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
      tags$div(class = "name", LIKELIHOOD_LABELS[y]),
      tags$div(class = "band", LIKELIHOOD_BANDS[y])
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

body { background: var(--ea-bg); font-size: 14px; }

.shiny-sidebar { background: var(--ea-tint); padding: 20px; height: 100%; }

.shiny-sidebar h6 {
  font-size: 11px; text-transform: uppercase; letter-spacing: 0.05em;
  color: var(--ea-muted); margin-top: 16px; margin-bottom: 8px;
}
.shiny-sidebar h6:first-child { margin-top: 0; }

.panel-section {
  background: var(--ea-surface); border: 1px solid var(--ea-line); border-radius: 4px;
  padding: 14px; margin-bottom: 12px;
}
.panel-h {
  font-size: 12px; text-transform: uppercase; letter-spacing: 0.05em;
  color: var(--ea-muted); padding-bottom: 6px; border-bottom: 1px solid var(--ea-tint);
}

/* --- 2. Stat cards -------------------------------------------------------- */

div.stat-card {
  background: var(--ea-surface);
  border: 1px solid var(--ea-line);
  border-radius: 4px;
  padding: 12px;
  text-align: center;
  /* Explicit display block so this can't ever be picked up as an inline-block
     or button by an over-eager Bootstrap rule. */
  display: block;
  cursor: default;
}
div.stat-card .stat-num { font-size: 24px; font-weight: 700; line-height: 1; color: var(--ea-ink); }
div.stat-card .stat-num.accent { color: var(--ea-green-dark); }
div.stat-card .stat-label {
  font-size: 10px; text-transform: uppercase; letter-spacing: 0.05em;
  color: var(--ea-muted); margin-top: 4px;
}

/* --- Affected-area list rows --------------------------------------------- */

.area-row {
  display: flex; align-items: center; padding: 6px 0;
  border-bottom: 1px solid var(--ea-tint); font-size: 13px;
  cursor: pointer;
}
.area-row:last-child { border-bottom: none; }
.area-row:hover { background: var(--ea-bg); }
.risk-bar { width: 4px; height: 22px; margin-right: 10px; border-radius: 1px; }
.risk-bar.red    { background: #b02020; }
.risk-bar.amber  { background: #e08020; }
.risk-bar.yellow { background: #f0d040; }
.risk-bar.green  { background: #4f9d4f; }

/* --- 3. Risk matrix grid -------------------------------------------------- */

.risk-matrix-wrap {
  display: grid !important;
  grid-template-columns: 16px 76px repeat(4, 36px) !important;
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
  font-size: 10px; color: var(--ea-muted); text-align: left;
  transform: rotate(35deg);
  transform-origin: bottom left;
  white-space: nowrap;
  position: relative;
  z-index: 10;
  align-self: start;
}
.matrix-row-header { font-size: 10px; color: var(--ea-muted); text-align: right; padding-right: 4px; line-height: 1.15; }
.matrix-row-header .band { font-size: 9px; color: var(--ea-muted); }

.matrix-likelihood-label {
  writing-mode: vertical-rl;
  transform: rotate(180deg);
  text-align: center;
  font-size: 10px;
  font-weight: 600;
  color: var(--ea-muted);
}
.matrix-impact-label {
  text-align: center;
  font-size: 10px;
  font-weight: 600;
  color: var(--ea-muted);
}

/* --- 4. Quiet-state notice ------------------------------------------------ */

/* Shown above the map when the current statement-day has no risk polygons.
   A neutral light-blue rather than warning-amber, because this is a normal
   operational state. */
.quiet-notice {
  background: var(--ea-tint);
  border: 1px solid var(--ea-green-mid);
  color: var(--ea-ink);
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
  border: 4px solid var(--ea-line);
  border-top-color: var(--ea-green-dark);
  border-radius: 50%;
  animation: map-spin 0.8s linear infinite;
}
@keyframes map-spin {
  to { transform: rotate(360deg); }
}

/* --- 6. Headline notice & statement info boxes ----------------------------- */

.headline-notice {
  background: var(--ea-surface);
  border: 1px solid var(--ea-line);
  border-left: 4px solid var(--ea-line);  /* colour overridden inline from risk keyword */
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
  background: var(--ea-surface);
  border: 1px solid var(--ea-line);
  border-left: 4px solid var(--ea-line);  /* width fixed; colour overridden inline per card */
  border-radius: 4px;
  padding: 10px 14px;
}
.source-info-label {
  font-size: 10px;
  text-transform: uppercase;
  letter-spacing: 0.05em;
  color: var(--ea-muted);
  margin-bottom: 4px;
}
.source-info-value {
  font-size: 13px;
  color: var(--ea-ink);
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
.health-unknown { color: var(--ea-muted); border-color: var(--ea-line-strong); background: var(--ea-tint); }
.health-unknown .dot { background: var(--ea-line-strong); }

.changes-strip {
  background: var(--ea-surface); border: 1px solid var(--ea-line); border-radius: 6px;
  padding: 8px 12px; margin-bottom: 12px; font-size: 13px;
}
.changes-strip summary { cursor: pointer; }
.changes-strip .chg-count { font-weight: 600; }
.changes-strip .chg-up   { color: #b02020; }
.changes-strip .chg-down { color: #1e6b1e; }
.changes-strip table { width: 100%; margin-top: 8px; font-size: 12px; }
.changes-strip td, .changes-strip th { padding: 2px 6px; }
.changes-strip .chg-scroll { max-height: 340px; overflow-y: auto; }

/* --- 6. Rank chips, exposure grid, briefing text ------------------------ */

.rank-chip {
  display: inline-block; min-width: 54px; text-align: center;
  padding: 1px 6px; border-radius: 3px; font-size: 11px; color: var(--ea-ink);
}
.exposure-grid { border-collapse: collapse; font-size: 12px; }
.exposure-grid th { font-weight: 500; padding: 2px 6px; text-align: center; white-space: nowrap; }
.exposure-grid td.name { text-align: left; padding: 2px 10px 2px 0; white-space: nowrap; }
.exposure-grid td.cell { min-width: 40px; height: 24px; text-align: center; border: 2px solid var(--ea-bg); }
.exposure-grid td.total { padding-left: 10px; font-weight: 600; }
/* --- 7. Header group, changes overlay, KPI row, legends ------------------ */

.stale-banner {
  display: flex; align-items: center; gap: 8px; margin: 0 15px 8px 15px;
  padding: 8px 12px; border-radius: 6px; font-size: 13px; border: 1px solid;
}
.stale-bad  { background: #f9e5e5; border-color: #b02020; color: #6b1212; }
.stale-warn { background: #fdf1e2; border-color: #e08020; color: #6b4300; }
.gaps-banner {
  margin: 0 15px 8px 15px; padding: 8px 12px; border-radius: 6px; font-size: 13px;
  background: var(--ea-tint); border: 1px solid var(--ea-green-mid); color: var(--ea-ink);
}
.stale-banner .btn-link { margin-left: auto; padding: 0; font-size: 13px; }

.header-right { display: flex; align-items: center; gap: 12px; }
.asof { font-size: 12px; color: var(--ea-muted); }

.changes-wrap { position: relative; height: 42px; margin-bottom: 10px; z-index: 1100; }
.changes-wrap .changes-strip {
  position: absolute; top: 0; left: 0; right: 0; margin: 0;
  box-shadow: 0 2px 8px rgba(0,0,0,0.12);
}
.changes-strip .chg-row { cursor: pointer; }
.changes-strip .chg-row:hover { background: var(--ea-tint); }
.changes-strip .chg-group td {
  font-size: 10px; text-transform: uppercase; letter-spacing: 0.05em;
  color: var(--ea-muted); padding-top: 8px;
}
.changes-strip .chg-hint { font-size: 11px; color: var(--ea-muted); margin-top: 4px; }
.changes-strip .chg-more { color: var(--ea-muted); font-size: 11px; }

.kpi-row { display: flex; gap: 12px; flex-wrap: wrap; margin-bottom: 14px; }
.kpi-row .stat-card { flex: 1 1 180px; margin: 0; }

.legend-row { display: flex; align-items: center; gap: 6px; flex-wrap: wrap;
              font-size: 11px; color: var(--ea-muted); margin: 8px 0; }
.legend-bar { width: 160px; height: 10px; border-radius: 2px;
              background: linear-gradient(to right, #fff3bf, #e08020, #b02020); }

.filter-row { display: flex; gap: 16px; align-items: flex-end; flex-wrap: wrap; margin-bottom: 8px; }
.filter-row .form-group { margin-bottom: 0; }
.filter-row .checkbox { margin: 0 0 6px 0; }

.exposure-grid { width: 100%; }
.exposure-grid tbody tr { cursor: pointer; }
.exposure-grid tbody tr:hover td.name { text-decoration: underline; }
.exp-bar { height: 8px; background: #e08020; border-radius: 2px; }

.empty-state {
  padding: 32px; text-align: center; color: var(--ea-muted); background: var(--ea-surface);
  border: 1px dashed var(--ea-line-strong); border-radius: 6px;
}

/* Planned tabs sit in a quiet dropdown rather than as four dead tabs. */
.nav-tabs .dropdown-toggle { color: var(--ea-muted); }

.briefing-body {
  white-space: pre-wrap; background: var(--ea-surface); border: 1px solid var(--ea-line);
  border-radius: 6px; padding: 12px; font-size: 13px;
}

/* --- 8. Environment Agency frame (colours from theme.R) ------------------- */

body { font-family: Arial, 'Helvetica Neue', Helvetica, sans-serif; color: var(--ea-ink); }
a { color: var(--ea-blue-dark); }

/* Full-bleed header band. */
.app-header {
  background: var(--ea-green-dark); color: var(--ea-surface);
  margin: 0 calc(-0.5 * var(--bs-gutter-x, 1.5rem)) 0 calc(-0.5 * var(--bs-gutter-x, 1.5rem));
  padding: 14px calc(0.5 * var(--bs-gutter-x, 1.5rem));
  border-bottom: 4px solid var(--ea-green);
}
.app-header h2 { color: var(--ea-surface); font-weight: 700; letter-spacing: 0.01em; }
.app-header .app-sub { font-size: 12px; color: rgba(255,255,255,0.85); margin-top: 2px; }
.app-header .asof { color: rgba(255,255,255,0.88); }
.app-header .btn-outline-secondary {
  color: var(--ea-surface); border-color: rgba(255,255,255,0.7); background: transparent;
}
.app-header .btn-outline-secondary:hover { background: rgba(255,255,255,0.14); color: var(--ea-surface); }
.ea-logo { height: 36px; margin-right: 14px; }
.header-left { display: flex; align-items: center; }

/* Tabs: green rule on the active tab. */
.nav-tabs { border-bottom: 1px solid var(--ea-line); margin-top: 10px; }
.nav-tabs .nav-link { color: var(--ea-blue-dark); border: 0; border-top: 3px solid transparent; border-radius: 0; }
.nav-tabs .nav-link.active {
  color: var(--ea-ink); background: var(--ea-surface); font-weight: 700;
  border-top: 3px solid var(--ea-green); border-left: 1px solid var(--ea-line); border-right: 1px solid var(--ea-line);
}

/* Buttons and focus. */
.btn-outline-secondary { color: var(--ea-green-dark); border-color: var(--ea-green-dark); }
.btn-outline-secondary:hover { background: var(--ea-green-dark); color: var(--ea-surface); border-color: var(--ea-green-dark); }
.btn-primary { background: var(--ea-green-dark); border-color: var(--ea-green-dark); }
:focus-visible { outline: 3px solid var(--ea-blue); outline-offset: 2px; }

/* Cards and panels sit on white with a hairline. */
div.stat-card, .panel-section, .changes-strip, .briefing-body, .empty-state {
  background: var(--ea-surface); border-color: var(--ea-line);
}
.shiny-sidebar { background: var(--ea-tint); }
table.dataTable thead th { color: var(--ea-ink); border-bottom: 2px solid var(--ea-green); }
.dataTables_wrapper .dataTables_paginate .paginate_button.current {
  background: var(--ea-green-dark) !important; color: var(--ea-surface) !important; border-color: var(--ea-green-dark) !important;
}
.exposure-grid td.name { color: var(--ea-ink); }
.irs--shiny .irs-bar, .irs--shiny .irs-handle { background: var(--ea-green-dark); border-color: var(--ea-green-dark); }
.irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to { background: var(--ea-green-dark); }
.form-check-input:checked { background-color: var(--ea-green-dark); border-color: var(--ea-green-dark); }
.stat-card .stat-num.accent { color: var(--ea-green-dark); }

/* Demonstration ribbon and controls (demo mode only). */
.demo-ribbon {
  background: var(--ea-ink); color: var(--ea-surface); font-size: 12px; font-weight: 700;
  letter-spacing: 0.06em; text-transform: uppercase; text-align: center; padding: 5px 12px;
  margin: 0 calc(-0.5 * var(--bs-gutter-x, 1.5rem));
}
.demo-controls {
  display: flex; gap: 18px; align-items: flex-end; flex-wrap: wrap; font-size: 13px;
  background: var(--ea-tint); border-bottom: 1px solid var(--ea-line);
  padding: 8px calc(0.5 * var(--bs-gutter-x, 1.5rem));
  margin: 0 calc(-0.5 * var(--bs-gutter-x, 1.5rem));
}
.demo-controls .form-group, .demo-controls .checkbox { margin: 0; }
.demo-controls label { font-size: 12px; color: var(--ea-muted); }
.demo-note { font-size: 12px; color: var(--ea-muted); }
.poc-tag { font-size: 10px; font-weight: 700; letter-spacing: 0.05em; text-transform: uppercase; color: var(--ea-ink);
           background: var(--ea-green-mid); border-radius: 8px; padding: 1px 7px; margin-left: 4px; }

/* Overview. */
.ov-headline { background: var(--ea-surface); border: 1px solid var(--ea-line); border-left: 6px solid var(--ea-green);
               border-radius: 4px; padding: 14px 18px; margin-bottom: 14px; }
.ov-headline .ov-kicker { font-size: 11px; text-transform: uppercase; letter-spacing: 0.06em; color: var(--ea-muted); }
.ov-headline .ov-text { font-size: 19px; line-height: 1.35; margin-top: 4px; }
.ov-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(190px, 1fr)); gap: 12px; margin-bottom: 14px; }
.ov-grid .stat-card { margin: 0; }
.ov-outlook { display: grid; grid-template-columns: repeat(5, 1fr); gap: 8px; margin-bottom: 16px; }
.ov-day { background: var(--ea-surface); border: 1px solid var(--ea-line); border-radius: 4px; padding: 10px; text-align: center; }
.ov-day .d { font-size: 12px; color: var(--ea-muted); }
.ov-day .n { font-size: 12px; margin-top: 6px; color: var(--ea-ink); }
.ov-day .rank-chip { margin-top: 6px; min-width: 70px; }
.ov-cols { display: grid; grid-template-columns: 3fr 2fr; gap: 14px; }
@media (max-width: 992px) { .ov-cols, .ov-outlook { grid-template-columns: 1fr; } }
.ov-panel { background: var(--ea-surface); border: 1px solid var(--ea-line); border-radius: 4px; padding: 12px 14px; }
.ov-panel h6 { margin: 0 0 8px 0; font-size: 12px; text-transform: uppercase; letter-spacing: 0.05em; color: var(--ea-muted); }
.ov-table { width: 100%; font-size: 13px; }
.ov-table td, .ov-table th { padding: 4px 6px; border-bottom: 1px solid var(--ea-line); }
.ov-table tr.clickable { cursor: pointer; }
.ov-table tr.clickable:hover { background: var(--ea-tint); }
.ov-foot { font-size: 12px; color: var(--ea-muted); margin-top: 10px; }
"


# -----------------------------------------------------------------------------
# Sidebar (left column).
#
# Controls grouped by purpose: layers, source, matrix, threshold, day. The
# order matters -- forecasters tend to set layers first, then narrow by
# source, then matrix cell, then numeric thresholds. The UI reads top-down
# in that order of decreasing frequency-of-change.
# -----------------------------------------------------------------------------

make_sidebar <- function() tags$div(
  class = "shiny-sidebar",
  
  h6("FGS statement"),
  # Choices populated server-side once fetch_recent_statements() has run.
  # Display labels show issue date/time; values are statement_ids so the
  # filters() reactive in app_server.R picks the right rows.
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
  checkboxInput("layer_const_risk",
                "Constituency forecast risk (latest statement, worst of 5 days)",
                value = FALSE),

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

make_centre <- function() tags$div(
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

make_right_panel <- function() tags$div(
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
  
  # Affected-areas list (app_server.R builds the row markup).
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

make_situation_tab <- function() tags$div(
  class = "p-3",
  uiOutput("situation_note"),
  uiOutput("situation_kpis"),
  tags$div(
    class = "filter-row",
    # POC-ONLY: statement look-back. Remove this control with the other POC-ONLY blocks.
    if (POC_FEATURES) selectInput("sit_statement", tagList("Statement ", tags$span(class = "poc-tag", "POC only")),
                                  choices = c("Latest (live table)" = "latest"), selected = "latest", width = "270px"),
    selectInput("sit_min_rank", "Minimum forecast risk",
                choices  = c("Any" = "0", "Very Low" = "1", "Low" = "2",
                             "Medium" = "3", "High" = "4"),
                selected = "3", width = "180px"),
    textInput("sit_search", "Search", placeholder = "Constituency or MP", width = "220px"),
    checkboxInput("sit_escalated", "Escalated only", FALSE),
    checkboxInput("sit_warning", "Warning or alert in force", FALSE),
    downloadButton("download_situation", "Export CSV",
                   class = "btn-sm btn-outline-secondary")
  ),
  DT::dataTableOutput("situation_table"),
  uiOutput("rank_legend"),
  tags$hr(),
  uiOutput("briefing_panel")
)


# -----------------------------------------------------------------------------
# Exposure history tab.
#
# Heat grid from fgs_constituency_exposure (job 21): the constituencies with
# most days at or above a chosen risk over the last 12 months, by month.
# -----------------------------------------------------------------------------

make_exposure_tab <- function() tags$div(
  class = "p-3",
  fluidRow(
    column(3, selectInput("exp_source", "Flood source",
                          choices  = c("Any source" = "any", "River" = "river",
                                       "Surface" = "surface", "Coastal" = "coastal",
                                       "Groundwater" = "ground"),
                          selected = "any")),
    column(3, selectInput("exp_metric", "Measure",
                          choices  = c("Days at Medium or above" = "days_medium_or_above",
                                       "Days at any flagged risk" = "days_flagged",
                                       "Share of days covered at Medium or above" = "share"),
                          selected = "days_medium_or_above")),
    column(3, sliderInput("exp_top_n", "Constituencies shown",
                          min = 10, max = 50, value = 25, step = 5))
  ),
  tags$small(class = "text-muted d-block mb-2",
             "Counts one day per date (day 1 of the last statement issued that date). ",
             "A blank cell means no flagged day that month, not missing data. Click a row for its history. ",
             "Days with no statement are not counted as quiet."),
  uiOutput("exposure_legend"),
  uiOutput("exposure_grid")
)


# -----------------------------------------------------------------------------
# Overview tab.
#
# The one-screen summary for senior forecasters and managers: what the latest
# statement says, how many constituencies are at risk, the five-day outlook,
# who to look at first and what has changed. Everything here comes from tables
# the app already reads, so it costs no extra queries. The map and the full
# tables sit behind it, one click away.
# -----------------------------------------------------------------------------

make_overview_tab <- function() tags$div(
  class = "p-3",
  uiOutput("ov_headline"),
  uiOutput("ov_kpis"),
  tags$div(class = "ov-panel mb-3",
           tags$h6("Five-day outlook: constituencies at Medium risk or above"),
           uiOutput("ov_outlook")),
  tags$div(
    class = "ov-cols",
    tags$div(class = "ov-panel",
             tags$h6("Constituencies to watch"),
             uiOutput("ov_watch")),
    tags$div(class = "ov-panel",
             tags$h6("Changed since the previous statement"),
             uiOutput("ov_changes"))
  ),
  uiOutput("ov_foot")
)


# -----------------------------------------------------------------------------
# MP audit log tab (demo mode only).
#
# Shows the review actions taken on briefing drafts in THIS session. It is a
# demonstration of the workflow the live app does not have yet. Nothing is
# saved, and nothing is sent or written to any warehouse.
# -----------------------------------------------------------------------------

make_audit_tab <- function() tags$div(
  class = "p-3",
  tags$div(class = "alert alert-secondary",
           tags$strong("Demonstration only. "),
           "This log is held in your browser session and disappears when you close the page. ",
           "Nothing is saved, sent or written to any warehouse. In the live app, briefings are read-only ",
           "until a decision is made on who may record a review."),
  DT::dataTableOutput("audit_table")
)


# -----------------------------------------------------------------------------
# Demo ribbon and controls (demo mode only).
#
# The ribbon is always visible so a screenshot of the demo can never be mistaken
# for live data.
# -----------------------------------------------------------------------------

make_demo_controls <- function() {
  tags$div(
    class = "demo-controls",
    selectInput("demo_scenario", "Scenario", choices = DEMO_SCENARIOS, selected = "developing", width = "200px"),
    checkboxInput("demo_problem", "Simulate a pipeline problem", FALSE),
    actionButton("demo_reset", "Reset demo actions", class = "btn-sm btn-outline-secondary"),
    tags$span(class = "demo-note",
              "People, places and rivers are fictional. The map is an illustrative hexagon layout, not real boundaries.")
  )
}


# -----------------------------------------------------------------------------
# Top-level UI.
#
# A function of the request, and every section above is a function too, so the
# widgets are built per request. That is what lets a bookmarked URL restore
# the tab, statement, day and filters: Shiny only applies a restored value
# when the widget is created while the request is being served.
# -----------------------------------------------------------------------------

ui <- function(request) {
  tabs <- list(
    tabPanel("Overview", make_overview_tab()),

    tabPanel("FGS Interrogator",
             # Stacks on small screens: sidebar and map first, counts below.
             tags$div(
               class = "row",
               tags$div(class = "col-12 col-md-4 col-xl-3", make_sidebar()),
               tags$div(class = "col-12 col-md-8 col-xl-6", make_centre()),
               tags$div(class = "col-12 col-xl-3",          make_right_panel())
             )
    ),

    tabPanel("Constituency situation", make_situation_tab()),
    tabPanel("Exposure history",       make_exposure_tab()),
    if (DEMO_MODE) tabPanel("MP audit log", make_audit_tab()),

    # Planned. These need data the pipeline does not hold yet (observed gauge
    # levels, the Met Office licence decision), so they sit in one menu.
    do.call(navbarMenu, c(list("Planned"), Filter(Negate(is.null), list(
      tabPanel("Warning issue times",   tags$p(class = "p-3 text-muted", "Planned. Needs warning issue history.")),
      tabPanel("Forecast verification", tags$p(class = "p-3 text-muted", "Planned. Needs observed gauge levels and a decision on the Met Office licence.")),
      tabPanel("Event playback",        tags$p(class = "p-3 text-muted", "Planned. Needs observed gauge levels and a decision on the Met Office licence.")),
      if (!DEMO_MODE) tabPanel("MP audit log", tags$p(class = "p-3 text-muted", "Planned. Needs a record of who sent what."))
    ))))
  )

  bslib::page_fluid(
    theme = ea_theme(),
    tags$head(tags$title(if (DEMO_MODE) "Flood Intelligence Tool (demonstration)" else "Flood Intelligence Tool"),
              tags$style(HTML(paste(ea_css_variables(), custom_css)))),
    if (DEMO_MODE) tags$div(class = "demo-ribbon", "Demonstration: synthetic data. Nothing is connected, saved or sent."),
    tags$div(
      class = "app-header",
      tags$div(class = "header-left",
               ea_logo(),
               tags$div(tags$h2("Flood Intelligence Tool"),
                        tags$div(class = "app-sub", "Flood Guidance Statement: forecast, constituencies and flood areas"))),
      tags$div(class = "header-right",
               uiOutput("data_asof"),
               tags$button(
                 type = "button", id = "copy_link", class = "btn btn-sm btn-outline-secondary",
                 title = "Copy a link to this exact view",
                 onclick = paste0(
                   "var b=this,t=b.innerHTML;",
                   "if(navigator.clipboard){navigator.clipboard.writeText(window.location.href).then(function(){",
                   "b.textContent='Copied';setTimeout(function(){b.innerHTML=t;},1500);});}",
                   "else{window.prompt('Copy this link',window.location.href);}"),
                 icon("link"), " Copy link"),
               uiOutput("health_badge"))
    ),
    if (DEMO_MODE) make_demo_controls(),
    # Shown only when the pipeline is failing, stale or unreadable.
    uiOutput("stale_banner"),
    # Shown only when a pipeline table has not been built yet.
    uiOutput("data_gaps_banner"),

    do.call(tabsetPanel, c(list(id = "main_tabs"), Filter(Negate(is.null), tabs)))
  )
}
