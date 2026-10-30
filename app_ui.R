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

body { background: var(--app-bg); font-size: 14px; }

.shiny-sidebar { background: var(--app-tint); padding: 20px; height: 100%; }

.shiny-sidebar h6 {
  font-size: 11px; text-transform: uppercase; letter-spacing: 0.05em;
  color: var(--app-muted); margin-top: 16px; margin-bottom: 8px;
}
.shiny-sidebar h6:first-child { margin-top: 0; }

.panel-section {
  background: var(--app-surface); border: 1px solid var(--app-line); border-radius: 4px;
  padding: 14px; margin-bottom: 12px;
}
.panel-h {
  font-size: 12px; text-transform: uppercase; letter-spacing: 0.05em;
  color: var(--app-muted); padding-bottom: 6px; border-bottom: 1px solid var(--app-tint);
}

/* --- 2. Stat cards -------------------------------------------------------- */

div.stat-card {
  background: var(--app-surface);
  border: 1px solid var(--app-line);
  border-radius: 4px;
  padding: 12px;
  text-align: center;
  /* Explicit display block so this can't ever be picked up as an inline-block
     or button by an over-eager Bootstrap rule. */
  display: block;
  cursor: default;
}
div.stat-card .stat-num { font-size: 24px; font-weight: 700; line-height: 1; color: var(--app-ink); }
div.stat-card .stat-num.accent { color: var(--app-blue); }
div.stat-card .stat-label {
  font-size: 10px; text-transform: uppercase; letter-spacing: 0.05em;
  color: var(--app-muted); margin-top: 4px;
}

/* --- Affected-area list rows --------------------------------------------- */

.area-row {
  display: flex; align-items: center; padding: 6px 0;
  border-bottom: 1px solid var(--app-tint); font-size: 13px;
  cursor: pointer;
}
.area-row:last-child { border-bottom: none; }
.area-row:hover { background: var(--app-bg); }
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
  font-size: 10px; color: var(--app-muted); text-align: left;
  transform: rotate(35deg);
  transform-origin: bottom left;
  white-space: nowrap;
  position: relative;
  z-index: 10;
  align-self: start;
}
.matrix-row-header { font-size: 10px; color: var(--app-muted); text-align: right; padding-right: 4px; line-height: 1.15; }
.matrix-row-header .band { font-size: 9px; color: var(--app-muted); }

.matrix-likelihood-label {
  writing-mode: vertical-rl;
  transform: rotate(180deg);
  text-align: center;
  font-size: 10px;
  font-weight: 600;
  color: var(--app-muted);
}
.matrix-impact-label {
  text-align: center;
  font-size: 10px;
  font-weight: 600;
  color: var(--app-muted);
}

/* --- 4. Quiet-state notice ------------------------------------------------ */

/* Shown above the map when the current statement-day has no risk polygons.
   A neutral light-blue rather than warning-amber, because this is a normal
   operational state. */
.quiet-notice {
  background: var(--app-tint);
  border: 1px solid var(--app-ice);
  color: var(--app-ink);
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
  border: 4px solid var(--app-line);
  border-top-color: var(--app-navy);
  border-radius: 50%;
  animation: map-spin 0.8s linear infinite;
}
@keyframes map-spin {
  to { transform: rotate(360deg); }
}

/* --- 6. Headline notice & statement info boxes ----------------------------- */

.headline-notice {
  background: var(--app-surface);
  border: 1px solid var(--app-line);
  border-left: 4px solid var(--app-line);  /* colour overridden inline from risk keyword */
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
  background: var(--app-surface);
  border: 1px solid var(--app-line);
  border-left: 4px solid var(--app-line);  /* width fixed; colour overridden inline per card */
  border-radius: 4px;
  padding: 10px 14px;
}
.source-info-label {
  font-size: 10px;
  text-transform: uppercase;
  letter-spacing: 0.05em;
  color: var(--app-muted);
  margin-bottom: 4px;
}
.source-info-value {
  font-size: 13px;
  color: var(--app-ink);
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
.health-unknown { color: var(--app-muted); border-color: var(--app-line-strong); background: var(--app-tint); }
.health-unknown .dot { background: var(--app-line-strong); }

.changes-strip {
  background: var(--app-surface); border: 1px solid var(--app-line); border-radius: 6px;
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
  padding: 1px 6px; border-radius: 3px; font-size: 11px; color: var(--app-ink);
}
.exposure-grid { border-collapse: collapse; font-size: 12px; }
.exposure-grid th { font-weight: 500; padding: 2px 6px; text-align: center; white-space: nowrap; }
.exposure-grid td.name { text-align: left; padding: 2px 10px 2px 0; white-space: nowrap; }
.exposure-grid td.cell { min-width: 40px; height: 24px; text-align: center; border: 2px solid var(--app-bg); }
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
  background: var(--app-tint); border: 1px solid var(--app-ice); color: var(--app-ink);
}
.stale-banner .btn-link { margin-left: auto; padding: 0; font-size: 13px; }

.header-right { display: flex; align-items: center; gap: 12px; }
.asof { font-size: 12px; color: var(--app-muted); }

.changes-wrap { position: relative; height: 42px; margin-bottom: 10px; z-index: 1100; }
.changes-wrap .changes-strip {
  position: absolute; top: 0; left: 0; right: 0; margin: 0;
  box-shadow: 0 2px 8px rgba(0,0,0,0.12);
}
.changes-strip .chg-row { cursor: pointer; }
.changes-strip .chg-row:hover { background: var(--app-tint); }
.changes-strip .chg-group td {
  font-size: 10px; text-transform: uppercase; letter-spacing: 0.05em;
  color: var(--app-muted); padding-top: 8px;
}
.changes-strip .chg-hint { font-size: 11px; color: var(--app-muted); margin-top: 4px; }
.changes-strip .chg-more { color: var(--app-muted); font-size: 11px; }

.kpi-row { display: flex; gap: 12px; flex-wrap: wrap; margin-bottom: 14px; }
.kpi-row .stat-card { flex: 1 1 180px; margin: 0; }

.legend-row { display: flex; align-items: center; gap: 6px; flex-wrap: wrap;
              font-size: 11px; color: var(--app-muted); margin: 8px 0; }
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
  padding: 32px; text-align: center; color: var(--app-muted); background: var(--app-surface);
  border: 1px dashed var(--app-line-strong); border-radius: 6px;
}

/* Planned tabs sit in a quiet dropdown rather than as four dead tabs. */
.nav-tabs .dropdown-toggle { color: var(--app-muted); }

.briefing-body {
  white-space: pre-wrap; background: var(--app-surface); border: 1px solid var(--app-line);
  border-radius: 6px; padding: 12px; font-size: 13px;
}

/* --- 8. Brand frame (colours from theme.R) --------------------------------- */

body { font-family: Arial, 'Helvetica Neue', Helvetica, sans-serif; color: var(--app-ink); }
a { color: var(--app-blue); }

/* Full-bleed header band. */
.app-header {
  background: linear-gradient(135deg, var(--app-navy) 0%, var(--app-navy) 55%, var(--app-navy-deep) 100%);
  color: var(--app-surface); position: relative; overflow: hidden;
  margin: 0 calc(-0.5 * var(--bs-gutter-x, 1.5rem)) 0 calc(-0.5 * var(--bs-gutter-x, 1.5rem));
  padding: 14px calc(0.5 * var(--bs-gutter-x, 1.5rem));
  border-bottom: 4px solid var(--app-sky);
}
/* A soft wave along the foot of the header, echoing the logo. Decorative. */
.app-header::after {
  content: ''; position: absolute; left: 0; right: 0; bottom: -1px; height: 18px; opacity: 0.9; pointer-events: none;
  background: url(data:image/svg+xml,%3Csvg%20xmlns=%27http://www.w3.org/2000/svg%27%20viewBox=%270%200%201200%2018%27%20preserveAspectRatio=%27none%27%3E%3Cpath%20d=%27M0%2012%20C150%200%20300%2018%20450%209%20S750%200%20900%2010%20S1100%2014%201200%206%20V18%20H0Z%27%20fill=%27%230161BD%27%20fill-opacity=%27.45%27/%3E%3C/svg%3E) bottom / 100% 100% no-repeat;
}
.app-header h2 { color: var(--app-surface); font-weight: 800; letter-spacing: 0.01em; }
.app-header h2 .hl { color: var(--app-cyan); }
.app-header .app-sub { font-size: 12px; color: var(--app-ice); margin-top: 2px; }
.app-header .asof { color: rgba(255,255,255,0.88); }
.app-header .btn-outline-secondary {
  color: var(--app-surface); border-color: rgba(255,255,255,0.7); background: transparent;
}
.app-header .btn-outline-secondary:hover { background: rgba(255,255,255,0.14); color: var(--app-surface); }
.brand-mark { height: 46px; margin-right: 14px; flex: none; }
.nation-strip { display: flex; align-items: flex-end; gap: 16px; flex-wrap: wrap; padding: 8px 4px 0; }
.nation-strip .form-group { margin-bottom: 0; }
.nation-note { font-size: 12px; color: var(--app-muted); padding-bottom: 8px; }
.header-left { display: flex; align-items: center; }

/* Tabs: sky rule on the active tab. */
.nav-tabs { border-bottom: 1px solid var(--app-line); margin-top: 10px; }
.nav-tabs .nav-link { color: var(--app-blue); font-weight: 600; border: 0; border-top: 3px solid transparent; border-radius: 0; }
.nav-tabs .nav-link.active {
  color: var(--app-navy); background: var(--app-surface); font-weight: 700;
  border-top: 3px solid var(--app-sky); border-left: 1px solid var(--app-line); border-right: 1px solid var(--app-line);
}

/* Buttons and focus. */
.btn-outline-secondary { color: var(--app-navy); border-color: var(--app-navy); }
.btn-outline-secondary:hover { background: var(--app-navy); color: var(--app-surface); border-color: var(--app-navy); }
.btn-primary { background: var(--app-blue); border-color: var(--app-blue); }
:focus-visible { outline: 3px solid var(--app-blue); outline-offset: 2px; }

/* Cards and panels sit on white with a hairline. */
div.stat-card, .panel-section, .changes-strip, .briefing-body, .empty-state {
  background: var(--app-surface); border-color: var(--app-line);
}
div.stat-card { border-top: 3px solid var(--app-sky); }
.shiny-sidebar { background: var(--app-tint); }
table.dataTable thead th { color: var(--app-ink); border-bottom: 2px solid var(--app-sky); }
.dataTables_wrapper .dataTables_paginate .paginate_button.current {
  background: var(--app-navy) !important; color: var(--app-surface) !important; border-color: var(--app-navy) !important;
}
.exposure-grid td.name { color: var(--app-ink); }
.irs--shiny .irs-bar, .irs--shiny .irs-handle { background: var(--app-navy); border-color: var(--app-navy); }
.irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to { background: var(--app-navy); }
.form-check-input:checked { background-color: var(--app-navy); border-color: var(--app-navy); }
.stat-card .stat-num.accent { color: var(--app-blue); }

/* Demonstration ribbon and controls (demo mode only). */
.demo-ribbon {
  background: var(--app-ink); color: var(--app-surface); font-size: 12px; font-weight: 700;
  letter-spacing: 0.06em; text-transform: uppercase; text-align: center; padding: 5px 12px;
  margin: 0 calc(-0.5 * var(--bs-gutter-x, 1.5rem));
}
.demo-controls {
  display: flex; gap: 18px; align-items: flex-end; flex-wrap: wrap; font-size: 13px;
  background: var(--app-tint); border-bottom: 1px solid var(--app-line);
  padding: 8px calc(0.5 * var(--bs-gutter-x, 1.5rem));
  margin: 0 calc(-0.5 * var(--bs-gutter-x, 1.5rem));
}
.demo-controls .form-group, .demo-controls .checkbox { margin: 0; }
.demo-controls label { font-size: 12px; color: var(--app-muted); }
.demo-note { font-size: 12px; color: var(--app-muted); }
.poc-tag { font-size: 10px; font-weight: 700; letter-spacing: 0.05em; text-transform: uppercase; color: var(--app-ink);
           background: var(--app-ice); border-radius: 8px; padding: 1px 7px; margin-left: 4px; }

/* Overview. */
.ov-headline { background: var(--app-surface); border: 1px solid var(--app-line); border-left: 6px solid var(--app-bright);
               border-radius: 4px; padding: 14px 18px; margin-bottom: 14px; }
.ov-headline .ov-kicker { font-size: 11px; text-transform: uppercase; letter-spacing: 0.06em; color: var(--app-muted); }
.ov-headline .ov-text { font-size: 19px; line-height: 1.35; margin-top: 4px; }
.ov-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(190px, 1fr)); gap: 12px; margin-bottom: 14px; }
.ov-grid .stat-card { margin: 0; }
.ov-outlook { display: grid; grid-template-columns: repeat(5, 1fr); gap: 8px; margin-bottom: 16px; }
.ov-day { background: var(--app-surface); border: 1px solid var(--app-line); border-radius: 4px; padding: 10px; text-align: center; }
.ov-day .d { font-size: 12px; color: var(--app-muted); }
.ov-day .n { font-size: 12px; margin-top: 6px; color: var(--app-ink); }
.ov-day .rank-chip { margin-top: 6px; min-width: 70px; }
.ov-cols { display: grid; grid-template-columns: 3fr 2fr; gap: 14px; }
@media (max-width: 992px) { .ov-cols, .ov-outlook { grid-template-columns: 1fr; } }
.ov-panel { background: var(--app-surface); border: 1px solid var(--app-line); border-radius: 4px; padding: 12px 14px; }
.ov-panel h6 { margin: 0 0 8px 0; font-size: 12px; text-transform: uppercase; letter-spacing: 0.05em; color: var(--app-muted); }
.ov-table { width: 100%; font-size: 13px; }
.ov-table td, .ov-table th { padding: 4px 6px; border-bottom: 1px solid var(--app-line); }
.ov-table tr.clickable { cursor: pointer; }
.ov-table tr.clickable:hover { background: var(--app-tint); }
.ov-foot { font-size: 12px; color: var(--app-muted); margin-top: 10px; }

/* --- 9. Guide ------------------------------------------------------------- */

.guide { display: grid; grid-template-columns: 230px minmax(0, 1fr); gap: 24px; align-items: start; }
.guide-nav { position: sticky; top: 8px; max-height: calc(100vh - 24px); overflow-y: auto;
             background: var(--app-surface); border: 1px solid var(--app-line); border-radius: 4px; padding: 12px; font-size: 13px; }
.guide-nav-h { font-size: 11px; text-transform: uppercase; letter-spacing: 0.06em; color: var(--app-muted); margin-bottom: 6px; }
.guide-nav ul { list-style: none; padding: 0; margin: 0; }
.guide-nav li { margin: 0; }
.guide-nav a { display: block; padding: 4px 6px; border-radius: 3px; text-decoration: none; }
.guide-nav a:hover { background: var(--app-tint); }
.guide-content { max-width: 880px; }
.guide-intro h2 { margin: 0 0 6px 0; }
.guide-search { max-width: 360px; margin-bottom: 12px; }
.guide-part { font-size: 12px; text-transform: uppercase; letter-spacing: 0.07em; color: var(--app-muted); margin: 22px 0 8px 0; }
.guide-card { background: var(--app-surface); border: 1px solid var(--app-line); border-left: 5px solid var(--app-bright);
              border-radius: 4px; padding: 14px 18px; margin-bottom: 14px; scroll-margin-top: 8px; }
.guide-card.guide-poc { border-left-color: var(--app-ice); }
.guide-head { display: flex; align-items: center; gap: 10px; flex-wrap: wrap; }
.guide-head h3 { margin: 0; font-size: 18px; }
.guide-card h4 { font-size: 12px; text-transform: uppercase; letter-spacing: 0.06em; color: var(--app-muted); margin: 14px 0 4px 0; }
.guide-card ol, .guide-card ul { padding-left: 20px; margin-bottom: 4px; }
.guide-card li { margin-bottom: 4px; }
.guide-q { margin: 8px 0 4px 0; }
.guide-where, .guide-tables { font-size: 12px; color: var(--app-muted); margin: 2px 0; }
.guide-gap { font-size: 11px; background: var(--app-tint); border: 1px solid var(--app-ice); border-radius: 8px; padding: 1px 8px; color: var(--app-ink); }
.guide-matrix-wrap { overflow-x: auto; margin: 10px 0; }
.guide-matrix { border-collapse: separate; border-spacing: 3px; font-size: 12px; }
.guide-matrix caption { caption-side: bottom; font-size: 11px; color: var(--app-muted); padding-top: 6px; }
.guide-matrix th { font-weight: 500; text-align: center; padding: 3px 8px; }
.guide-matrix th[scope=row] { text-align: right; white-space: nowrap; }
.guide-cell { width: 92px; height: 40px; text-align: center; font-weight: 700; border-radius: 3px; }
.guide-cell.green  { background: #4f9d4f; color: var(--app-surface); }
.guide-cell.yellow { background: #f0d040; color: var(--app-ink); }
.guide-cell.amber  { background: #e08020; color: var(--app-ink); }
.guide-cell.red    { background: #b02020; color: var(--app-surface); }
.guide-table { width: 100%; font-size: 13px; border-collapse: collapse; margin-top: 6px; }
.guide-table th, .guide-table td { padding: 6px 8px; border-bottom: 1px solid var(--app-line); vertical-align: top; text-align: left; }
.guide-table thead th { border-bottom: 2px solid var(--app-bright); }
.guide-lineage { display: block; overflow-x: auto; }
.guide-glossary dt { font-weight: 700; margin-top: 8px; }
.guide-glossary dd { margin: 0 0 2px 0; }
@media (max-width: 992px) { .guide { grid-template-columns: 1fr; } .guide-nav { position: static; max-height: none; } }
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
  checkboxInput("layer_risk_grid", "Risk grid, 100 m (zoom in)", value = FALSE),
  conditionalPanel(
    condition = "input.layer_risk_grid == true",
    tags$div(
      style = "margin-left: 16px; margin-top: -4px;",
      sliderInput("grid_min_score", "Show cells scoring at least", min = 0, max = 90, value = 10, step = 5, ticks = FALSE),
      tags$p(class = "small", HTML(guide_link_html("risk-grid", "How the risk score is calculated"))),
      uiOutput("grid_note"))
  ),
  checkboxInput("layer_rfg", "Rapid flood guidance polygons (when in force)", value = FALSE),
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
             tags$div("Affected Target Areas", class = "stat-label")),
    
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
      h6("Affected Target Areas", class = "panel-h"),
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

# Nations filter, under the header. The data holds constituencies in every nation; this chooses
# which are shown, on every tab. England is the starting choice (FGS_DEFAULT_NATIONS), and
# clearing the box shows all four.
make_nation_strip <- function() tags$div(
  class = "nation-strip",
  title = "Wales, Scotland and Northern Ireland are included in the data. Add them here to see constituencies the forecast reaches there.",
  selectizeInput("nations", "Constituencies in",
                 choices  = NATIONS, selected = DEFAULT_NATIONS, multiple = TRUE,
                 options  = list(placeholder = "All four nations"), width = "320px"),
  tags$span(class = "visually-hidden",
            "Wales, Scotland and Northern Ireland are included in the data. Add them here to see constituencies the forecast reaches there.")
)

make_situation_tab <- function() tags$div(
  class = "sit-page",
  tags$h1(class = "app-title", "Constituencies and briefings"),
  tags$p(class = "app-lead", "Each constituency's forecast risk, the warnings in force and its MP briefing. Select a row for the briefing."),
  uiOutput("situation_note"),
  uiOutput("situation_kpis"),
  tags$h2("Find constituencies"),
  tags$div(
    class = "filter-row",
    # POC-ONLY: statement look-back. Remove this control with the other POC-ONLY blocks.
    if (POC_FEATURES) selectInput("sit_statement", tagList("Statement ", tags$span(class = "poc-tag", "POC only")),
                                  choices = c("Latest (live table)" = "latest"), selected = "latest", width = "270px"),
    selectInput("sit_min_rank", "Minimum forecast risk",
                choices  = c("Any" = "0", "Very Low" = "1", "Low" = "2",
                             "Medium" = "3", "High" = "4"),
                selected = "3", width = "180px"),
    selectizeInput("sit_sources", "Flood source",
                   choices  = c("River" = "river", "Coastal" = "coastal", "Surface water" = "surface",
                                "Groundwater" = "ground"),
                   multiple = TRUE, options = list(placeholder = "All sources"), width = "230px"),
    textInput("sit_search", "Search", placeholder = "Constituency or MP", width = "220px"),
    downloadButton("download_situation", "Export CSV",
                   class = "btn-sm btn-outline-secondary")
  ),
  tags$details(
    class = "app-details",
    tags$summary("More filters"),
    tags$div(
      class = "filter-row",
      selectInput("sit_min_impact", "Minimum impact",
                  choices  = c("Any" = "0", stats::setNames(as.character(seq_along(IMPACT_LABELS)), IMPACT_LABELS)),
                  selected = "0", width = "160px"),
      checkboxInput("sit_escalated", "Escalated only", FALSE),
      checkboxInput("sit_warning", "Warning or alert in force", FALSE)
    )
  ),
  tags$h2("Constituencies"),
  DT::dataTableOutput("situation_table"),
  uiOutput("rank_legend"),
  tags$h2(class = "sit-briefing-h", "MP briefing"),
  uiOutput("briefing_panel")
)


# -----------------------------------------------------------------------------
# Constituency history tab (was Warning history).
#
# One row per constituency: how many flood alerts and flood warnings have been
# issued for areas inside it, and the total. Select a row to see each area.
# Counts come from ea_warning_episodes (job 25). The slider narrows the period.
# -----------------------------------------------------------------------------

# Warning history tab: how often each flood warning area and flood alert area (each Target Area) has been issued, and for how long.
make_area_history_tab <- function() tags$div(
  class = "wh-page",
  tags$h1(class = "app-title", "Warning history"),
  tags$p(class = "app-lead", "How many times each flood warning area and flood alert area has been issued, and for how long it was in force. Select a row to see each time it was issued."),
  uiOutput("ah_note"),
  tags$div(
    class = "filter-row",
    sliderInput("ah_range", "Issued between",
                min = Sys.Date() - 3650L, max = Sys.Date(), value = c(Sys.Date() - 3650L, Sys.Date()),
                timeFormat = "%d %b %Y", width = "420px"),
    selectInput("ah_kind", "Kind", choices = c("Flood warning and alert" = "both", "Flood warning" = "flood_warning", "Flood alert" = "flood_alert"), width = "220px"),
    textInput("ah_search", "Search", placeholder = "Area name or code", width = "220px"),
    downloadButton("download_ah", "Export CSV", class = "btn-sm btn-outline-secondary")
  ),
  uiOutput("ah_kpis"),
  tags$h2("Target Areas"),
  DT::dataTableOutput("ah_table"),
  tags$h2(class = "sit-briefing-h", "Each time it was issued"),
  uiOutput("ah_detail_title"),
  DT::dataTableOutput("ah_detail"),
  tags$div(class = "mt-2", downloadButton("download_ah_detail", "Export this table", class = "btn-sm btn-outline-secondary"))
)

make_warning_history_tab <- function() tags$div(
  class = "wh-page",
  tags$h1(class = "app-title", "Constituency history"),
  tags$p(class = "app-lead", "How many flood alerts and flood warnings have been issued for areas in each constituency. Select a row to see each area."),
  uiOutput("wh_note"),
  tags$div(
    class = "filter-row",
    sliderInput("wh_range", "Issued between",
                min = Sys.Date() - 3650L, max = Sys.Date(), value = c(Sys.Date() - 3650L, Sys.Date()),
                timeFormat = "%d %b %Y", width = "420px"),
    textInput("wh_search", "Search", placeholder = "Constituency or MP", width = "220px"),
    downloadButton("download_warning_history", "Export CSV", class = "btn-sm btn-outline-secondary")
  ),
  tags$h2("Constituencies"),
  DT::dataTableOutput("wh_table"),
  tags$h2(class = "sit-briefing-h", "Areas in the selected constituency"),
  uiOutput("wh_detail_title"),
  checkboxInput("wh_detailed", "Detailed: show the date and time of every alert and warning issued", FALSE),
  DT::dataTableOutput("wh_areas"),
  tags$div(class = "mt-2", downloadButton("download_wh_areas", "Export this table",
                                          class = "btn-sm btn-outline-secondary")),
  tags$h2(class = "sit-briefing-h", "Warnings in force at the same time"),
  uiOutput("wh_concurrent_note"),
  if (requireNamespace("plotly", quietly = TRUE)) plotly::plotlyOutput("wh_concurrent", height = "auto")
)


# -----------------------------------------------------------------------------
# Warnings vs FGS tab.
#
# Was each alert and warning under an FGS risk polygon on the day it was issued, and was
# that forecast made before the day or on it? Reads ea_warning_fgs_coverage (job 26). The
# top shows how the polygons covered the warnings in a period, at each risk level. Below,
# the warnings themselves; select one for the statements behind it.
# -----------------------------------------------------------------------------

make_coverage_tab <- function() tags$div(
  class = "cov-page",
  tags$h1(class = "app-title", "Warnings and the forecast"),
  tags$p(class = "app-lead", "Was each alert and warning under an FGS risk polygon on the day it was issued, and was that forecast made before the day or on it?"),
  uiOutput("cov_note"),
  tags$div(
    class = "filter-row",
    sliderInput("cov_range", "Warnings issued between",
                min = Sys.Date() - 730L, max = Sys.Date(), value = c(Sys.Date() - 365L, Sys.Date()),
                timeFormat = "%d %b %Y", width = "420px"),
    selectInput("cov_type", "Kind of warning", choices = COVERAGE_TYPES, selected = "any", width = "220px"),
    selectInput("cov_rank", "Counts as covered at", choices = COVERAGE_RISKS, selected = 1L, width = "260px"),
    downloadButton("download_coverage", "Export CSV", class = "btn-sm btn-outline-secondary")
  ),
  uiOutput("cov_kpis"),
  tags$h2("How the polygons covered the warnings"),
  tags$p(class = "app-caption", "By how serious a polygon must be to count."),
  DT::dataTableOutput("cov_shares"),
  if (requireNamespace("plotly", quietly = TRUE)) plotly::plotlyOutput("cov_plot", height = "auto"),
  tags$h2(class = "sit-briefing-h", "The warnings"),
  uiOutput("cov_list_note"),
  DT::dataTableOutput("cov_table"),
  tags$h2(class = "sit-briefing-h", "Statements behind a warning"),
  uiOutput("cov_detail_title"),
  DT::dataTableOutput("cov_detail"),
  tags$details(class = "app-details", tags$summary("How coverage is judged"), tags$p(
             "The reference is the day the warning was issued, because where there is an impact the FGS keeps a polygon over the area for the whole warning. ",
             "Before: a statement issued on an earlier day put a polygon over the area for the issue day, and the earliest one decides. ",
             "Same day: the earliest was issued on the issue day. Later days only: no polygon covered the issue day, but one covered a later day the warning was in force. ",
             "Not covered: no polygon was over the area on any day the warning was in force. ",
             "A coastal warning area counts as covered when a polygon covers the alert area above it. ",
             "The FGS forecasts risk across regions and the Environment Agency warns for particular rivers and coasts, ",
             "so a warning with no polygon is a disagreement between the two, not necessarily a missed forecast. ",
             "Polygons that had no warning under them are not counted here."))
)


# -----------------------------------------------------------------------------
# Hydrometrics tab.
#
# Environment Agency real-time river level, flow and rainfall readings, the daily
# archive and the live tail as one series. Narrow by area, river, parameter,
# station and FFFS ID, pick up to eight measures, set how many days back. One panel per unit;
# the shaded band is the live tail that is not yet archived.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Rain gauges tab.
#
# A map of the Environment Agency's rain gauges. Each is coloured by its rainfall over the past 1, 12 or 24 hours;
# click one for its name, ids, latest reading and all three totals. Clicked gauges are added to a bar chart of their
# past 24 hours beside the map (up to six, one panel each, on one scale). A gauge with no recent reading is grey.
# -----------------------------------------------------------------------------

# Gauges near thresholds tab: for each warning area, the gauge linked to its Observed flood warning threshold, how close the level is, whether it is
# rising, and who is below it. The thresholds and links are job 28's (restricted); levels are the live feed's.
# Gauge value tab: what each gauge stands behind. A gauge's thresholds belong to warning areas (pipeline notebook 28); the value is the indicative expected
# annual damage in those areas, gross (all of it) and split (an area's value shared between the gauges that drive it, so the split figures add up).
make_gauge_value_tab <- function() tags$div(
  class = "areas-page",
  tags$h1(class = "app-title", "Gauge value"),
  tags$p(class = "app-lead", "What each gauge stands behind: the indicative expected annual damage in the warning areas its thresholds belong to. Ranked by the split value."),
  uiOutput("gv_note"),
  uiOutput("gv_econ_bar"),
  uiOutput("gv_kpis"),
  tags$div(class = "filter-row", downloadButton("download_gv", "Export CSV", class = "btn-sm btn-outline-secondary")),
  DT::dataTableOutput("gv_table"),
  tags$h2(class = "sit-briefing-h", "Areas behind the selected gauge"),
  uiOutput("gv_detail_title"),
  DT::dataTableOutput("gv_detail"),
  uiOutput("gv_basis")
)

make_threshold_board_tab <- function() tags$div(
  class = "areas-page",
  tags$h1(class = "app-title", "Gauges near thresholds"),
  tags$p(class = "app-lead", "The gauges linked to the flood warning thresholds, closest first, with the people at risk below each."),
  uiOutput("tb_note"),
  tags$div(
    class = "filter-row",
    selectInput("tb_within", "Show gauges within", choices = c("0.25 m" = 0.25, "0.5 m" = 0.5, "1 m" = 1, "2 m" = 2), selected = 1, width = "180px"),
    checkboxInput("tb_rising", "Only those at or above, or rising", FALSE),
    downloadButton("download_tb", "Export CSV", class = "btn-sm btn-outline-secondary")
  ),
  uiOutput("tb_kpis"),
  DT::dataTableOutput("tb_table"),
  tags$details(class = "app-details", tags$summary("How this is worked out"), tags$p(
    "Each flood warning area has Observed thresholds linked to gauges (notebook 28). The Response threshold is the one that matters, so for each area and gauge the lowest Response ",
    "threshold is used, or the lowest of any kind where there is no Response threshold. Headroom is the threshold minus the newest level, in metres, so it needs no datum. ",
    "The trend compares the newest level with the average two to four hours earlier. The time to the threshold is shown only for a rising level and is a straight-line estimate, ",
    "not a forecast. A reading older than three hours is left out. The people at risk are the people in the area in a High or Medium rivers and sea band. ",
    "In force now is what the Environment Agency feed has in force for the area. Potential means the area lies under a risk polygon of Low or above in the latest Flood Guidance Statement and nothing is in force there yet."))
)

make_rain_tab <- function() tags$div(
  class = "rain-page",
  tags$h1(class = "app-title", "Rain gauges"),
  tags$p(class = "app-lead", "Rainfall recorded by Environment Agency gauges, live. Select a gauge on the map to chart it."),
  tags$div(
    class = "filter-row",
    selectInput("rain_area",   "EA area",   choices = RT_ALL, width = "230px"),
    selectInput("rain_metric", "Colour by", choices = RAIN_METRICS, selected = "mm_24h", width = "170px"),
    selectizeInput("rain_find", "Find a gauge", choices = NULL, width = "320px",
                   options = list(placeholder = "Name, station or FFFS ID")),
    actionButton("rain_refresh", "Refresh", icon = icon("rotate"), class = "btn-sm btn-outline-secondary")
  ),
  tags$details(
    class = "app-details",
    tags$summary("Filter out stale or suspect gauges"),
    tags$div(
      class = "filter-row",
      checkboxInput("rain_hide_stale", sprintf("Hide gauges with no reading for %d hours", RAIN_STALE_HOURS), value = FALSE),
      numericInput("rain_max_reading", "Ignore single readings above (mm)", value = RAIN_MAX_READING, min = 1, max = 1000, step = 5, width = "200px"),
      checkboxInput("rain_hide_suspect", "Hide gauges with suspect values", value = FALSE)
    )
  ),
  uiOutput("rain_message"),
  uiOutput("rain_summary"),
  tags$div(
    class = "row g-4",
    tags$div(class = "col-12 col-xl-7", leaflet::leafletOutput("rain_map", height = "72vh")),
    tags$div(
      class = "col-12 col-xl-5",
      tags$h2("Charted gauges"),
      selectizeInput("rain_charted", "Click the map to add a gauge, up to 6", choices = NULL, multiple = TRUE, width = "100%",
                     options = list(maxItems = RAIN_MAX_CHARTED, placeholder = "Click a gauge on the map")),
      tags$div(class = "d-flex flex-wrap align-items-center gap-3 mb-2",
               radioButtons("rain_mode", NULL, choices = c("Each interval" = "bars", "Running total" = "cumulative"), selected = "bars", inline = TRUE),
               radioButtons("rain_bars", NULL, choices = c("1 hour" = 3600L, "15 minutes" = 900L), selected = 3600L, inline = TRUE),
               actionButton("rain_clear", "Clear", class = "btn-sm btn-outline-secondary")),
      uiOutput("rain_chart_head"),
      if (requireNamespace("plotly", quietly = TRUE)) plotly::plotlyOutput("rain_plot", height = "auto")
      else tags$p(class = "text-muted", "The plotly package is not installed on this server, so the chart cannot be drawn.")
    )
  ),
  tags$details(
    class = "app-details",
    tags$summary("About these readings"),
    tags$p("This uses Environment Agency flood and river level data from the real-time data API (Beta). ",
           "Totals add up the readings in the window; times are UTC. Where a gauge reports both 15-minute totals and ",
           "instantaneous readings, the totals are used. A single reading below zero or above the limit is left out of the totals. ",
           "A gauge with such readings, or a total too large to believe, has a dashed outline and is suspect. ",
           "These are readings as supplied, not quality-controlled.")
  )
)


make_hydrometrics_tab <- function() tags$div(
  class = "rt-page",
  tags$h1(class = "app-title", "River levels and flows"),
  tags$p(class = "app-lead", "Levels, flows and other readings from Environment Agency gauges. Choose a station, then the measures to chart."),
  tags$h2("Choose a station"),
  tags$p(class = "app-caption", "Narrow by area, river or parameter, or go straight to a station or FFFS ID."),
  tags$div(
    class = "filter-row",
    selectInput("rt_area",    "EA area",   choices = RT_ALL, width = "230px"),
    selectInput("rt_river",   "River",     choices = RT_ALL, width = "230px"),
    selectInput("rt_param",   "Parameter", choices = RT_ALL, width = "170px"),
    selectInput("rt_station", "Station",   choices = RT_ALL, width = "260px"),
    selectizeInput("rt_fffs", "FFFS ID",   choices = RT_ALL, width = "170px")
  ),
  tags$h2("What to chart"),
  tags$div(
    class = "filter-row",
    selectizeInput("rt_measures", "Measures (up to 8)", choices = NULL, multiple = TRUE, width = "620px",
                   options = list(maxItems = RT_MAX_MEASURES, placeholder = "Pick a station to start")),
    selectInput("rt_days", "How far back", choices = RT_WINDOWS, selected = 3L, width = "180px"),
    checkboxInput("rt_thresholds", "Show flood warning thresholds, where available", FALSE),
    conditionalPanel("input.rt_thresholds", checkboxInput("rt_thr_res_only", "Only the RES thresholds", FALSE)),
    actionButton("rt_refresh", "Refresh", icon = icon("rotate"), class = "btn-sm btn-outline-secondary")
  ),
  uiOutput("rt_message"),
  if (requireNamespace("plotly", quietly = TRUE)) plotly::plotlyOutput("rt_plot", height = "auto")
  else tags$p(class = "text-muted", "The plotly package is not installed on this server, so the chart cannot be drawn."),
  uiOutput("rt_thr_note"),
  uiOutput("rt_summary"),
  tags$details(
    class = "app-details",
    tags$summary("About these readings"),
    tags$p("This uses Environment Agency flood and river level data from the real-time data API (Beta). ",
           "Times are UTC. The dashed line is where the archive ends and the live tail begins; ",
           "the shaded band to its right is live and not yet archived.")
  )
)


# -----------------------------------------------------------------------------
# Exposure history tab.
#
# Heat grid from fgs_constituency_exposure (job 21): the constituencies with
# most days at or above a chosen risk over the last 12 months, by month.
# -----------------------------------------------------------------------------

make_exposure_tab <- function() tags$div(
  class = "exp-page",
  tags$h1(class = "app-title", "Exposure history"),
  tags$p(class = "app-lead", "The constituencies with most days at or above a chosen forecast risk over the last 12 months, by month."),
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
  tags$p(class = "app-caption",
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
  class = "ov-page",
  tags$h1(class = "app-title", "Overview"),
  uiOutput("ov_rfg"),
  uiOutput("ov_headline"),
  tags$section(class = "ov-section",
               tags$h2("Warnings and alerts in force"),
               uiOutput("ov_inforce")),
  uiOutput("ov_kpis"),
  tags$section(class = "ov-section",
               tags$h2("Five-day outlook"),
               tags$p(class = "app-caption", "Constituencies at Medium risk or above, day by day."),
               uiOutput("ov_outlook")),
  tags$div(
    class = "ov-cols",
    tags$section(class = "ov-section",
                 tags$h2("Constituencies to watch"),
                 uiOutput("ov_watch")),
    tags$section(class = "ov-section",
                 tags$h2("Changed since the previous statement"),
                 uiOutput("ov_changes"))
  ),
  uiOutput("ov_foot")
)


# -----------------------------------------------------------------------------
# Rapid Flood Guidance tab (FGS_Notebooks jobs 36 and 37).
#
# The Flood Forecasting Centre's rapid flooding guidance: its status, the valid polygons on a map, what lies under each (people, households, critical
# sites), the warning and alert areas and constituencies it reaches, and the history of recent updates. Counted from address points.
# -----------------------------------------------------------------------------

make_rfg_tab <- function() tags$div(
  class = "areas-page rfg-page",
  tags$h1(class = "app-title", "Rapid Flood Guidance"),
  tags$p(class = "app-lead", "Rapid flooding guidance from the Flood Forecasting Centre: where short, intense rainfall may cause flooding, and who and what lies under it."),
  uiOutput("rfg_note"),
  uiOutput("rfg_status"),
  tags$div(
    class = "area-cols",
    tags$div(leaflet::leafletOutput("rfg_map", height = "420px")),
    tags$div(tags$h2("Polygons"), DT::dataTableOutput("rfg_polygon_table"))),
  tags$h2("Warning and alert areas it reaches"),
  DT::dataTableOutput("rfg_areas_table"),
  tags$h2("Constituencies it reaches"),
  DT::dataTableOutput("rfg_cons_table"),
  tags$h2("Critical sites inside it"),
  DT::dataTableOutput("rfg_sites_table"),
  tags$h2("Recent updates"),
  tags$p(class = "app-caption", "Select an update to see it above. The in force guidance opens by default."),
  DT::dataTableOutput("rfg_history"),
  tags$p(class = "app-caption", "Counted from address points inside each valid polygon (pipeline notebook 37). A person inside two areas of one kind is in both rows, so the area rows should not be added together.")
)


# -----------------------------------------------------------------------------
# Warning areas tab.
#
# One row per flood warning area and flood alert area, with the people and households inside it and how many of them sit in a High or
# Medium rivers and sea band (FGS_Notebooks job 35). Pick a row for the figures, the overlap with other areas and the sites inside.
# Rivers and the sea only: surface water is not counted.
# -----------------------------------------------------------------------------

make_areas_tab <- function() tags$div(
  class = "areas-page",
  tags$h1(class = "app-title", "Target Areas"),
  tags$p(class = "app-lead", "Who and what sits inside each flood warning and flood alert area, and how much of it lies in each flood risk band."),
  uiOutput("areas_note"),
  uiOutput("areas_forecast"),
  uiOutput("areas_econ_bar"),
  tags$div(
    class = "filter-row",
    selectInput("areas_kind", "Kind", choices = c("Flood warning and alert" = "both", "Flood warning" = "flood_warning", "Flood alert" = "flood_alert"), width = "220px"),
    selectInput("areas_county", "County", choices = c("All" = ""), width = "200px"),
    selectInput("areas_source", "Flood source", choices = c("Rivers and the sea" = "any", "Fluvial (rivers)" = "fluvial", "Coastal" = "coastal"), width = "200px"),
    selectInput("areas_sort", "Sort by", choices = c("Risk score" = "risk_score", "People at risk" = "people_rs_high_medium", "Share at risk" = "share_people_high_medium", "People" = "people",
                                                    "Households at High or Medium" = "households_rs_high_medium", "Expected annual damage" = "econ_sort", "Critical sites" = "sites", "Area name" = "area_name"), width = "230px"),
    textInput("areas_search", "Search", placeholder = "Area name", width = "200px"),
    downloadButton("download_areas", "Download CSV", class = "btn-primary")
  ),
  tags$p(class = "app-caption", "Rivers and the sea bands, from the National Flood Risk Assessment (NaFRA2). The bar shows the share of the area's people in High, Medium, Low and Very Low chance of flooding in any year. Select an area name for the detail."),
  DT::dataTableOutput("areas_table"),
  uiOutput("areas_band_legend"),
  tags$h2(class = "sit-briefing-h", "The selected area"),
  uiOutput("areas_description"),
  tags$h3("Areas that share people with it"),
  DT::dataTableOutput("areas_overlaps"),
  tags$h2("Who the warning areas reach"),
  uiOutput("areas_coverage"),
  tags$details(class = "app-details", tags$summary("How the figures are made"), tags$p(
    "Each address point is placed in the areas whose boundary contains it, and its people are the Census 2021 output area total shared equally among the points in that output area. ",
    "Areas overlap, so a person can be in more than one: read the unique and split figures in the Target Area page before adding areas together, and never add warning areas to alert areas. ",
    "At risk means a High or Medium band in the rivers and sea risk map. Surface water is not counted, because the Environment Agency does not warn for it. ",
    "Until AddressBase Premium is available the points come from the National Statistics UPRN Lookup, so counts are of addresses, not of buildings or businesses."))
)


# -----------------------------------------------------------------------------
# Target Area tab: one warning or alert area. The figures, the 100 m risk grid inside its boundary, the same area by flood band,
# the people by band, what it overlaps, and the constituencies it lies in.
# -----------------------------------------------------------------------------

make_area_detail_tab <- function() tags$div(
  class = "areas-page",
  tags$div(class = "filter-row",
           selectizeInput("area_pick", "Area", choices = NULL, width = "460px", options = list(placeholder = "Choose an area")),
           downloadButton("download_area_brief", "Download a one-page brief (PDF)", class = "btn-sm btn-primary"),
           downloadButton("download_area_cells", "Download this area's 100 m cells", class = "btn-sm btn-outline-secondary")),
  uiOutput("area_head"),
  uiOutput("area_rfg"),
  uiOutput("area_kpis"),
  # Top row: the two maps on the left; flood band, surface water, critical sites and constituencies on the right. Second row: expected annual damage
  # and who is vulnerable. Then the gauges and the warning history, each across both columns. All stack on a narrow screen (see .area-grid in design.R).
  tags$div(
    class = "area-grid",
    tags$div(
      class = "area-col-maps",
      tags$h2("Risk grid"),
      tags$p(class = "app-caption", "Each square is 100 m. Colour is the composite risk score, with the warning area outlined. ",
             HTML(guide_link_html("risk-grid", "How the risk score is calculated"))),
      leaflet::leafletOutput("area_grid_map", height = "420px"),
      uiOutput("area_grid_note"),
      tags$h2("The same area by flood band"),
      tags$p(class = "app-caption", "The map can show the NaFRA2 bands on their own, without the score."),
      leaflet::leafletOutput("area_band_map", height = "420px")),
    tags$div(
      class = "area-col-figures",
      tags$h2("By flood band"),
      tags$p(class = "app-caption", "Rivers and the sea. Surface water is not counted."),
      uiOutput("area_bands"),
      tags$h2("Surface water (context)"),
      uiOutput("area_sw"),
      tags$h2("Critical sites"),
      uiOutput("area_sites"),
      DT::dataTableOutput("area_sites_table"),
      uiOutput("area_sites_download"),
      tags$h2("Constituencies"),
      uiOutput("area_cons"))),
  tags$div(
    class = "area-grid",
    tags$div(
      tags$h2("Expected annual damage (indicative)"),
      uiOutput("area_econ_bar"),
      uiOutput("area_econ")),
    tags$div(
      tags$h2("Who is vulnerable"),
      uiOutput("area_vuln"))),
  tags$div(
    class = "area-wide area-table",
    tags$h2("Linked gauges"),
    uiOutput("area_gauges_note"),
    DT::dataTableOutput("area_gauges_table")),
  tags$div(
    class = "area-wide area-table",
    tags$h2("Warning issue history"),
    uiOutput("area_issue_summary"),
    if (requireNamespace("plotly", quietly = TRUE)) plotly::plotlyOutput("area_issue_chart", height = "auto"),
    DT::dataTableOutput("area_issue_table"),
    downloadButton("download_area_issues", "Download this area's issue history", class = "btn-sm btn-outline-secondary")),
  tags$div(
    class = "area-wide",
    tags$h2("Who responds"),
    tags$p(class = "app-caption", "The police forces, authorities and regions this area lies in, with the people in each. A person is counted once in each table, so each table adds up to the area's people, except combined authorities, which cover only part of England: people outside one are not listed."),
    uiOutput("area_responders"),
    tags$h2("Areas that share people with it"),
    DT::dataTableOutput("area_overlaps"))
)


# -----------------------------------------------------------------------------
# Constituency exposure tab.
#
# Per constituency: people, people in a High or Medium rivers and sea band, and how many of those no warning area (or no alert area) covers.
# The constituency code in the exposure table must match the app's constituency id; the note says when names could not be matched.
# -----------------------------------------------------------------------------

make_const_exposure_tab <- function() tags$div(
  class = "areas-page",
  tags$h1(class = "app-title", "Constituency exposure"),
  tags$p(class = "app-lead", "People in each constituency who are in a High or Medium band for flooding from rivers and the sea, and how many of them no flood warning area or flood alert area covers."),
  uiOutput("const_exp_note"),
  tags$div(
    class = "filter-row",
    selectInput("const_exp_cover", "Gap measured against", choices = c("Flood warning areas" = "warning", "Flood alert areas" = "alert"), width = "240px"),
    selectInput("const_pot_rank", "Potential: areas under forecast risk of at least", choices = c("Very Low" = 1, "Low" = 2, "Medium" = 3, "High" = 4), selected = 2, width = "260px"),
    checkboxInput("const_flagged", "Only constituencies with something in force or potential", FALSE),
    downloadButton("download_const_exp", "Export CSV", class = "btn-sm btn-outline-secondary")
  ),
  tags$p(class = "app-caption", "In force now is from the Environment Agency feed. Potential is the number of flood warning and flood alert areas that lie under a risk polygon in the latest Flood Guidance Statement, not ones that will be issued. Select a constituency for its detail and a one-page brief."),
  DT::dataTableOutput("const_exp_table"),
  tags$h2(class = "sit-briefing-h", "The selected constituency"),
  uiOutput("const_detail"),
  downloadButton("download_const_brief", "Download a one-page brief (PDF)", class = "btn-sm btn-primary")
)


# -----------------------------------------------------------------------------
# Exports tab.
#
# Three lists to take away as CSV: the flood warning and flood alert areas linked to constituencies, every constituency with its
# MP's contact details, and the briefing emails drafted for the current statement. The builders are in helpers.R (exp_*_table).
# Nothing here is sent anywhere. The exports ignore the nations filter: each file holds everything it describes.
# -----------------------------------------------------------------------------

make_exports_tab <- function() tags$div(
  class = "exports-page",
  tags$h1(class = "app-title", "Exports"),
  tags$p(class = "app-lead", "Lists to take away as CSV files. They open in Excel and work as the source for a mail merge."),
  tags$section(
    class = "ex-section",
    tags$h2("Flood warning and flood alert areas linked to constituencies"),
    tags$p("Each flood warning area and flood alert area with the constituencies it overlaps, the share of the area in each, and the MP."),
    checkboxInput("exp_all_overlaps", "Include very small overlaps as well (by default only overlaps at or above the app's minimum)", FALSE),
    uiOutput("exp_links_note"),
    downloadButton("download_exp_links", "Download CSV", class = "btn-primary")),
  tags$section(
    class = "ex-section",
    tags$h2("Warning area exposure"),
    tags$p("Each flood warning area and flood alert area with the people, households and people in a High or Medium rivers and sea band inside it. Restricted: handle as the data is classified."),
    downloadButton("download_exp_areas", "Download CSV", class = "btn-primary")),
  tags$section(
    class = "ex-section",
    tags$h2("Warning areas by responder"),
    tags$p("One row per area and the police force, district, county, ward, constituency or region it lies in, with the people in each. Restricted."),
    downloadButton("download_exp_responders", "Download CSV", class = "btn-primary")),
  tags$section(
    class = "ex-section",
    tags$h2("Warning areas by flood band"),
    tags$p("One row per area and rivers and sea band: the people and households in it. Restricted."),
    downloadButton("download_exp_bands", "Download CSV", class = "btn-primary")),
  tags$section(
    class = "ex-section",
    tags$h2("Constituencies and MP contact details"),
    tags$p("Every constituency with its nation, MP, party, email address and phone number."),
    uiOutput("exp_cons_note"),
    downloadButton("download_exp_cons", "Download CSV", class = "btn-primary")),
  tags$section(
    class = "ex-section",
    tags$h2("Emails for the current Flood Guidance Statement"),
    tags$p("The briefing drafts for the latest statement, one row per MP: the address, subject and body, with the worst risk and the draft's status. ",
           "These are drafts. Nothing has been sent, and the sender and sign-off are placeholders to complete."),
    uiOutput("exp_mail_note"),
    downloadButton("download_exp_mail", "Download CSV", class = "btn-primary"))
)


# -----------------------------------------------------------------------------
# MP audit log tab (demo mode only).
#
# Shows the review actions taken on briefing drafts in THIS session. It is a
# demonstration of the workflow the live app does not have yet. Nothing is
# saved, and nothing is sent or written to any warehouse.
# -----------------------------------------------------------------------------

make_audit_tab <- function() tags$div(
  tags$h1(class = "app-title", "MP audit log"),
  tags$div(class = "quiet-notice",
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
    tabPanel("Rapid Flood Guidance",   make_rfg_tab()),
    tabPanel("Warning areas",          make_areas_tab()),
    tabPanel("Target Area",            make_area_detail_tab()),
    tabPanel("Constituency exposure",  make_const_exposure_tab()),
    tabPanel("Exposure history",       make_exposure_tab()),
    tabPanel("Warning history",        make_area_history_tab()),
    tabPanel("Constituency history",   make_warning_history_tab()),
    tabPanel("Warnings vs FGS",        make_coverage_tab()),
    tabPanel("Hydrometrics",           make_hydrometrics_tab()),
    tabPanel("Gauges near thresholds", make_threshold_board_tab()),
    tabPanel("Gauge value",            make_gauge_value_tab()),
    tabPanel("Rain gauges",            make_rain_tab()),
    tabPanel("Exports",                make_exports_tab()),
    tabPanel("Guide",                  make_guide_tab()),
    tabPanel("Planned",                make_planned_tab(DEMO_MODE)),
    if (DEMO_MODE) tabPanel("MP audit log", make_audit_tab()),
    if (DEMO_MODE) tabPanel("Style guide",  make_styleguide_tab())
  )

  bslib::page_fluid(
    theme = app_theme(),
    tags$head(tags$title(if (DEMO_MODE) "Flood Intelligence Tool (demonstration)" else "Flood Intelligence Tool"),
              tags$style(HTML(paste(app_css_variables(), custom_css, design_css(), areas_css()))),
              tags$script(HTML(app_nav_js()))),
    if (DEMO_MODE) tags$div(class = "demo-ribbon", "Demonstration: synthetic data. Nothing is connected, saved or sent."),
    tags$a(class = "skip-link", href = "#main-content", "Skip to main content"),
    app_masthead(),
    app_navigation(DEMO_MODE),
    if (DEMO_MODE) make_demo_controls(),
    # Shown only when the pipeline is failing, stale or unreadable.
    uiOutput("stale_banner"),
    # Shown only when a pipeline table has not been built yet.
    uiOutput("data_gaps_banner"),

    tags$main(
      id = "main-content", class = "app-container app-main",
      do.call(tabsetPanel, c(list(id = "main_tabs"), Filter(Negate(is.null), tabs)))
    ),
    app_footer()
  )
}
