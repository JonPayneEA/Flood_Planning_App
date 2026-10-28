# =============================================================================
# design.R
# The design system and the page shell for the Flood Intelligence Tool.
#
# WHAT THIS IS. A small design system after the GOV.UK Design System: Arial, the GOV.UK
# type scale and spacing, plain links, square form controls with a heavy border, the yellow
# focus state, flat buttons, tables with hairlines, inset text for notices, and a page that
# is mostly white with generous space. The BRAND is kept: the navy and sky blues and the
# house-on-waves mark in the masthead. The FGS risk colours are never touched here; they
# carry a fixed meaning and stay in the data (see theme.R).
#
# WHAT IT IS NOT. A rewrite. theme.R still holds the palette and the chart theme, and the
# rules in app_ui.R still style the components. design_css() is added AFTER them, so it
# overrides them one component at a time, and a component it does not mention keeps its
# old look.
#
# THE SHELL. A navy masthead (mark, name, data age, copy link, pipeline health), then a
# service navigation of five sections, then, for a section with several views, a sub
# navigation. The views are the same tabPanels as before: Shiny's own tab strip is hidden and the
# links click the hidden tabs, so every observer that reads input$main_tabs, bookmarking
# and the tests carry on unchanged. APP_SECTIONS is the one list of what sits where.
#
# COLOURS AND TYPE are tokens at the top of design_css(); change them there. The contrast
# of every pair is checked in tests/testthat/test-design.R.
# =============================================================================

# GOV.UK colours, with the brand navy and sky from theme.R on top. Text colours are
# checked against their backgrounds in test-design.R.
GOV_COLOURS <- list(
  text        = "#0B0C0C",   # body text
  secondary   = "#505A5F",   # secondary text, hints, captions
  border      = "#B1B4B6",   # borders and hairlines
  grey        = "#F3F2F1",   # light grey bands and panels
  grey_hover  = "#DBDAD9",   # secondary button on hover
  link        = "#1D70B8",   # links
  link_hover  = "#003078",
  focus       = "#FFDD00",   # the focus state
  error       = "#D4351C",
  success     = "#00703C",
  white       = "#FFFFFF"
)

# Contrast pairs that must hold (foreground, background, minimum ratio). test-design.R checks them.
DESIGN_CONTRAST_PAIRS <- list(
  list("text",      "white",  7),
  list("text",      "grey",   7),
  list("secondary", "white",  4.5),
  list("secondary", "grey",   4.5),
  list("link",      "white",  4.5),
  list("link",      "grey",   4.5),
  list("link_hover", "white", 7),
  list("error",     "white",  4.5),
  list("success",   "white",  4.5),
  list("text",      "focus",  7)
)

# Which view sits in which section. A view is the value of a tabPanel in the hidden main_tabs
# tabset, and `label` is what the navigation shows. Order is the order shown. Every value here
# must be a real tab, and every tab must be here (tests/testthat/test-design.R checks both).
# Views listed in `demo` appear only in demonstration mode.
APP_SECTIONS <- list(
  list(id = "now", label = "Now",
       intro = "What the forecast says and who it reaches.",
       views = list(list(value = "Overview",         label = "Overview"),
                    list(value = "FGS Interrogator", label = "Forecast map"))),
  list(id = "areas", label = "Warning areas",
       intro = "Who lives inside each flood warning and flood alert area, and how much of that is at risk from rivers and the sea.",
       views = list(list(value = "Warning areas", label = "Warning areas"),
                    list(value = "Area detail", label = "Area detail"),
                    list(value = "Constituency exposure", label = "Constituency exposure"))),
  list(id = "constituencies", label = "Constituencies and briefings",
       intro = "Each constituency's forecast, its warnings and its MP briefing.",
       views = list(list(value = "Constituency situation", label = "Constituencies and briefings"))),
  list(id = "observed", label = "Observed",
       intro = "What the Environment Agency's gauges are recording.",
       views = list(list(value = "Rain gauges",  label = "Rain gauges"),
                    list(value = "Hydrometrics", label = "River levels and flows"))),
  list(id = "history", label = "History",
       intro = "How often and how long warnings were in force, and how well the forecast matched them.",
       views = list(list(value = "Warning history",   label = "Warning history"),
                    list(value = "Exposure history",  label = "Exposure history"),
                    list(value = "Warnings vs FGS",   label = "Warnings and the forecast"))),
  list(id = "exports", label = "Exports",
       intro = "Lists to take away: areas by constituency, MP contact details and the current briefing emails.",
       views = list(list(value = "Exports", label = "Exports"))),
  list(id = "help", label = "Help",
       intro = "How the figures are made, and what is planned.",
       views = list(list(value = "Guide",   label = "Guide"),
                    list(value = "Planned", label = "Planned"),
                    list(value = "MP audit log",  label = "MP audit log",  demo = TRUE),
                    list(value = "Style guide",   label = "Style guide",   demo = TRUE)))
)

# The sections with the demonstration-only views dropped when not in demonstration mode.
app_sections <- function(demo = FALSE) {
  lapply(APP_SECTIONS, function(s) {
    s$views <- Filter(function(v) isTRUE(demo) || !isTRUE(v$demo), s$views)
    s
  })
}

# Every tab value the shell knows, in order.
app_view_values <- function(demo = FALSE) {
  unlist(lapply(app_sections(demo), function(s) vapply(s$views, function(v) v$value, character(1))))
}

# The CSS. Tokens are written {{name}} and filled from GOV_COLOURS and APP_PALETTE, so a colour is changed in one place.
design_css <- function() {
  css <- r"---(
/* ===== Design system: tokens ============================================== */
:root {
  --gov-text: {{text}}; --gov-secondary: {{secondary}}; --gov-border: {{border}}; --gov-grey: {{grey}};
  --gov-grey-hover: {{grey_hover}}; --gov-link: {{link}}; --gov-link-hover: {{link_hover}};
  --gov-focus: {{focus}}; --gov-error: {{error}}; --gov-success: {{success}};
  --gov-container: 1280px; --gov-gutter: 30px;
  --sp-1: 5px; --sp-2: 10px; --sp-3: 15px; --sp-4: 20px; --sp-5: 25px; --sp-6: 30px; --sp-7: 40px; --sp-8: 50px; --sp-9: 60px;
}

/* ===== Base: type, links, focus =========================================== */
html { font-size: 100%; }
body {
  background: #FFFFFF; color: var(--gov-text);
  font-family: Arial, 'Helvetica Neue', Helvetica, sans-serif;
  font-size: 19px; line-height: 1.3158; -webkit-font-smoothing: antialiased;
}
@media (max-width: 40.0525em) { body { font-size: 16px; line-height: 1.25; } }
h1, h2, h3, h4, h5, h6 { color: var(--gov-text); font-weight: 700; margin-top: 0; }
h1, .app-title { font-size: 36px; line-height: 1.1111; margin-bottom: var(--sp-6); letter-spacing: 0; }
h2 { font-size: 24px; line-height: 1.25; margin-bottom: var(--sp-4); }
h3 { font-size: 19px; line-height: 1.3158; margin-bottom: var(--sp-3); }
h4, h5, h6 { font-size: 16px; line-height: 1.25; margin-bottom: var(--sp-2); }
@media (max-width: 40.0525em) { h1, .app-title { font-size: 27px; } h2 { font-size: 21px; } }
p { margin-top: 0; margin-bottom: var(--sp-4); }
.app-lead { font-size: 24px; line-height: 1.25; margin-bottom: var(--sp-6); }
@media (max-width: 40.0525em) { .app-lead { font-size: 19px; } }
.text-muted, .app-caption, small, .small { color: var(--gov-secondary) !important; }
.app-caption { font-size: 16px; }
small, .small { font-size: 16px; }

a { color: var(--gov-link); text-decoration: underline; text-decoration-thickness: max(1px, .0625rem); text-underline-offset: .1578em; }
a:hover { color: var(--gov-link-hover); text-decoration-thickness: max(3px, .1875rem, .12em); }
a:focus, a:focus-visible {
  outline: 3px solid transparent; color: var(--gov-text); background-color: var(--gov-focus);
  box-shadow: 0 -2px var(--gov-focus), 0 4px var(--gov-text); text-decoration: none;
}
:focus-visible { outline: 3px solid var(--gov-focus); outline-offset: 0; box-shadow: inset 0 0 0 2px var(--gov-text); }
@media (prefers-reduced-motion: reduce) { * { animation-duration: .01ms !important; transition-duration: .01ms !important; } }

.skip-link { position: absolute; left: -9999px; top: 0; z-index: 3000; padding: var(--sp-2) var(--sp-3); background: var(--gov-focus); color: var(--gov-text); font-weight: 700; }
.skip-link:focus { left: 0; }

/* ===== Shell: masthead, navigation, main, footer ========================== */
body > .container-fluid { padding-left: 0; padding-right: 0; }
.app-container { max-width: var(--gov-container); margin: 0 auto; padding: 0 var(--gov-gutter); }
@media (max-width: 40.0525em) { .app-container { padding: 0 var(--sp-3); } }

.app-masthead { background: {{navy}}; color: #FFFFFF; border-bottom: 6px solid {{sky}}; }
.app-masthead .masthead-inner { display: flex; align-items: center; justify-content: space-between; gap: var(--sp-4); flex-wrap: wrap; padding-top: var(--sp-3); padding-bottom: var(--sp-3); }
.masthead-brand { display: flex; align-items: center; gap: var(--sp-3); color: #FFFFFF; text-decoration: none; }
.masthead-brand:hover { color: #FFFFFF; text-decoration: none; }
.masthead-brand:focus, .masthead-brand:focus-visible { color: var(--gov-text); }
.masthead-brand .brand-mark { height: 46px; margin: 0; }
.masthead-name { font-size: 30px; line-height: 1.0; font-weight: 700; margin: 0; color: inherit; }
.masthead-name .hl { color: {{cyan}}; }
.masthead-brand:focus .masthead-name .hl { color: inherit; }
.masthead-sub { font-size: 16px; color: {{ice}}; margin-top: 3px; }
.masthead-meta { display: flex; align-items: center; gap: var(--sp-3); font-size: 16px; flex-wrap: wrap; }
.masthead-meta .asof { color: #FFFFFF; font-size: 16px; }
.masthead-meta .btn-outline-secondary { background: transparent; color: #FFFFFF; border: 2px solid #FFFFFF; box-shadow: none; }
.masthead-meta .btn-outline-secondary:hover { background: #FFFFFF; color: {{navy}}; }
@media (max-width: 40.0525em) { .masthead-name { font-size: 22px; } .masthead-sub { display: none; } }

.app-nav { background: #FFFFFF; border-bottom: 1px solid var(--gov-border); }
.app-nav ul, .app-subnav ul { list-style: none; margin: 0; padding: 0; display: flex; flex-wrap: wrap; gap: 0 var(--sp-6); }
.app-nav a { display: block; padding: var(--sp-3) 0 11px; font-size: 19px; font-weight: 400; color: var(--gov-text); text-decoration: none; border-bottom: 4px solid transparent; }
.app-nav a:hover { color: var(--gov-link-hover); border-bottom-color: var(--gov-border); }
.app-nav a[aria-current='page'] { font-weight: 700; border-bottom-color: {{navy}}; }
.app-nav a:focus, .app-nav a:focus-visible { border-bottom-color: var(--gov-text); }
@media (max-width: 40.0525em) { .app-nav ul { flex-direction: column; } .app-nav a { padding: var(--sp-2) 0; border-bottom: 0; border-left: 4px solid transparent; padding-left: var(--sp-2); } .app-nav a[aria-current='page'] { border-left-color: {{navy}}; } }

.app-subnav { background: var(--gov-grey); border-bottom: 1px solid var(--gov-border); }
.app-subnav[hidden], .app-subnav ul[hidden] { display: none; }
.app-subnav a { display: block; padding: var(--sp-2) 0; font-size: 16px; }
.app-subnav a[aria-current='page'] { color: var(--gov-text); font-weight: 700; text-decoration: none; border-bottom: 3px solid {{navy}}; padding-bottom: 7px; }
.app-subnav .section-intro { font-size: 16px; color: var(--gov-secondary); padding: var(--sp-2) 0; margin: 0; flex-basis: 100%; }

.app-context { background: #FFFFFF; padding-top: var(--sp-3); }
.app-main { padding-top: var(--sp-6); padding-bottom: var(--sp-9); }
ul#main_tabs, ul#main_tabs.nav { display: none !important; }                       /* the shell's own navigation replaces Shiny's tab strip */
ul#main_tabs + .tab-content { padding: 0; }

.app-footer { margin-top: var(--sp-9); background: var(--gov-grey); border-top: 1px solid var(--gov-border); padding: var(--sp-6) 0 var(--sp-7); font-size: 16px; }
.app-footer p { margin-bottom: var(--sp-2); }
.app-footer ul { list-style: none; padding: 0; margin: 0 0 var(--sp-4); display: flex; flex-wrap: wrap; gap: var(--sp-2) var(--sp-5); }
.app-footer .licence { color: var(--gov-secondary); }

/* The demonstration ribbon becomes a phase banner. */
.demo-ribbon { background: var(--gov-grey); color: var(--gov-text); text-transform: none; letter-spacing: 0; font-size: 16px; font-weight: 400; text-align: left; padding: var(--sp-2) var(--gov-gutter); margin: 0; border-bottom: 1px solid var(--gov-border); }
.demo-ribbon::before { content: 'Demonstration'; background: {{navy}}; color: #FFFFFF; font-weight: 700; text-transform: uppercase; font-size: 14px; letter-spacing: .03em; padding: 2px 8px; margin-right: var(--sp-3); }
.demo-controls { background: var(--gov-grey); border-bottom: 1px solid var(--gov-border); font-size: 16px; padding-left: var(--gov-gutter); padding-right: var(--gov-gutter); }

/* ===== Components ========================================================= */
/* Form controls: square, a 2px dark border, and the yellow focus. */
label, .control-label { font-size: 16px; font-weight: 700; color: var(--gov-text); margin-bottom: var(--sp-1); }
.form-control, .form-select, .selectize-input, .selectize-control.single .selectize-input {
  border: 2px solid var(--gov-text); border-radius: 0; box-shadow: none; font-size: 16px; min-height: 40px; color: var(--gov-text); background-color: #FFFFFF;
}
.selectize-input.focus, .form-control:focus, .form-select:focus { border-color: var(--gov-text); box-shadow: inset 0 0 0 2px var(--gov-text); outline: 3px solid var(--gov-focus); outline-offset: 0; }
.selectize-dropdown { border: 2px solid var(--gov-text); border-radius: 0; font-size: 16px; }
.selectize-dropdown .active { background: {{navy}}; color: #FFFFFF; }
.selectize-control.multi .selectize-input > div { border-radius: 0; background: var(--gov-grey); color: var(--gov-text); border: 1px solid var(--gov-border); }

/* Buttons: flat, with a bar underneath. */
.btn { border-radius: 0; font-size: 16px; font-weight: 400; min-height: 40px; padding: 8px 10px 7px; line-height: 1.25; }
.btn-primary { background: {{navy}}; border-color: {{navy}}; color: #FFFFFF; box-shadow: 0 2px 0 {{navy_deep}}; }
.btn-primary:hover { background: {{navy_deep}}; border-color: {{navy_deep}}; }
.btn-outline-secondary { background: var(--gov-grey); color: var(--gov-text); border: 2px solid transparent; box-shadow: 0 2px 0 #929191; }
.btn-outline-secondary:hover { background: var(--gov-grey-hover); color: var(--gov-text); border-color: transparent; }
.btn:focus-visible { outline: 3px solid var(--gov-focus); box-shadow: 0 2px 0 var(--gov-text); background-color: var(--gov-focus); color: var(--gov-text); }

/* Tables: a hairline under each row, a heavier rule under the header, no boxes. */
table.dataTable, .table, .ov-table { font-size: 16px; border-collapse: collapse; }
table.dataTable thead th, .table thead th, .ov-table th { font-weight: 700; color: var(--gov-text); border-bottom: 2px solid var(--gov-text); padding: var(--sp-2) var(--sp-4) var(--sp-2) 0; }
table.dataTable tbody td, .table tbody td, .ov-table td { padding: var(--sp-2) var(--sp-4) var(--sp-2) 0; border-bottom: 1px solid var(--gov-border); }
table.dataTable.stripe tbody tr.odd, table.dataTable.stripe > tbody > tr.odd > * { background: transparent; box-shadow: none; }
.dataTables_wrapper { font-size: 16px; }

/* Panels: no boxes. A heading and space do the grouping. */
.ov-panel, .panel-section { background: transparent; border: 0; padding: 0; }
.ov-panel h6, .panel-h { font-size: 19px; font-weight: 700; text-transform: none; letter-spacing: 0; color: var(--gov-text); border-bottom: 0; margin: 0 0 var(--sp-3); padding: 0; }
.ov-day { background: var(--gov-grey); border: 0; border-radius: 0; }

/* Key figures: a rule on top, a large number, a plain label. */
div.stat-card { background: transparent; border: 0; border-top: 5px solid {{navy}}; border-radius: 0; padding: var(--sp-3) 0 0; text-align: left; }
div.stat-card .stat-num { font-size: 36px; font-weight: 700; line-height: 1.1; color: var(--gov-text); }
div.stat-card .stat-num.accent { color: {{navy}}; }
div.stat-card .stat-label { font-size: 16px; text-transform: none; letter-spacing: 0; color: var(--gov-secondary); margin-top: var(--sp-1); }

/* Notices: inset text, with a coloured bar. The risk-coloured cards keep the bar colour they set inline. */
.ov-headline { background: transparent; border: 0; border-left: 10px solid {{navy}}; border-radius: 0; padding: var(--sp-1) 0 var(--sp-1) var(--sp-4); margin-bottom: var(--sp-6); }
.ov-headline .ov-kicker { font-size: 16px; text-transform: none; letter-spacing: 0; color: var(--gov-secondary); }
.ov-headline .ov-text { font-size: 24px; line-height: 1.25; margin-top: var(--sp-1); }
.quiet-notice, .gaps-banner { background: var(--gov-grey); border: 0; border-left: 5px solid {{navy}}; border-radius: 0; font-size: 16px; padding: var(--sp-3) var(--sp-4); }
.stale-banner { border-radius: 0; font-size: 16px; }
.headline-notice, .source-info-card, .changes-strip, .briefing-body, .empty-state { background: var(--gov-grey); border-radius: 0; border-top: 0; border-right: 0; border-bottom: 0; }
.briefing-body, .empty-state { border-left: 0; }
.empty-state { border: 0; color: var(--gov-secondary); }
.source-info-label, .changes-strip .chg-group td, .legend-row { text-transform: none; letter-spacing: 0; font-size: 16px; }
.shiny-sidebar { background: var(--gov-grey); }
/* The nation filter sits in the masthead: a label and a compact control on one line. */
.masthead-right { flex: 1 1 0; min-width: 0; display: flex; flex-direction: column; align-items: flex-end; gap: var(--sp-2); }
.masthead-right .masthead-meta { justify-content: flex-end; gap: var(--sp-2); }
.masthead-right .masthead-meta .asof, .masthead-right .masthead-meta .asof * { font-size: 14px; }
@media (min-width: 64em) { .masthead-right .masthead-meta { flex-wrap: nowrap; } .masthead-right .btn, .masthead-right .health-badge { white-space: nowrap; } }
@media (max-width: 90em) { .masthead-sub { display: none; } }
@media (min-width: 64em) { .app-masthead .masthead-inner { flex-wrap: nowrap; } .masthead-brand { flex: 0 0 auto; } }
@media (max-width: 40.0525em) { .masthead-right { align-items: flex-start; flex-basis: 100%; } }
.masthead-filter .nation-strip { display: flex; align-items: center; gap: var(--sp-3); padding: 0; flex-wrap: wrap; }
.masthead-filter .form-group { display: flex; align-items: center; gap: var(--sp-3); margin: 0; }
.masthead-filter label { color: #FFFFFF; margin: 0; white-space: nowrap; font-size: 16px; }
.masthead-filter .selectize-input { min-height: 36px; padding-top: 4px; padding-bottom: 4px; }
.filter-row { margin-bottom: var(--sp-4); }
.health-badge { border-radius: 0; font-size: 16px; }

/* Sliders and checkboxes keep the brand navy. */
.irs--shiny .irs-bar, .irs--shiny .irs-handle, .irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to { background: {{navy}}; border-color: {{navy}}; }
.form-check-input:checked { background-color: {{navy}}; border-color: {{navy}}; }

/* ===== Overview =========================================================== */
.ov-page { max-width: 100%; }
.ov-headline .ov-text { font-size: 24px; line-height: 1.25; font-weight: 700; margin: var(--sp-1) 0 var(--sp-3); }
.ov-headline .ov-risk { font-size: 19px; }
.ov-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(220px, 1fr)); gap: var(--sp-6); margin-bottom: var(--sp-8); }
button.stat-btn { display: block; width: 100%; font: inherit; color: inherit; cursor: pointer; text-align: left; background: transparent; border: 0; border-top: 5px solid {{navy}}; padding: var(--sp-3) 0 var(--sp-2); }
button.stat-btn:hover { background: var(--gov-grey); }
button.stat-btn.open { background: var(--gov-grey); }
button.stat-btn .stat-num { display: block; font-size: 36px; font-weight: 700; line-height: 1.1; }
button.stat-btn .stat-label { display: block; font-size: 16px; color: var(--gov-secondary); margin-top: var(--sp-1); }
button.stat-btn .stat-hint { display: block; font-size: 16px; color: var(--gov-link); text-decoration: underline; margin-top: var(--sp-1); }
.ov-inforce-list { margin-top: var(--sp-4); overflow-x: auto; }
.ov-section { margin-bottom: var(--sp-8); }
.ov-section > h2 { margin-bottom: var(--sp-2); }
.ov-outlook { display: grid; grid-template-columns: repeat(5, 1fr); gap: var(--sp-4); }
.ov-day { background: transparent; border: 0; border-top: 2px solid var(--gov-text); border-radius: 0; padding: var(--sp-2) 0 0; text-align: left; }
.ov-day .d { font-size: 16px; font-weight: 700; color: var(--gov-text); margin-bottom: var(--sp-2); }
.ov-day .n { font-size: 16px; color: var(--gov-secondary); }
.ov-cols { display: grid; grid-template-columns: 3fr 2fr; gap: var(--sp-8); }
.ov-table { width: 100%; }
.ov-table tr.clickable:hover { background: var(--gov-grey); }
.ov-foot { font-size: 16px; color: var(--gov-secondary); margin-top: var(--sp-3); }
@media (max-width: 64em) { .ov-cols { grid-template-columns: 1fr; gap: 0; } }
@media (max-width: 40.0525em) { .ov-outlook { grid-template-columns: 1fr 1fr; } .ov-grid { gap: var(--sp-4); } }

/* Disclosure: a summary link that opens a panel. Used for secondary filters and notes. */
.app-details { margin: 0 0 var(--sp-4); }
.app-details > summary { color: var(--gov-link); text-decoration: underline; cursor: pointer; font-size: 16px; display: inline-block; margin-bottom: var(--sp-2); }
.app-details > summary:hover { color: var(--gov-link-hover); }
.app-details[open] > summary { margin-bottom: var(--sp-3); }
.app-details > :not(summary) { border-left: 5px solid var(--gov-border); padding-left: var(--sp-4); }
.rain-page .filter-row { margin-bottom: var(--sp-3); }
.rain-page p.mb-1 { margin-bottom: var(--sp-4) !important; }
.rain-page .leaflet-container { border: 1px solid var(--gov-border); }

.rt-page > h2 { margin-top: var(--sp-6); margin-bottom: var(--sp-1); }
.rt-page .filter-row { margin-bottom: var(--sp-3); }
.rt-page p.text-muted.mb-0 { margin-top: var(--sp-3); }

/* Pages with several tables: a heading and space between them, in place of the old horizontal rules. */
.sit-briefing-h { margin-top: var(--sp-8); }
.sit-page > h2, .wh-page > h2, .cov-page > h2 { margin-top: var(--sp-6); margin-bottom: var(--sp-2); }
.sit-page .filter-row, .wh-page .filter-row, .cov-page .filter-row, .exp-page .row { margin-bottom: var(--sp-3); }
.cov-page .app-caption { margin-bottom: var(--sp-3); }
.guide h2 { margin-top: var(--sp-7); }

/* Warning icons beside the in-force counts. */
.warn-icon { height: 44px; width: auto; max-width: 56px; object-fit: contain; flex: 0 0 auto; }
.warn-icon[viewBox] { width: 44px; }
button.stat-btn .stat-top { display: flex; align-items: center; gap: var(--sp-3); }
.ov-list-h { display: flex; align-items: center; gap: var(--sp-3); }
.ov-list-h .warn-icon { height: 32px; width: auto; }
.ov-list-h .warn-icon[viewBox] { width: 32px; }

.ex-section { margin-bottom: var(--sp-8); max-width: 48em; }
.ex-section .btn { margin-top: var(--sp-2); }
.ex-section .shiny-input-container { width: auto; max-width: 100%; }

/* Style guide (demonstration mode). */
.sg-swatches { display: flex; flex-wrap: wrap; gap: var(--sp-3); margin-bottom: var(--sp-6); }
.sg-swatch { width: 170px; font-size: 16px; }
.sg-swatch .chip { height: 56px; border: 1px solid var(--gov-border); margin-bottom: var(--sp-1); }
.sg-section { margin-bottom: var(--sp-8); }
.sg-row { display: flex; flex-wrap: wrap; gap: var(--sp-4); align-items: flex-end; margin-bottom: var(--sp-4); }
)---"
  p <- APP_PALETTE
  vals <- c(GOV_COLOURS, list(navy = p$navy, navy_deep = p$navy_deep, sky = p$sky, cyan = p$cyan, ice = p$ice))
  for (n in names(vals)) css <- gsub(paste0("{{", n, "}}"), vals[[n]], css, fixed = TRUE)
  css
}

# The script that makes the navigation work. The links click Shiny's own (hidden) tab anchors, so input$main_tabs, bookmarking and every
# observer behave as they did. It also remembers the last view each section was left on, and keeps the current link marked.
app_nav_js <- function() {
  r"---(
(function () {
  var last = {};
  function tabAnchor(v) {
    var as = document.querySelectorAll('#main_tabs a[data-value]');
    for (var i = 0; i < as.length; i++) { if (as[i].getAttribute('data-value') === v) return as[i]; }
    return null;
  }
  function mark(v) {
    var link = document.querySelector('.app-subnav a[data-view="' + v + '"]');
    if (!link) return;
    var sec = link.getAttribute('data-section');
    last[sec] = v;
    document.querySelectorAll('.app-nav a').forEach(function (a) {
      if (a.getAttribute('data-section') === sec) a.setAttribute('aria-current', 'page'); else a.removeAttribute('aria-current');
    });
    document.querySelectorAll('.app-subnav ul').forEach(function (u) { u.hidden = (u.getAttribute('data-section') !== sec); });
    var bar = document.querySelector('.app-subnav'); if (bar) bar.hidden = false;
    document.querySelectorAll('.app-subnav a').forEach(function (a) {
      if (a.getAttribute('data-view') === v) a.setAttribute('aria-current', 'page'); else a.removeAttribute('aria-current');
    });
    var tit = document.querySelector('.app-subnav ul:not([hidden]) .section-intro'); if (tit) tit.hidden = false;
  }
  function go(v) { var a = tabAnchor(v); if (a) { a.click(); mark(v); } }
  document.addEventListener('click', function (e) {
    var el = e.target.closest ? e.target.closest('[data-view], [data-section-link]') : null;
    if (!el) return;
    e.preventDefault();
    if (el.hasAttribute('data-section-link')) {
      var sec = el.getAttribute('data-section-link');
      go(last[sec] || el.getAttribute('data-first'));
    } else {
      go(el.getAttribute('data-view'));
    }
  });
  function current() {
    var a = document.querySelector('#main_tabs li.active a, #main_tabs a.active');
    return a ? a.getAttribute('data-value') : null;
  }
  document.addEventListener('shown.bs.tab', function (e) { var v = e.target && e.target.getAttribute && e.target.getAttribute('data-value'); if (v) mark(v); });
  if (window.jQuery) {
    jQuery(document).on('shiny:connected', function () { var v = current(); if (v) mark(v); });
    jQuery(document).on('shown.bs.tab', 'a[data-toggle="tab"], a[data-bs-toggle="tab"]', function (e) { var v = jQuery(e.target).attr('data-value'); if (v) mark(v); });
  }
  document.addEventListener('DOMContentLoaded', function () { var v = current() || 'Overview'; mark(v); });
})();
)---"
}

# The masthead: mark, name, then the data age, copy link and pipeline health the app already had.
app_masthead <- function() {
  tags$header(
    class = "app-masthead",
    tags$div(
      class = "app-container masthead-inner",
      tags$a(href = "#main-content", class = "masthead-brand", `data-view` = "Overview",
             app_logo(),
             tags$div(tags$p(class = "masthead-name", "Flood ", tags$span(class = "hl", "Intelligence"), " Tool"),
                      tags$div(class = "masthead-sub", "Flood Guidance Statement: forecast, constituencies and flood areas"))),
      tags$div(
        class = "masthead-right",
        tags$div(class = "masthead-filter", make_nation_strip()),
        tags$div(
          class = "masthead-meta",
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
      )
    )
  )
}

# Service navigation (sections) and the sub navigation (the views of the section you are in).
app_navigation <- function(demo = FALSE) {
  secs <- app_sections(demo)
  top <- tags$nav(
    class = "app-nav", `aria-label` = "Sections",
    tags$div(class = "app-container",
             tags$ul(lapply(secs, function(s)
               tags$li(tags$a(href = "#", `data-section` = s$id, `data-section-link` = s$id,
                              `data-first` = s$views[[1L]]$value, s$label)))))
  )
  sub <- tags$nav(
    class = "app-subnav", `aria-label` = "Views", hidden = NA,
    tags$div(class = "app-container",
             lapply(secs, function(s)
               tags$ul(`data-section` = s$id, hidden = NA,
                       tags$li(class = "section-intro", s$intro),
                       lapply(s$views, function(v)
                         tags$li(tags$a(href = "#", `data-view` = v$value, `data-section` = s$id, v$label))))))
  )
  tagList(top, sub)
}

# The footer: attribution and licence, and a way to the Guide.
app_footer <- function() {
  tags$footer(
    class = "app-footer", role = "contentinfo",
    tags$div(
      class = "app-container",
      tags$ul(tags$li(tags$a(href = "#", `data-view` = "Guide", "How the figures are made")),
              tags$li(tags$a(href = "#", `data-view` = "Planned", "What is planned"))),
      tags$p("This uses Environment Agency flood and river level data from the real-time data API (Beta)."),
      tags$p(class = "licence",
             "All content is available under the ",
             tags$a(href = "https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", "Open Government Licence v3.0"),
             ", except where otherwise stated. Flood Guidance Statement text and risk areas are from the Met Office and the Environment Agency's Flood Forecasting Centre. ", HTML("&copy;"), " Crown copyright.")
    )
  )
}

# The "Planned" page: the placeholders that were four tabs in a menu.
make_planned_tab <- function(demo = FALSE) tags$div(
  tags$h1(class = "app-title", "Planned"),
  tags$p(class = "app-lead", "These views are not built yet. Each waits on data the pipeline does not hold."),
  tags$ul(
    tags$li(tags$strong("Warning issue times. "), "The history is held (see Warning history). A view of the hour and day warnings are issued is still to come."),
    tags$li(tags$strong("Forecast verification. "), "Needs observed gauge levels and a decision on the Met Office licence."),
    tags$li(tags$strong("Event playback. "), "Needs observed gauge levels and a decision on the Met Office licence."),
    if (!demo) tags$li(tags$strong("MP audit log. "), "Needs a record of who sent what.")
  )
)

# The style guide: every part of the design system on one page, for review. Demonstration mode only.
make_styleguide_tab <- function() {
  sw <- function(name, hex, note = "") tags$div(class = "sg-swatch", tags$div(class = "chip", style = paste0("background:", hex)),
                                                 tags$strong(name), tags$br(), hex, if (nzchar(note)) tagList(tags$br(), tags$span(class = "app-caption", note)))
  p <- APP_PALETTE
  tags$div(
    tags$h1(class = "app-title", "Style guide"),
    tags$p(class = "app-lead", "The parts of the design system, after the GOV.UK Design System, in the brand blue. This page appears in demonstration mode only."),
    tags$div(class = "sg-section", tags$h2("Type"),
      tags$p(class = "app-caption", "Arial. Body 19 pixels (16 on a phone), headings 36, 24 and 19, captions and controls 16."),
      tags$h1(class = "app-title", "Page title, 36 pixels"),
      tags$p(class = "app-lead", "A lead paragraph, 24 pixels. It says what the page tells you before any control or table."),
      tags$h2("Section heading, 24 pixels"),
      tags$p("Body text, 19 pixels, line height about 1.3. Lines stay under about 70 characters where the page allows. ", tags$a(href = "#", "A link looks like this"), "."),
      tags$h3("Sub-heading, 19 pixels bold"),
      tags$p(class = "app-caption", "A caption or hint, 16 pixels, secondary colour.")),
    tags$div(class = "sg-section", tags$h2("Colour"),
      tags$div(class = "sg-swatches",
        sw("Brand navy", p$navy, "masthead, active navigation, buttons"), sw("Brand sky", p$sky, "the rule under the masthead"),
        sw("Text", GOV_COLOURS$text), sw("Secondary text", GOV_COLOURS$secondary), sw("Border", GOV_COLOURS$border), sw("Light grey", GOV_COLOURS$grey, "bands and notices"),
        sw("Link", GOV_COLOURS$link), sw("Focus", GOV_COLOURS$focus, "the yellow focus state"))),
    tags$div(class = "sg-section", tags$h2("Buttons and controls"),
      tags$div(class = "sg-row",
        tags$button(type = "button", class = "btn btn-primary", "Primary"),
        tags$button(type = "button", class = "btn btn-outline-secondary", "Secondary"),
        tags$button(type = "button", class = "btn btn-primary", disabled = NA, "Disabled")),
      tags$div(class = "sg-row",
        selectInput("sg_select", "A select", choices = c("One", "Two", "Three"), width = "220px"),
        textInput("sg_text", "A text box", value = "Some text", width = "220px"),
        checkboxInput("sg_check", "A tick box", value = TRUE))),
    tags$div(class = "sg-section", tags$h2("Table"),
      tags$table(class = "table", style = "max-width: 640px;",
        tags$thead(tags$tr(tags$th("Constituency"), tags$th("Risk"), tags$th("Warnings"))),
        tags$tbody(tags$tr(tags$td("Aldwyn"), tags$td("Medium"), tags$td("2")),
                   tags$tr(tags$td("Brenn"), tags$td("High"), tags$td("5")),
                   tags$tr(tags$td("Cardell"), tags$td("Low"), tags$td("0"))))),
    tags$div(class = "sg-section", tags$h2("Key figures"),
      tags$div(class = "kpi-row",
        tags$div(class = "stat-card", tags$div(class = "stat-num", "143"), tags$div(class = "stat-label", "constituencies at Medium or above")),
        tags$div(class = "stat-card", tags$div(class = "stat-num accent", "12"), tags$div(class = "stat-label", "warnings in force")))),
    tags$div(class = "sg-section", tags$h2("Warning icons"),
      tags$p(class = "app-caption", "A house with one, two or three waves. Put the Environment Agency's own files in www/icons (flood-alert, flood-warning, severe-flood-warning, .svg or .png) and they replace these."),
      tags$div(class = "sg-row", lapply(c("alert", "warning", "severe"), function(l) tags$div(warning_icon(l), tags$br(), l)))),
    tags$div(class = "sg-section", tags$h2("Notices"),
      tags$div(class = "ov-headline", tags$div(class = "ov-kicker", "Headline"), tags$div(class = "ov-text", "Inset text carries the main message of a page.")),
      tags$div(class = "quiet-notice", "A quiet notice for a normal state, such as a day with no risk areas."),
      tags$div(class = "stale-banner stale-warn", "A warning banner, for a pipeline that is late."))
  )
}


# Styles for the Warning areas pages: the band bar and chips, the figure tiles, the two-column detail and the popup table.
areas_css <- function() {
  paste0(r"---(
.band-bar { display: flex; height: 18px; width: 200px; background: #F3F2F1; }
.band-bar i { display: block; height: 100%; }
.band-chip { display: inline-block; padding: 1px 8px; font-size: 13px; font-weight: 700; }
.band-legend { display: flex; flex-wrap: wrap; gap: 6px 18px; font-size: 14px; margin: 8px 0 26px; }
.band-legend i { display: inline-block; width: 14px; height: 14px; margin-right: 5px; vertical-align: -2px; border: 1px solid #B1B4B6; }
.kpi-row { display: grid; grid-template-columns: repeat(5, 1fr); gap: 26px; margin: 4px 0 28px; }
.kpi-row > div { border-top: 5px solid #053D7E; padding-top: 10px; }
.kpi-row b { display: block; font-size: 34px; line-height: 1.1; }
.kpi-row span { font-size: 15px; color: #505A5F; }
.area-cols { display: grid; grid-template-columns: 1.15fr 1fr; gap: 34px; }
.area-cols table, .popup-figs { border-collapse: collapse; width: 100%; font-size: 15px; }
.area-cols th { text-align: left; border-bottom: 2px solid #0B0C0C; padding: 6px 8px 6px 0; }
.area-cols td, .popup-figs td { border-bottom: 1px solid #B1B4B6; padding: 7px 8px 7px 0; }
.area-cols td.n, .area-cols th.n, .popup-figs td.n { text-align: right; }
.popup-figs { min-width: 260px; margin-top: 6px; }
.responder-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(420px, 1fr)); gap: 10px 34px; }
.responder-table { border-collapse: collapse; width: 100%; font-size: 15px; }
.responder-table th { text-align: left; border-bottom: 2px solid #0B0C0C; padding: 6px 8px 6px 0; }
.responder-table td { border-bottom: 1px solid #B1B4B6; padding: 7px 8px 7px 0; }
.responder-table td.n, .responder-table th.n { text-align: right; }
@media (max-width: 900px) { .kpi-row { grid-template-columns: repeat(2, 1fr); } .area-cols { grid-template-columns: 1fr; } }
)---")
}
