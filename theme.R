# =============================================================================
# theme.R
# Look and feel for the Flood Intelligence Tool, in one place.
#
# The palette is sampled from the tool's own logo (navy field, white house,
# sky-blue waves). To restyle the app, change this list and nothing else.
# tests/ checks the contrast pairs below, so a bad swap fails a test.
#
# NOT in this file, on purpose: the FGS risk colours (red, amber, yellow, green
# on the matrix, map, chips and heat grid). They come from the FGS User Guide
# and carry a fixed meaning. The brand sits in the frame (header, tabs, buttons,
# surfaces) and the risk colours stay in the data. The frame is blue so that it
# is never mistaken for the "Very Low" risk green.
#
# magenta is the colour of flood warning areas on the map and is not brand.
# =============================================================================

APP_PALETTE <- list(
  navy        = "#053D7E",   # header, white-on-navy text, navy text on light surfaces (11:1 with white)
  navy_deep   = "#01306C",   # header gradient end
  blue        = "#0161BD",   # links, focus, primary buttons (white text 6.2:1)
  bright      = "#038CF4",   # decoration: borders, rules, accents. Not for text
  sky         = "#38C1FB",   # decoration: header rule, stat-card edge. Not for text on white
  cyan        = "#75D2FA",   # text on navy only (title highlight)
  ice         = "#9ED8F8",   # soft borders and chips
  tint        = "#EAF4FC",   # sidebar and quiet surfaces
  magenta     = "#BE0553",   # flood warning areas on the map
  ink         = "#10253F",   # body text
  ink_muted   = "#475A70",   # secondary text
  line        = "#CCDAE8",   # hairlines
  line_strong = "#9FB3C8",
  bg          = "#F3F7FB",   # page background
  surface     = "#FFFFFF"    # cards and panels
)

# Contrast pairs that must hold (foreground, background, minimum ratio).
# Used by tests/testthat/test-theme.R.
APP_CONTRAST_PAIRS <- list(
  list("surface",    "navy",       4.5),
  list("cyan",       "navy",       4.5),
  list("surface",    "blue",       4.5),
  list("ink",        "surface",    7),
  list("ink",        "bg",         7),
  list("ink",        "tint",       7),
  list("ink_muted",  "bg",         4.5),
  list("ink_muted",  "surface",    4.5),
  list("blue",       "surface",    4.5),
  list("blue",       "bg",         4.5),
  list("blue",       "tint",       4.5),
  list("navy",       "surface",    4.5),
  list("navy",       "tint",       4.5),
  list("magenta",    "surface",    4.5)
)

app_contrast <- function(fg, bg) {
  lum <- function(hex) {
    v <- grDevices::col2rgb(hex)[, 1] / 255
    v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4)
    sum(c(0.2126, 0.7152, 0.0722) * v)
  }
  l <- sort(c(lum(fg), lum(bg)), decreasing = TRUE)
  (l[1L] + 0.05) / (l[2L] + 0.05)
}

# Bootstrap 5 theme. System sans-serif stack, so nothing is fetched from the
# network (the app must run on a locked-down estate and the demo offline).
app_theme <- function() {
  p <- APP_PALETTE
  bslib::bs_theme(
    version = 5,
    primary = p$navy, secondary = p$ink_muted, success = p$blue,
    info = p$blue, danger = p$magenta,
    bg = p$surface, fg = p$ink,
    "body-bg" = p$bg, "body-color" = p$ink,
    "link-color" = p$blue, "link-hover-color" = p$navy,
    "font-family-base" = "Arial, 'Helvetica Neue', Helvetica, sans-serif",
    "border-radius" = "4px", "btn-border-radius" = "4px",
    "border-color" = p$line
  )
}

# CSS custom properties used by the rules in app_ui.R. Defined once here.
app_css_variables <- function() {
  p <- APP_PALETTE
  paste0(":root {",
         "--app-navy:", p$navy, ";--app-navy-deep:", p$navy_deep, ";",
         "--app-blue:", p$blue, ";--app-bright:", p$bright, ";",
         "--app-sky:", p$sky, ";--app-cyan:", p$cyan, ";--app-ice:", p$ice, ";",
         "--app-tint:", p$tint, ";--app-magenta:", p$magenta, ";",
         "--app-ink:", p$ink, ";--app-muted:", p$ink_muted, ";",
         "--app-line:", p$line, ";--app-line-strong:", p$line_strong, ";",
         "--app-bg:", p$bg, ";--app-surface:", p$surface, ";",
         "--bs-link-color:", p$blue, ";--bs-body-bg:", p$bg, ";--bs-body-color:", p$ink, ";}")
}

# The logo mark (house on waves), drawn inline so no file has to ship. If an
# approved mark-only file is dropped at www/logo.svg (or .png) it is used instead.
app_logo <- function() {
  for (f in c("logo.svg", "logo.png")) {
    if (file.exists(file.path("www", f))) {
      return(htmltools::tags$img(src = f, alt = "", class = "brand-mark"))
    }
  }
  htmltools::HTML(paste0(
    '<svg class="brand-mark" viewBox="0 0 120 100" role="img" aria-label="Flood Intelligence Tool">',
    '<path d="M60 6 L104 42 H94 V66 H26 V42 H16 Z" fill="#FAFBFB"/>',
    '<rect x="52" y="44" width="16" height="22" rx="2" fill="', APP_PALETTE$navy, '"/>',
    '<path d="M0 70 C25 52 50 52 75 66 S110 80 120 62 V100 H0Z" fill="', APP_PALETTE$sky, '"/>',
    '<path d="M0 80 C30 66 55 70 80 82 S110 90 120 76 V100 H0Z" fill="', APP_PALETTE$blue, '"/>',
    '<path d="M0 92 C35 80 70 86 120 90 V100 H0Z" fill="', APP_PALETTE$navy_deep, '"/></svg>'))
}


# -----------------------------------------------------------------------------
# Warning icons
#
# warning_icon(level) is the symbol for a flood alert, a flood warning or a severe flood warning (level "alert", "warning" or
# "severe"), shown beside the counts and lists. To use the Environment Agency's own artwork, put the files in www/icons/ named
# flood-alert, flood-warning and severe-flood-warning, as .svg or .png: they replace the drawing. Until they are there a plain
# house with one, two or three waves is drawn, in the app's own alert, warning and severe colours (not the official artwork).
# The icon is decoration: the count and its label beside it carry the meaning, so it is hidden from screen readers.
# -----------------------------------------------------------------------------

APP_SEVERE_HEX <- "#6B0233"
WARNING_ICON_FILES <- c(alert = "flood-alert", warning = "flood-warning", severe = "severe-flood-warning")

warning_icon <- function(level = c("alert", "warning", "severe")) {
  level <- match.arg(level)
  stem <- WARNING_ICON_FILES[[level]]
  for (ext in c("svg", "png")) {
    f <- paste0(stem, ".", ext)
    if (file.exists(file.path("www", "icons", f))) {
      return(htmltools::tags$img(src = file.path("icons", f), alt = "", `aria-hidden` = "true", class = paste("warn-icon", paste0("warn-icon-", level))))
    }
  }
  colour <- c(alert = APP_ALERT_HEX, warning = APP_WARNING_HEX, severe = APP_SEVERE_HEX)[[level]]
  n <- c(alert = 1L, warning = 2L, severe = 3L)[[level]]
  waves <- vapply(seq_len(n), function(i) {
    y <- 62 + (i - 1L) * 10
    sprintf('<path d="M8 %d q 10 -8 20 0 t 20 0 t 20 0 t 20 0" fill="none" stroke="#FFFFFF" stroke-width="3.5" stroke-linecap="round"/>', y)
  }, character(1))
  htmltools::HTML(paste0(
    sprintf('<svg class="warn-icon warn-icon-%s" viewBox="0 0 96 96" aria-hidden="true" focusable="false">', level),
    sprintf('<rect width="96" height="96" rx="8" fill="%s"/>', colour),
    '<path d="M48 12 L82 40 H74 V56 H22 V40 H14 Z" fill="#FFFFFF"/>',
    sprintf('<rect x="42" y="42" width="12" height="14" fill="%s"/>', colour),
    paste(waves, collapse = ""), '</svg>'))
}


# -----------------------------------------------------------------------------
# Charts (plotly)
#
# One look for every chart: the app font and ink, a transparent surface, faint
# horizontal gridlines only, no zero line, a quiet axis line, and a navy hover
# label. Series colours come from the palette, each at least 3:1 against the
# surface (checked in test-theme.R); alerts are blue and warnings magenta, the
# same magenta as warning areas on the map.
# -----------------------------------------------------------------------------

APP_SERIES <- c("#053D7E", "#BE0553", "#038CF4", "#475A70", "#7A2E8E", "#0B7A75", "#5B7FA6", "#8A5A00")
APP_ALERT_HEX   <- "#0161BD"
APP_WARNING_HEX <- "#BE0553"

# A whole-number tick step that gives about `target` ticks, so counts never crowd.
nice_dtick <- function(max_value, target = 5L) {
  m <- suppressWarnings(as.numeric(max_value))
  if (length(m) == 0L || is.na(m) || m <= target) return(1)
  raw <- m / target
  mag <- 10^floor(log10(raw))
  step <- c(1, 2, 5, 10)[which(c(1, 2, 5, 10) * mag >= raw)[1L]] * mag
  max(1, step)
}

# Axis settings shared by every chart. grid = TRUE draws the faint horizontal lines.
app_plot_axis <- function(title = NULL, grid = FALSE, ...) {
  p <- APP_PALETTE
  c(list(title = list(text = if (is.null(title)) "" else title, font = list(size = 12, color = p$ink_muted)),
         showgrid = grid, gridcolor = p$line, gridwidth = 0.5, zeroline = FALSE,
         showline = TRUE, linecolor = p$line_strong, linewidth = 1,
         ticks = "outside", tickcolor = p$line_strong, ticklen = 4,
         tickfont = list(size = 11, color = p$ink_muted), automargin = TRUE), list(...))
}

# Applies the common look to a plotly figure.
app_plot_style <- function(fig, ...) {
  p <- APP_PALETTE
  plotly::layout(fig,
    font = list(family = "Arial, 'Helvetica Neue', Helvetica, sans-serif", color = p$ink, size = 12),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
    hoverlabel = list(bgcolor = p$navy, bordercolor = p$navy, font = list(color = "#FFFFFF", size = 12)),
    legend = list(orientation = "h", x = 1, xanchor = "right", y = 1.04, yanchor = "bottom", font = list(size = 12, color = p$ink)),
    ...)
}
