# =============================================================================
# theme.R
# Environment Agency styling, in one place.
#
# PROVISIONAL. No official EA palette was available when this was written.
#   - green (#008A4B), green_mid, green_tint: sampled from the EA logo, not
#     taken from brand guidelines.
#   - blue (#54BCE7) and magenta (#BE0553): already in this app, labelled there
#     as EA Pantone 2985 and 227.
#   - Everything else is a neutral or a darker step chosen for contrast.
# To use the official values, change this list and nothing else. tests/
# checks the contrast pairs below, so a bad swap fails a test.
#
# NOT in this file, on purpose: the FGS risk colours (red, amber, yellow, green
# on the matrix, map, chips and heat grid). They come from the FGS User Guide
# and carry a fixed meaning. The brand green must never be mistaken for the
# "Very Low" risk green, so the brand sits in the frame (header, tabs, buttons,
# surfaces) and the risk colours stay in the data.
# =============================================================================

EA_PALETTE <- list(
  green       = "#008A4B",   # brand fill, borders, decoration. White text on it is 4.43:1, so not for text
  green_dark  = "#00663B",   # header, white-on-green text, green text on light surfaces (7.09:1 with white)
  green_mid   = "#83C0A0",
  green_tint  = "#E5F0E9",   # sidebar and quiet surfaces
  blue        = "#54BCE7",   # brand blue. Decorative only: 2.16:1 on white
  blue_dark   = "#00548B",   # links and focus (7.95:1 on white)
  magenta     = "#BE0553",   # flood warning areas on the map
  ink         = "#1D1D1B",   # body text
  ink_muted   = "#505A5F",   # secondary text (6.56:1 on the page background)
  line        = "#CBD5CF",   # hairlines
  line_strong = "#9AA8A0",
  bg          = "#F4F7F5",   # page background
  surface     = "#FFFFFF"    # cards and panels
)

# Contrast pairs that must hold (foreground, background, minimum ratio).
# Used by tests/testthat/test-theme.R.
EA_CONTRAST_PAIRS <- list(
  list("surface",    "green_dark", 4.5),
  list("ink",        "surface",    7),
  list("ink",        "bg",         7),
  list("ink",        "green_tint", 7),
  list("ink_muted",  "bg",         4.5),
  list("ink_muted",  "surface",    4.5),
  list("blue_dark",  "surface",    4.5),
  list("blue_dark",  "bg",         4.5),
  list("green_dark", "surface",    4.5),
  list("green_dark", "green_tint", 4.5),
  list("magenta",    "surface",    4.5)
)

ea_contrast <- function(fg, bg) {
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
ea_theme <- function() {
  p <- EA_PALETTE
  bslib::bs_theme(
    version = 5,
    primary = p$green_dark, secondary = p$ink_muted, success = p$green,
    info = p$blue_dark, danger = p$magenta,
    bg = p$surface, fg = p$ink,
    "body-bg" = p$bg, "body-color" = p$ink,
    "link-color" = p$blue_dark, "link-hover-color" = p$ink,
    "font-family-base" = "Arial, 'Helvetica Neue', Helvetica, sans-serif",
    "border-radius" = "4px", "btn-border-radius" = "4px",
    "border-color" = p$line
  )
}

# CSS custom properties used by the rules in app_ui.R. Defined once here.
ea_css_variables <- function() {
  p <- EA_PALETTE
  paste0(":root {",
         "--ea-green:", p$green, ";--ea-green-dark:", p$green_dark, ";",
         "--ea-green-mid:", p$green_mid, ";--ea-tint:", p$green_tint, ";",
         "--ea-blue:", p$blue, ";--ea-blue-dark:", p$blue_dark, ";",
         "--ea-magenta:", p$magenta, ";--ea-ink:", p$ink, ";",
         "--ea-muted:", p$ink_muted, ";--ea-line:", p$line, ";",
         "--ea-line-strong:", p$line_strong, ";--ea-bg:", p$bg, ";",
         "--ea-surface:", p$surface, ";}")
}

# Optional logo. Drop an approved file at www/ea-logo.svg (or .png) and it
# appears in the header. None is bundled: the logo is a protected mark and the
# approved artwork should come from the brand team.
ea_logo <- function() {
  for (f in c("ea-logo.svg", "ea-logo.png")) {
    if (file.exists(file.path("www", f))) {
      return(htmltools::tags$img(src = f, alt = "Environment Agency", class = "ea-logo"))
    }
  }
  NULL
}
