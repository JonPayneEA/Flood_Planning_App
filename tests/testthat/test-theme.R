source(file.path(app_root, "theme.R"), local = FALSE)

test_that("every contrast pair in the theme meets its minimum", {
  for (p in EA_CONTRAST_PAIRS) {
    r <- ea_contrast(EA_PALETTE[[p[[1]]]], EA_PALETTE[[p[[2]]]])
    expect_gte(r, p[[3]], label = sprintf("%s on %s = %.2f", p[[1]], p[[2]], r))
  }
})

test_that("the brand green fails as white-text background, which is why a darker green exists", {
  # Documents the reason for two greens: if a swap makes the fill pass, this
  # test tells you the dark green may no longer be needed.
  expect_lt(ea_contrast("#FFFFFF", EA_PALETTE$green), 4.5)
  expect_gte(ea_contrast("#FFFFFF", EA_PALETTE$green_dark), 4.5)
})

test_that("the brand blue is decorative only", {
  expect_lt(ea_contrast(EA_PALETTE$blue, "#FFFFFF"), 3)
  expect_false(any(vapply(EA_CONTRAST_PAIRS, function(p) p[[1]] == "blue", logical(1))))
})

test_that("brand colours never collide with the risk colours", {
  risk <- c("#b02020", "#e08020", "#f0d040", "#4f9d4f")
  for (b in c(EA_PALETTE$green, EA_PALETTE$green_dark, EA_PALETTE$green_mid)) {
    for (r in risk) expect_false(identical(tolower(b), tolower(r)))
  }
  # The brand green must look different enough from the "Very Low" green that
  # the frame is never read as a risk signal.
  d <- sqrt(sum((grDevices::col2rgb(EA_PALETTE$green_dark)[, 1] - grDevices::col2rgb("#4f9d4f")[, 1])^2))
  expect_gt(d, 60)
})

test_that("every palette colour is exposed as a CSS variable", {
  css <- ea_css_variables()
  for (v in c("green", "green-dark", "green-mid", "tint", "blue", "blue-dark", "magenta", "ink", "muted", "line", "line-strong", "bg", "surface")) {
    expect_match(css, paste0("--ea-", v, ":"), fixed = TRUE)
  }
})

ui_css <- function() {
  txt <- paste(readLines(file.path(app_root, "app_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  a <- regexpr('custom_css <- "', txt, fixed = TRUE)
  start <- a + attr(a, "match.length")
  end <- regexpr('\n"\n', substr(txt, start, nchar(txt)), fixed = TRUE)
  substr(txt, start, start + end - 2L)
}

test_that("the stylesheet is structurally sound", {
  css <- ui_css()
  expect_gt(nchar(css), 5000)
  expect_equal(lengths(regmatches(css, gregexpr("{", css, fixed = TRUE))),
               lengths(regmatches(css, gregexpr("}", css, fixed = TRUE))))
  expect_equal(lengths(regmatches(css, gregexpr("/*", css, fixed = TRUE))),
               lengths(regmatches(css, gregexpr("*/", css, fixed = TRUE))))
  # Flat CSS: after every "{" the next brace must be its "}", except for
  # @media and @keyframes wrappers. A block inserted mid-rule (which has happened) breaks this.
  flat <- gsub("/\\*.*?\\*/", "", css, perl = TRUE)
  pos <- gregexpr("[{}]", flat)[[1]]
  chars <- strsplit(flat, "")[[1]][pos]
  starts <- c(0L, pos[-length(pos)])
  bad <- character()
  for (i in seq_along(chars)) {
    if (chars[i] != "{") next
    before <- substr(flat, if (i == 1L) 1L else pos[i - 1L] + 1L, pos[i] - 1L)
    if (grepl("@media|@keyframes", before)) next
    if (i == length(chars) || chars[i + 1L] != "}") bad <- c(bad, trimws(before))
  }
  expect_equal(bad, character(), info = paste("rules with a nested or unclosed block:", paste(utils::head(bad, 3), collapse = " | ")))
  # And no declaration is split in two.
  expect_false(grepl("text-ali[^g]", css))
})

test_that("every CSS variable the stylesheet uses is defined", {
  css <- ui_css()
  used <- unique(regmatches(css, gregexpr("var\\(--ea-[a-z-]+\\)", css))[[1]])
  used <- sub("var\\((--ea-[a-z-]+)\\)", "\\1", used)
  defined <- regmatches(ea_css_variables(), gregexpr("--ea-[a-z-]+(?=:)", ea_css_variables(), perl = TRUE))[[1]]
  expect_setequal(setdiff(used, defined), character())
})

test_that("only risk and status colours remain as literal hex in the stylesheet", {
  css <- ui_css()
  lits <- unique(tolower(regmatches(css, gregexpr("#[0-9A-Fa-f]{6}", css))[[1]]))
  allowed <- c("#b02020", "#e08020", "#f0d040", "#4f9d4f", "#fff3bf",                 # risk colours and heat ramp
               "#1e6b1e", "#eaf5ea", "#8a5a00", "#fdf1e2", "#8b1a1a", "#f9e5e5",    # health badge states
               "#6b1212", "#6b4300")                                                   # banner text
  expect_setequal(setdiff(lits, allowed), character())
})
