source(file.path(app_root, "theme.R"), local = FALSE)

test_that("every contrast pair in the theme meets its minimum", {
  for (p in APP_CONTRAST_PAIRS) {
    r <- app_contrast(APP_PALETTE[[p[[1]]]], APP_PALETTE[[p[[2]]]])
    expect_gte(r, p[[3]], label = sprintf("%s on %s = %.2f", p[[1]], p[[2]], r))
  }
})

test_that("the light brand blues are decorative only", {
  # sky is too light for text on white. If a swap makes it pass, it may be used for text.
  expect_lt(app_contrast(APP_PALETTE$sky, "#FFFFFF"), 3)
  expect_lt(app_contrast(APP_PALETTE$bright, "#FFFFFF"), 4.5)
  for (name in c("sky", "bright", "ice")) {
    expect_false(any(vapply(APP_CONTRAST_PAIRS, function(p) p[[1]] == name, logical(1))))
  }
})

test_that("brand colours never collide with the risk colours", {
  risk <- c("#b02020", "#e08020", "#f0d040", "#4f9d4f")
  brand <- unlist(APP_PALETTE[c("navy", "navy_deep", "blue", "bright", "sky", "cyan", "ice")])
  for (b in brand) for (r in risk) expect_false(identical(tolower(b), tolower(r)))
  # The frame must look different enough from every risk colour to never be read as one.
  for (r in risk) {
    d <- sqrt(sum((grDevices::col2rgb(APP_PALETTE$navy)[, 1] - grDevices::col2rgb(r)[, 1])^2))
    expect_gt(d, 60)
  }
})

test_that("every palette colour is exposed as a CSS variable", {
  css <- app_css_variables()
  for (v in c("navy", "navy-deep", "blue", "bright", "sky", "cyan", "ice", "tint", "magenta", "ink", "muted", "line", "line-strong", "bg", "surface")) {
    expect_match(css, paste0("--app-", v, ":"), fixed = TRUE)
  }
})

test_that("the header mark is drawn inline when no file is supplied", {
  html <- as.character(app_logo())
  expect_match(html, "brand-mark", fixed = TRUE)
  expect_match(html, APP_PALETTE$sky, fixed = TRUE)
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
  used <- unique(regmatches(css, gregexpr("var\\(--app-[a-z-]+\\)", css))[[1]])
  used <- sub("var\\((--app-[a-z-]+)\\)", "\\1", used)
  defined <- regmatches(app_css_variables(), gregexpr("--app-[a-z-]+(?=:)", app_css_variables(), perl = TRUE))[[1]]
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
