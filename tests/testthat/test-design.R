source(file.path(app_root, "theme.R"), local = FALSE)
source(file.path(app_root, "design.R"), local = FALSE)

lum <- function(hex) {
  v <- c(strtoi(substr(hex, 2, 3), 16L), strtoi(substr(hex, 4, 5), 16L), strtoi(substr(hex, 6, 7), 16L)) / 255
  v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4)
  0.2126 * v[1] + 0.7152 * v[2] + 0.0722 * v[3]
}
ratio <- function(a, b) { x <- sort(c(lum(a), lum(b)), decreasing = TRUE); (x[1] + 0.05) / (x[2] + 0.05) }

test_that("every design colour pair meets its contrast minimum", {
  for (p in DESIGN_CONTRAST_PAIRS)
    expect_gte(ratio(GOV_COLOURS[[p[[1]]]], GOV_COLOURS[[p[[2]]]]), p[[3]], label = paste(p[[1]], "on", p[[2]]))
  expect_gte(ratio("#FFFFFF", APP_PALETTE$navy), 7)
})

test_that("the design stylesheet has every token filled and balanced braces", {
  css <- design_css()
  expect_false(grepl("{{", css, fixed = TRUE))
  expect_equal(lengths(regmatches(css, gregexpr("{", css, fixed = TRUE))),
               lengths(regmatches(css, gregexpr("}", css, fixed = TRUE))))
  expect_match(css, APP_PALETTE$navy, fixed = TRUE)
})

test_that("the navigation lists each view once, and demo views only in demo mode", {
  v <- app_view_values(FALSE)
  expect_false(anyDuplicated(v) > 0)
  expect_false(any(c("MP audit log", "Style guide") %in% v))
  expect_true(all(c("MP audit log", "Style guide") %in% app_view_values(TRUE)))
  html <- as.character(app_navigation(FALSE))
  for (x in v) expect_match(html, paste0('data-view="', x, '"'), fixed = TRUE)
})

test_that("navigation views match the tabs the page builds", {
  txt <- paste(readLines(file.path(app_root, "app_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  tabs <- regmatches(txt, gregexpr('tabPanel\\("[^"]+"', txt))[[1]]
  tabs <- sub('tabPanel\\("', "", sub('"$', "", tabs))
  expect_setequal(tabs, app_view_values(TRUE))
  expect_match(txt, 'id = "main_tabs"', fixed = TRUE)
})

test_that("the shell has a skip link target and a footer with the licence", {
  expect_match(as.character(app_footer()), "Open Government Licence", fixed = TRUE)
  expect_match(as.character(app_footer()), "real-time data API", fixed = TRUE)
  expect_match(app_nav_js(), "main_tabs", fixed = TRUE)
})


test_that("Copy link saves the view on the server and copies a short link, and nothing writes the whole state into the address", {
  app_r <- paste(readLines(file.path(app_root, "app.R"), warn = FALSE), collapse = "\n")
  expect_true(grepl('enableBookmarking = "server"', app_r, fixed = TRUE)); expect_false(grepl('enableBookmarking = "url"', app_r, fixed = TRUE))
  srv <- paste(readLines(file.path(app_root, "app_server.R"), warn = FALSE), collapse = "\n")
  expect_false(grepl("updateQueryString", srv, fixed = TRUE))                                  # the address bar is left alone
  expect_true(grepl("input$copy_link_click", srv, fixed = TRUE) && grepl('sendCustomMessage("copy_link"', srv, fixed = TRUE))
  js <- app_nav_js()
  expect_true(grepl("addCustomMessageHandler('copy_link'", js, fixed = TRUE) && grepl("window.fgsCopy = function", js, fixed = TRUE))
  des <- paste(readLines(file.path(app_root, "design.R"), warn = FALSE), collapse = "\n")
  expect_true(grepl("copy_link_click", des, fixed = TRUE)); expect_false(grepl("writeText(window.location.href)", des, fixed = TRUE))   # no longer copies the address bar
})


test_that("the footer carries a build stamp and the app says what link it made, so a wrong shared link can be traced", {
  des <- paste(readLines(file.path(app_root, "design.R"), warn = FALSE), collapse = "\n")
  srv <- paste(readLines(file.path(app_root, "app_server.R"), warn = FALSE), collapse = "\n")
  glob <- paste(readLines(file.path(app_root, "global.R"), warn = FALSE), collapse = "\n")
  expect_true(grepl('uiOutput("build_stamp"', des, fixed = TRUE) && grepl("output$build_stamp", srv, fixed = TRUE) && grepl("APP_BUILD <-", glob, fixed = TRUE))
  expect_true(grepl('showModal(modalDialog(title = "The link the app made"', srv, fixed = TRUE))
})


test_that("the Economics research view is a proof-of-concept view", {
  expect_true("Economics research" %in% app_view_values(FALSE, poc = TRUE))
  expect_false("Economics research" %in% app_view_values(FALSE, poc = FALSE))
})


test_that("the plain tables on the Target Area page are styled where they sit, not only on the Rapid Flood Guidance page", {
  css <- paste(readLines(file.path(app_root, "design.R"), warn = FALSE), collapse = "\n")
  expect_match(css, ".area-grid table:not(.dataTable)", fixed = TRUE)
  expect_match(css, ".area-wide table:not(.dataTable)", fixed = TRUE)
  expect_match(css, ".area-grid td.n", fixed = TRUE)                       # numbers right-aligned there too
})
