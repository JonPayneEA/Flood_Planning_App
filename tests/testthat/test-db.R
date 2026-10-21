# The database layer, with Databricks replaced by fakes. These test what the app does
# about a connection that drops, not brickster.

net_err <- "Failed to connect to warehouse 0a86ae40313bb7db: Failed to perform HTTP request."
sql_err <- "[TABLE_OR_VIEW_NOT_FOUND] The table or view `c`.`s`.`fgs_constituency_changes` cannot be found. SQLSTATE: 42P01; line 12 pos 9"

# A data layer whose connector, query runner and sleeper are fakes that record what happened.
fake_db <- function(script = list(), retries = 2L) {
  d <- load_data_layer(stub_query = FALSE)
  d$DB_RETRIES <- retries
  d$log <- new.env(); d$log$connects <- 0L; d$log$closes <- 0L; d$log$runs <- 0L; d$log$sleeps <- numeric(); d$log$step <- 0L
  # `script` is a list of per-attempt outcomes: "ok", "net" (network error on the query), "connect" (network error
  # while connecting), "sql" (a SQL error). Anything past the end is "ok".
  outcome <- function() { d$log$step <- d$log$step + 1L; if (d$log$step <= length(script)) script[[d$log$step]] else "ok" }
  d$db_connector <- function() {
    d$log$connects <- d$log$connects + 1L
    if (identical(d$log$pending_connect_fail, TRUE)) { d$log$pending_connect_fail <- FALSE; stop(net_err) }
    structure(list(id = d$log$connects), class = "fake_con")
  }
  d$db_run <- function(con, sql) {
    d$log$runs <- d$log$runs + 1L
    o <- outcome()
    if (o == "net") stop(net_err)
    if (o == "sql") stop(sql_err)
    if (o == "connect") stop(net_err)
    data.frame(x = 1L, con = con$id)
  }
  d$db_close <- function(con) { d$log$closes <- d$log$closes + 1L; NULL }
  d$db_sleep <- function(s) d$log$sleeps <- c(d$log$sleeps, s)
  d
}

test_that("one connection is built and reused, so there is no test query per call", {
  d <- fake_db()
  for (i in 1:5) expect_equal(d$db_query("SELECT 1")$con, 1L)
  expect_equal(d$log$connects, 1L); expect_equal(d$log$runs, 5L); expect_equal(d$log$closes, 0L)
})

test_that("a network error on a query drops the connection and the retry builds a fresh one", {
  d <- fake_db(list("net", "ok"))
  r <- suppressMessages(d$db_query("SELECT 1"))
  expect_equal(r$x, 1L)
  expect_equal(d$log$connects, 2L)                  # rebuilt after the drop
  expect_equal(d$log$closes, 1L)                    # and the broken one was closed
  expect_equal(d$log$sleeps, 1.5)
  expect_equal(r$con, 2L)
})

test_that("a failure while connecting is retried too", {
  d <- fake_db()
  d$log$pending_connect_fail <- TRUE
  r <- suppressMessages(d$db_query("SELECT 1"))
  expect_equal(r$x, 1L); expect_equal(d$log$connects, 2L); expect_equal(d$log$sleeps, 1.5)
})

test_that("retries run out, the real message is raised, and the next call tries afresh", {
  d <- fake_db(list("net", "net", "net"))
  e <- tryCatch(suppressMessages(d$db_query("SELECT 1")), error = function(e) e)
  expect_equal(conditionMessage(e), net_err)
  expect_equal(d$log$runs, 3L); expect_equal(d$log$sleeps, c(1.5, 3))
  expect_equal(d$db_query("SELECT 1")$x, 1L)        # the script is exhausted: back to normal
})

test_that("a SQL error is raised at once, not retried, and keeps the connection", {
  d <- fake_db(list("sql"))
  expect_error(d$db_query("SELECT 1"), "TABLE_OR_VIEW_NOT_FOUND")
  expect_equal(d$log$runs, 1L); expect_length(d$log$sleeps, 0L)
  expect_equal(d$log$closes, 0L)                    # the warehouse answered, so the connection is fine
  d$db_query("SELECT 1"); expect_equal(d$log$connects, 1L)
})

test_that("FGS_QUERY_RETRIES = 0 means no retry", {
  d <- fake_db(list("net", "ok"), retries = 0L)
  expect_error(suppressMessages(d$db_query("SELECT 1")), "Failed to connect")
  expect_equal(d$log$runs, 1L); expect_length(d$log$sleeps, 0L)
})

test_that("the shared connection is rebuilt once it is old", {
  d <- fake_db()
  d$db_query("SELECT 1")
  d$.fgs_db$made <- Sys.time() - d$DB_CONN_MAX_AGE - 5
  d$db_query("SELECT 1")
  expect_equal(d$log$connects, 2L); expect_equal(d$log$closes, 1L)
})

test_that("the console tells the story of a drop: start, then recovery", {
  d <- fake_db(list("net", "ok"))
  msgs <- character()
  withCallingHandlers(d$db_query("SELECT 1"),
    message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
  expect_length(msgs, 2L)
  expect_match(msgs[1], "\\] Databricks warehouse unreachable: Failed to connect to warehouse")
  expect_match(msgs[2], "\\] Databricks warehouse reachable again after [0-9]+ s \\(1 failed request\\)")
})

test_that("a healthy run says nothing", {
  d <- fake_db()
  msgs <- character()
  withCallingHandlers(for (i in 1:3) d$db_query("SELECT 1"), message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
  expect_length(msgs, 0L)
})

test_that("a missing table does not count as an outage, and ends one if there was one", {
  d <- fake_db(list("net", "sql"))
  msgs <- character()
  withCallingHandlers(try(d$db_query("SELECT 1"), silent = TRUE), message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
  expect_true(any(grepl("unreachable", msgs)))
  expect_true(any(grepl("reachable again", msgs)))   # the warehouse answered, even if with an error
})

test_that("demo mode still refuses to touch a connection at all", {
  consts <- load_app_constants()
  e <- new.env(parent = globalenv())
  for (n in ls(consts, all.names = TRUE)) assign(n, get(n, consts), envir = e)
  assign("DEMO_MODE", TRUE, envir = e); assign("CACHE_TTL_SECONDS", 0, envir = e)
  old <- setwd(app_root); on.exit(setwd(old)); source("data.R", local = e)
  built <- FALSE; e$db_connector <- function() { built <<- TRUE; list() }
  expect_error(e$db_query("SELECT 1"), "Demo mode"); expect_error(e$db_connection(), "Demo mode")
  expect_false(built)
})

test_that("the retry setting is validated", {
  expect_error(load_app_constants(list(FGS_QUERY_RETRIES = "9")), "between 0 and 5")
  expect_error(load_app_constants(list(FGS_QUERY_RETRIES = "abc")), "number")
  expect_equal(get("DB_RETRIES", load_app_constants(list(FGS_QUERY_RETRIES = "0"))), 0L)
  expect_equal(get("DB_RETRIES", load_app_constants()), 2L)
})


test_that("brickster's query progress is switched off, so queries do not raise pop-ups", {
  skip_if_not_installed("cli")
  old <- options(cli.progress_handlers_only = NULL); on.exit(options(old), add = TRUE)
  load_app_constants()
  expect_identical(getOption("cli.progress_handlers_only"), character())
  # With that option cli picks no handler at all, and a Shiny pop-up is a handler.
  expect_length(asNamespace("cli")$cli_progress_select_handlers(list(), environment()), 0L)
})

test_that("FGS_SHOW_QUERY_PROGRESS=1 leaves cli's progress alone", {
  old <- options(cli.progress_handlers_only = NULL); on.exit(options(old), add = TRUE)
  load_app_constants(extra_env = list(FGS_SHOW_QUERY_PROGRESS = "1"))
  expect_null(getOption("cli.progress_handlers_only"))
})

test_that("a setting the user already made is not overridden", {
  old <- options(cli.progress_handlers_only = "cli"); on.exit(options(old), add = TRUE)
  load_app_constants()
  expect_identical(getOption("cli.progress_handlers_only"), "cli")
})
