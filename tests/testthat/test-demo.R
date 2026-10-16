skip_if_not_installed("sf")

# One demo data layer for the whole file: generating the geometry takes seconds.
demo_env <- local({
  env <- new.env(parent = globalenv())
  consts <- load_app_constants()
  for (n in ls(consts, all.names = TRUE)) assign(n, get(n, consts), envir = env)
  assign("DEMO_MODE", TRUE, envir = env)
  assign("CACHE_TTL_SECONDS", 0, envir = env)
  old <- setwd(app_root); on.exit(setwd(old))
  source("data.R", local = env)
  env
})
scn <- c("quiet", "developing", "severe")

test_that("demo mode refuses every warehouse call, by design", {
  expect_error(demo_env$db_query("SELECT 1"), "Demo mode")
  expect_error(demo_env$db_connection(), "Demo mode")
})

test_that("no demo fetcher reaches for the warehouse", {
  calls <- 0L
  demo_env$db_query <- function(sql) { calls <<- calls + 1L; stop("blocked") }
  on.exit(rm("db_query", envir = demo_env))
  for (s in scn) {
    # Ask for each scenario through the same path the app uses.
    for (fn in c("fetch_situation", "fetch_briefing_drafts", "fetch_pipeline_health", "fetch_recent_statements",
                 "fetch_ea_constituency_mp", "fetch_constituency_risk_geometry", "fetch_data_signature")) {
      expect_no_error(demo_env[[fn]]())
    }
    expect_no_error(demo_env$fetch_exposure(12))
    expect_no_error(demo_env$fetch_changes(4812L))
    for (d in 1:5) {
      expect_no_error(demo_env$fetch_risk_polygons(4812L, d))
      expect_no_error(demo_env$fetch_ea_areas_for_statement(4812L, d))
      expect_no_error(demo_env$fetch_summary_counts(4812L, d))
      expect_no_error(demo_env$fetch_ea_geometry(4812L, d))
      expect_no_error(demo_env$fetch_constituency_geometry(4812L, d))
      expect_no_error(demo_env$fetch_constituencies_for_statement(4812L, d))
    }
    break   # the session-less default scenario is enough here; scenarios are covered below
  }
  expect_equal(calls, 0L)
})

test_that("nothing in the app's code can write to a warehouse", {
  files <- file.path(app_root, c("data.R", "demo.R", "app_server.R", "app_ui.R", "helpers.R", "theme.R", "global.R", "app.R"))
  for (f in files) {
    lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
    code <- sub("\\s#.*$", "", sub("^\\s*#.*$", "", lines))       # drop comments
    expect_false(any(grepl("dbWriteTable|dbExecute|dbSendStatement|dbAppendTable|dbCreateTable|dbRemoveTable|dbSendUpdate",
                           code)), info = f)
    expect_false(any(grepl("\\b(INSERT|UPDATE|DELETE|MERGE|DROP|ALTER|TRUNCATE|CREATE|GRANT|REVOKE|COPY INTO)\\b", code)), info = f)
  }
})

test_that("global.R loads in demo mode with no credentials and without brickster", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("DT")
  old <- setwd(app_root); on.exit(setwd(old), add = TRUE)
  script <- paste(
    "suppressMessages(source('global.R'))",
    "cat('DEMO=', DEMO_MODE, '\\n')",
    "cat('BRICKSTER=', 'brickster' %in% loadedNamespaces(), '\\n')",
    "cat('DBI_ATTACHED=', 'package:DBI' %in% search(), '\\n')",
    "cat(paste0('TOKEN=[', Sys.getenv('DATABRICKS_TOKEN'), ']'), '\\n')", sep = "; ")
  out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("-e", shQuote(script)),
                                  env = c("FGS_DEMO_MODE=1", "DATABRICKS_TOKEN="), stdout = TRUE, stderr = TRUE))
  expect_true(any(grepl("DEMO= TRUE", out)), info = paste(out, collapse = "\n"))
  expect_true(any(grepl("BRICKSTER= FALSE", out)))
  expect_true(any(grepl("DBI_ATTACHED= FALSE", out)))      # sf may load DBI; the app must not attach it
  expect_true(any(grepl("TOKEN=\\[\\]", out)))
})

test_that("the same scenario always generates the same data", {
  a <- demo_env$demo_situation("developing")
  rm(list = ls(demo_env$.demo_env, all.names = TRUE), envir = demo_env$.demo_env)
  b <- demo_env$demo_situation("developing")
  expect_equal(a, b)
})

test_that("everything is fictional", {
  g <- demo_env$demo_geometry()$table
  expect_false(anyDuplicated(g$constituency_id) > 0)
  expect_false(anyDuplicated(g$constituency_name) > 0)
  expect_false(anyDuplicated(g$mp_name[!is.na(g$mp_name)]) > 0)
  expect_equal(sum(is.na(g$mp_name)), 2L)                                  # two vacant seats
  expect_true(all(grepl("@example\\.invalid$", g$mp_email[!is.na(g$mp_email)])))
  expect_true(all(grepl("^DEMO-C[0-9]{3}$", g$constituency_id)))
  ea <- demo_env$demo_ea_areas()$table
  expect_true(all(grepl("^DEMO-(FWA|FAA)-[0-9]{3}$", ea$ea_area_code)))
})

test_that("scenarios escalate in the order the labels promise", {
  n3 <- vapply(scn, function(s) sum(demo_env$demo_situation(s)$fgs_worst_rank >= 3L), integer(1))
  expect_equal(unname(n3[["quiet"]]), 0L)
  expect_lt(n3[["quiet"]], n3[["developing"]])
  expect_lt(n3[["developing"]], n3[["severe"]])
  expect_equal(nrow(demo_env$demo_changes_for("quiet", 4812L)), 0L)       # nothing changes when nothing happens
  expect_equal(nrow(demo_env$demo_briefings("quiet")), 0L)
})

for (s in scn) {
  test_that(paste("the", s, "scenario is internally consistent"), {
    sit <- demo_env$demo_situation(s)
    expect_equal(nrow(sit), demo_env$demo_geometry()$n)
    expect_true(all(sit$fgs_worst_rank == pmax(sit$fgs_rank_day1, sit$fgs_rank_day2, sit$fgs_rank_day3, sit$fgs_rank_day4, sit$fgs_rank_day5)))
    expect_true(all((sit$fgs_worst_rank == 0L) == is.na(sit$fgs_worst_date)))

    # Risk levels agree with the matrix the live app uses.
    for (d in 1:5) {
      ci <- demo_env$demo_const_inter(s, 4812L, d)
      if (nrow(ci) == 0L) next
      expect_true(all(ci$intersection_pct > 0 & ci$intersection_pct <= 1))
      m <- merge(ci[, .(risk_x, risk_y, risk_level)], demo_env$RISK_MATRIX[, .(risk_x = x, risk_y = y, colour)], by = c("risk_x", "risk_y"))
      expect_equal(unname(demo_env$RISK_LEVEL_BY_COLOUR[m$colour]), m$risk_level)
      ea <- demo_env$demo_ea_inter(s, 4812L, d)
      if (nrow(ea) > 0L) expect_true(all(ea$intersection_pct >= 0.25 & ea$intersection_pct <= 1))   # the live 25% rule
    }

    # Every flagged constituency traces back to an overlap of 25% or more.
    flagged <- sit[fgs_worst_rank >= 1L, constituency_id]
    traced <- unique(unlist(lapply(1:5, function(d) {
      ci <- demo_env$demo_const_inter(s, 4812L, d); ci[intersection_pct >= 0.25, constituency_id] })))
    expect_setequal(flagged, intersect(flagged, traced))
    expect_setequal(traced, flagged)

    # Changes: only real differences, typed correctly, only for dates both statements cover.
    ch <- demo_env$demo_changes_for(s, 4812L)
    if (nrow(ch) > 0L) {
      expect_true(all(ch$new_rank != ch$previous_rank))
      expect_true(all(ch$rank_change == ch$new_rank - ch$previous_rank))
      expect_true(all(ch$change_type[ch$previous_rank == 0L] == "new"))
      expect_true(all(ch$change_type[ch$new_rank == 0L] == "cleared"))
      expect_true(all(ch$day_index %in% 1:4))
    }
    # Escalation flags come from the changes table, nothing else.
    esc <- unique(ch[change_type %in% c("new", "escalated"), constituency_id])
    expect_setequal(sit[fgs_escalated == TRUE, constituency_id], esc)

    # Briefings: exactly the constituencies at Medium or above that have an MP.
    br <- demo_env$demo_briefings(s)
    expect_setequal(br$constituency_id, sit[fgs_worst_rank >= 3L & !is.na(mp_name), constituency_id])
    expect_true(all(br$status == "draft"))
    expect_true(all(grepl("^Dear ", br$body)))
    expect_false(any(grepl("NA", br$body, fixed = TRUE)))
  })
}

test_that("the briefing says when the statement was issued, to the minute", {
  br <- demo_env$demo_briefings("developing")
  expect_match(br$body[1], "issued at 09:30 on")
})

test_that("health is clean by default and fails on request", {
  expect_equal(demo_env$demo_health(FALSE)[check_name == "overall", status], "ok")
  h <- demo_env$demo_health(TRUE)
  expect_equal(h[check_name == "overall", status], "fail")
  expect_equal(h[status == "fail" & check_name != "overall", check_name], "mp_details_age")
  expect_equal(stale_status(h, now = Sys.time())$level, "fail")
  expect_equal(stale_status(demo_env$demo_health(FALSE), now = Sys.time())$level, "ok")
})

test_that("the scenario belongs to the session, so two viewers cannot affect each other", {
  size <- function(scenario) {
    out <- NULL
    shiny::testServer(function(input, output, session) {
      session$userData$demo_scenario <- scenario
      out <<- sum(demo_env$fetch_situation()$fgs_worst_rank >= 3L)
    }, {})
    out
  }
  expect_equal(size("quiet"), 0L)
  expect_gt(size("severe"), size("developing"))
  expect_equal(demo_env$demo_current()$scenario, "developing")           # no session: the default
})

test_that("demo statements look like the warehouse's: newest first, timestamps not text", {
  st <- demo_env$fetch_recent_statements(100L)
  expect_equal(nrow(st), 8L)
  expect_true(inherits(st$issued_at, "POSIXct"))
  expect_equal(st$statement_id, sort(st$statement_id, decreasing = TRUE))
  expect_equal(format(st$issued_at[1], "%H:%M"), "09:30")
})
