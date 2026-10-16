skip_if_not_installed("leaflet")
skip_if_not_installed("DT")
skip_if_not_installed("sf")

# ---- fake data ---------------------------------------------------------------

mons_now <- format(Sys.Date(), "%Y-%m")
fake <- list(
  statements = function(n) data.table(statement_id = c(7L, 6L), issued_at = c("2026-10-15T09:30:00", "2026-10-14T09:30:00"),
    headline = "h", source_coastal = "", source_surface = "", source_ground = "", source_river = "", england_forecast = "", pdf_url = ""),
  health = function() data.table(check_name = c("a", "b", "overall"), status = c("ok", "warn", "warn"),
    value = "", detail = "", checked_at = format(Sys.time() - 600, "%Y-%m-%dT%H:%M:%S", tz = "UTC")),
  situation = function() data.table(constituency_id = c("c1", "c2", "c3"), constituency_name = c("Alpha", "Beta", "Gamma"),
    mp_matched = TRUE, mp_name = c("A MP", NA, "G MP"), mp_party = "P", mp_email = "e", mp_phone = "t",
    fgs_statement_id = 7L, fgs_issued_at = "2026-10-15T09:30:00", fgs_worst_rank = c(4L, 3L, 1L), fgs_worst_level = "x",
    fgs_worst_date = "2026-10-16", fgs_worst_sources = "river", fgs_rank_day1 = 1L, fgs_rank_day2 = 2L, fgs_rank_day3 = 3L,
    fgs_rank_day4 = 4L, fgs_rank_day5 = 0L, fgs_changed = TRUE, fgs_escalated = c(TRUE, FALSE, FALSE),
    severe_warnings = 0L, warnings = c(1L, 0L, 0L), alerts = 0L, has_active_warning = c(TRUE, FALSE, FALSE)),
  briefings = function() data.table(statement_id = 7L, constituency_id = "c1", constituency_name = "Alpha", mp_name = "A MP",
    mp_email = "a@x", worst_rank = 4L, worst_level = "High", worst_date = "2026-10-16", is_escalation = TRUE,
    has_active_warning = TRUE, subject = "Subj", body = "Line1\nLine2", status = "draft", reviewed_by = NA_character_),
  exposure = function(n) data.table(constituency_id = c("c1", "c1", "c2"), constituency_name = c("Alpha", "Alpha", "Beta"),
    month = c(mons_now, format(Sys.Date() - 40, "%Y-%m"), mons_now), source = "any",
    days_medium_or_above = c(3L, 5L, 1L), days_flagged = c(4L, 6L, 2L), worst_rank = 4L, days_covered = c(10L, 30L, 10L)),
  changes = function(sid) data.table(constituency_id = c("c1", "c2", "c1"), constituency_name = c("Alpha", "Beta", "Alpha"),
    forecast_date = c("2026-10-16", "2026-10-16", "2026-10-17"), day_index = 1:3, source = c("river", "surface", "river"),
    previous_rank = c(1L, 3L, 0L), new_rank = c(3L, 2L, 2L), rank_change = c(2L, -1L, 2L),
    change_type = c("escalated", "de_escalated", "new"))
)

# Build the server in an environment where every fetch is a stub. Counters
# record how often the statement list and health table were read.
make_server_env <- function(overrides = list(), auto_refresh = 30) {
  env <- new.env(parent = globalenv())
  consts <- load_app_constants()
  for (n in ls(consts, all.names = TRUE)) assign(n, get(n, consts), envir = env)
  assign("CACHE_TTL_SECONDS", 0, envir = env)
  assign("AUTO_REFRESH_SECONDS", auto_refresh, envir = env)
  # app_server.R sources data.R by relative path, so load it from the project root.
  old_wd <- setwd(app_root); on.exit(setwd(old_wd), add = TRUE)
  source("guide.R", local = env)
  source("app_server.R", local = env)   # also sources data.R
  setwd(old_wd)

  env$calls <- new.env()
  env$calls$statements <- 0L; env$calls$health <- 0L; env$calls$signature <- 0L
  stubs <- list(
    db_query = function(sql) data.table(),
    fetch_recent_statements = function(n) { env$calls$statements <- env$calls$statements + 1L; fake$statements(n) },
    fetch_pipeline_health = function() { env$calls$health <- env$calls$health + 1L; fake$health() },
    fetch_situation = fake$situation, fetch_briefing_drafts = fake$briefings,
    fetch_exposure = fake$exposure, fetch_changes = fake$changes, fetch_changes_log = function(sid) NULL,
    fetch_ea_constituency_mp = function() data.table(ea_area_code = "a", constituency_name = "X", constituency_overlap_pct = 1,
                                                    mp_name = "m", mp_party = "p", mp_email = "e", mp_phone = "t"),
    fetch_summary_counts = function(...) list(polygon_count = 0L, ea_area_count = 0L, constituency_count = 0L),
    fetch_ea_areas_for_statement = function(...) data.table(ea_area_code = character(), ea_area_name = character(),
      ea_area_type = character(), source = character(), risk_level = character(), risk_colour = character(),
      risk_x = integer(), risk_y = integer(), intersection_pct = numeric()),
    fetch_constituencies_for_statement = function(...) data.table(constituency_id = character(), constituency_name = character(),
      source = character(), risk_x = integer(), risk_y = integer(), risk_colour = character(), risk_level = character(),
      intersection_pct = numeric()),
    fetch_constituency_risk_geometry = function() NULL,
    fetch_data_signature = function() { env$calls$signature <- env$calls$signature + 1L; paste0("sig", env$calls$signature) }
  )
  for (n in names(overrides)) stubs[[n]] <- overrides[[n]]
  for (n in names(stubs)) assign(n, stubs[[n]], envir = env)
  env
}

base_inputs <- list(refresh_data = 0, statement_id = "7", day_index = "1", sit_min_rank = "3", sit_search = "",
                    sit_escalated = FALSE, sit_warning = FALSE, exp_source = "any",
                    exp_metric = "days_medium_or_above", exp_top_n = 25)

html_of <- function(x) gsub("\\s+", " ", as.character(x$html))

# ---- tests ---------------------------------------------------------------------

test_that("the health badge and the stale banner reflect the health table", {
  env <- make_server_env()
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)
    expect_match(html_of(output$health_badge), "health-warn")
    expect_match(html_of(output$health_badge), "1 check needs attention")
    expect_match(html_of(output$stale_banner), "stale-warn")
    expect_match(html_of(output$stale_banner), "b")
  })
})

test_that("a stale health table raises a red banner", {
  env <- make_server_env(list(fetch_pipeline_health = function() {
    h <- fake$health(); h$checked_at <- "2020-01-01T00:00:00"; h }))
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)
    expect_match(html_of(output$stale_banner), "stale-bad")
    expect_match(html_of(output$stale_banner), "may have stopped")
  })
})

test_that("an unreadable health table says so instead of showing a healthy badge", {
  env <- make_server_env(list(fetch_pipeline_health = function() stop("table missing")))
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)
    expect_match(html_of(output$stale_banner), "could not be read")
    expect_match(html_of(output$health_badge), "health-unknown")
  })
})

test_that("the changes strip groups by constituency and links to the map", {
  env <- make_server_env()
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)
    h <- html_of(output$changes_strip)
    expect_match(h, "1 up")
    expect_match(h, "1 down")
    expect_match(h, "goto_constituency")
    expect_equal(lengths(regmatches(h, gregexpr("Alpha", h))), 1L)     # one row for Alpha, not two
    expect_match(h, "\\+1 more")
  })
})

test_that("the situation tab filters by risk, escalation, warning and search", {
  env <- make_server_env()
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)
    expect_equal(situation_filtered()$constituency_name, c("Alpha", "Beta"))
    session$setInputs(sit_min_rank = "0")
    expect_equal(nrow(situation_filtered()), 3L)
    session$setInputs(sit_escalated = TRUE)
    expect_equal(situation_filtered()$constituency_name, "Alpha")
    session$setInputs(sit_escalated = FALSE, sit_warning = TRUE)
    expect_equal(situation_filtered()$constituency_name, "Alpha")
    session$setInputs(sit_warning = FALSE, sit_search = "g mp")
    expect_equal(situation_filtered()$constituency_name, "Gamma")      # matches on MP name, any case
    session$setInputs(sit_search = "zzz")
    expect_equal(nrow(situation_filtered()), 0L)
    expect_match(html_of(output$situation_kpis), "Forecast Medium or above")
  })
})

test_that("a briefing draft shows for a selected row and says so when there is none", {
  env <- make_server_env()
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)
    session$setInputs(situation_table_rows_selected = 1L)
    expect_match(html_of(output$briefing_panel), "DRAFT ONLY")
    expect_match(html_of(output$briefing_panel), "Subj")
    session$setInputs(situation_table_rows_selected = 2L)
    expect_match(html_of(output$briefing_panel), "No briefing draft")
  })
})

test_that("a failing product fetch degrades to a message, not a crash", {
  env <- make_server_env(list(fetch_situation = function() stop("nope"), fetch_exposure = function(n) stop("nope")))
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)
    expect_match(html_of(output$situation_note), "could not be read")
    expect_match(html_of(output$exposure_grid), "could not be read")
  })
})

test_that("exposure grid ranks constituencies, and the share measure divides by days covered", {
  env <- make_server_env()
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)
    h <- html_of(output$exposure_grid)
    expect_lt(regexpr("Alpha", h)[1], regexpr("Beta", h)[1])           # Alpha has 8 days, Beta 1
    session$setInputs(exp_metric = "share")
    h2 <- html_of(output$exposure_grid)
    expect_match(h2, "30%")                                            # 3 of 10 days
    expect_match(h2, "Overall")
  })
})

test_that("auto-refresh reloads only when the signature changes", {
  env <- make_server_env(auto_refresh = 30)
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)
    session$flushReact()
    s0 <- env$calls$statements
    # The stub signature changes on every call, so each tick looks like new data.
    session$elapse(31000)
    expect_gt(env$calls$statements, s0)
    # One change must cause one reload, not a loop. (The observer once read the
    # data version it was also writing, so it re-triggered itself.)
    expect_lte(env$calls$statements, s0 + 3L)
  })
  env2 <- make_server_env(list(fetch_data_signature = function() "same"), auto_refresh = 30)
  testServer(env2$server, {
    do.call(session$setInputs, base_inputs)
    session$flushReact()
    s0 <- env2$calls$statements
    session$elapse(31000); session$elapse(31000)
    expect_equal(env2$calls$statements, s0)
  })
})

test_that("auto-refresh can be switched off", {
  env <- make_server_env(auto_refresh = 0)
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)
    session$flushReact()
    session$elapse(600000)
    expect_equal(env$calls$signature, 0L)
  })
})

square <- function(id, name, extra = list()) {
  poly <- sf::st_sfc(sf::st_polygon(list(rbind(c(0, 50), c(1, 50), c(1, 51), c(0, 50)))), crs = 4326)
  sf::st_sf(data.frame(constituency_id = id, constituency_name = name, stringsAsFactors = FALSE), geometry = poly)
}

test_that("jumping to a constituency draws it without error, and a missing boundary is handled", {
  env <- make_server_env(list(fetch_constituency_shape = function(cid) square(cid, "Alpha")))
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)
    expect_no_error(session$setInputs(goto_constituency = "c1"))
  })
  env2 <- make_server_env(list(fetch_constituency_shape = function(cid) NULL))
  testServer(env2$server, {
    do.call(session$setInputs, base_inputs)
    expect_no_error(session$setInputs(goto_constituency = "c9"))
  })
})

test_that("the constituency risk layer handles data, no data and a failed query", {
  risk <- function() {
    s <- square("c1", "Alpha")
    s$fgs_worst_rank <- 4L; s$fgs_worst_level <- "High"; s$fgs_worst_date <- "2026-10-16"
    s$fgs_worst_sources <- "river"; s$fgs_escalated <- TRUE; s$severe_warnings <- 0L
    s$warnings <- 1L; s$alerts <- 0L; s$mp_name <- "A MP"; s
  }
  for (fn in list(risk, function() risk()[0, ], function() stop("down"))) {
    env <- make_server_env(list(fetch_constituency_risk_geometry = fn))
    testServer(env$server, {
      do.call(session$setInputs, base_inputs)
      expect_no_error(session$setInputs(layer_const_risk = TRUE))
      expect_no_error(session$setInputs(layer_const_risk = FALSE))
    })
  }
})

test_that("in the live app the briefing review buttons do nothing and are not shown", {
  env <- make_server_env()
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)
    session$setInputs(situation_table_rows_selected = 1L)
    expect_false(grepl("rv_reviewed", html_of(output$briefing_panel), fixed = TRUE))
    expect_false(grepl("demonstration", tolower(html_of(output$briefing_panel)), fixed = TRUE))
    session$setInputs(rv_reviewed = 1, rv_sent = 1, rv_rejected = 1)
    expect_length(review_status(), 0L)
    expect_equal(nrow(review_log()), 0L)
    expect_match(html_of(output$briefing_panel), "DRAFT ONLY")
  })
})

test_that("a map layer that errors cannot leave the spinner on, and the reason is shown", {
  # The failure behind a map that "always spins": a layer's query or geometry parse
  # throws, the observer stops partway, and its busy flag is never cleared.
  env <- make_server_env(list(fetch_ea_geometry = function(...) stop("geometry parse failed")))
  testServer(env$server, {
    do.call(session$setInputs, c(base_inputs, list(layer_ea_areas = TRUE, layer_polygons = TRUE)))
    session$setInputs(day_index = "2")                     # a change that makes every layer re-fetch
    expect_false(busy_ea())
    expect_false(busy_polygons())
    expect_false(busy_const())
  })
})

test_that("the map title shows the statement's real time, not midnight", {
  # The real warehouse hands issued_at back as text. as.POSIXct() on that reads 00:00.
  env <- make_server_env()
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)
    expect_match(output$map_title, "15 Oct 2026 09:30", fixed = TRUE)
    expect_false(grepl("00:00", output$map_title, fixed = TRUE))
    expect_false(grepl("UTC", output$map_title, fixed = TRUE))
  })
})


# ---- missing tables and unreachable warehouses ----------------------------------

missing_changes <- paste("[TABLE_OR_VIEW_NOT_FOUND] The table or view `prd_dash_lab`.`flood_forecasting_unrestricted`.`fgs_constituency_changes`",
                         "cannot be found. SQLSTATE: 42P01; line 12 pos 9")
missing_briefings <- sub("fgs_constituency_changes", "mp_briefing_drafts", missing_changes)

test_that("a missing changes table is explained on screen, not silently dropped", {
  env <- make_server_env(list(fetch_changes = function(sid) stop(missing_changes)))
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)
    expect_match(html_of(output$changes_strip), "Change tracking unavailable")
    expect_match(html_of(output$changes_strip), "fgs_constituency_changes")
    expect_match(html_of(output$changes_strip), "notebook 18")
    expect_match(html_of(output$ov_changes), "Change tracking unavailable")
    expect_false(grepl("No change", html_of(output$ov_changes)))                # not claimed as 'no change'
    expect_match(html_of(output$data_gaps_banner), "fgs_constituency_changes \\(pipeline notebook 18\\)")
    expect_match(html_of(output$data_gaps_banner), "Refresh statements")
  })
})

test_that("missing briefings are not mistaken for 'below the threshold'", {
  env <- make_server_env(list(fetch_briefing_drafts = function(...) stop(missing_briefings)))
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)
    session$setInputs(situation_table_rows_selected = 1L)
    h <- html_of(output$briefing_panel)
    expect_match(h, "Briefing drafts unavailable"); expect_match(h, "mp_briefing_drafts"); expect_match(h, "notebook 20")
    expect_false(grepl("below the pipeline", h))
    expect_match(html_of(output$data_gaps_banner), "mp_briefing_drafts")
  })
})

test_that("Refresh clears the gaps, and they reappear only if still missing", {
  built <- FALSE
  env <- make_server_env(list(fetch_changes = function(sid) if (!built) stop(missing_changes) else fake$changes(sid)))
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)
    expect_match(html_of(output$data_gaps_banner), "fgs_constituency_changes")
    built <<- TRUE
    session$setInputs(refresh_data = 1)
    expect_null(output$data_gaps_banner)
    expect_match(html_of(output$changes_strip), "1 up")
  })
})

test_that("no gap banner when nothing is missing", {
  env <- make_server_env()
  testServer(env$server, { do.call(session$setInputs, base_inputs); expect_null(output$data_gaps_banner) })
})

test_that("an unreachable warehouse is not reported as a missing table", {
  env <- make_server_env(list(fetch_changes = function(sid) stop("Failed to connect to warehouse 0a86ae40313bb7db: Failed to perform HTTP request.")))
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)
    expect_null(output$data_gaps_banner)
    expect_match(html_of(output$changes_strip), "could not be loaded")
  })
})

test_that("the console names the fetch that failed, once, not 'fn'", {
  env <- make_server_env(list(fetch_changes = function(sid) stop(missing_changes)))
  msgs <- character()
  withCallingHandlers(
    testServer(env$server, {
      do.call(session$setInputs, base_inputs)
      session$setInputs(statement_id = "6"); session$setInputs(statement_id = "7")      # switch back and forth
      invisible(html_of(output$changes_strip))
    }),
    message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
  failed <- grep("failed:", msgs, value = TRUE)
  expect_true(any(grepl("fetch_changes failed", failed)))
  expect_false(any(grepl("\\] fn failed", failed)))
  expect_lte(sum(grepl("fetch_changes failed", failed)), 1L)                 # logged once, however often it is asked
})

test_that("the look-back still works when the changes table is missing", {
  env <- make_server_env(list(
    fetch_changes = function(sid) stop(missing_changes),
    fetch_all_constituencies = function() data.table(constituency_id = c("c1", "c2"), constituency_name = c("Alpha", "Beta")),
    fetch_mp_lookup = function() data.table(constituency_name = "Alpha", mp_name = "A MP", mp_party = "P", mp_email = "e", mp_phone = "t"),
    fetch_constituency_intersections = function(sid) data.table(constituency_id = "c1", constituency_name = "Alpha",
      forecast_date = "2026-10-02", day_index = 1L, source = "river", risk_level = "Medium", intersection_pct = 1)))
  testServer(env$server, {
    do.call(session$setInputs, modifyList(base_inputs, list(sit_min_rank = "0")))
    session$setInputs(sit_statement = "6")
    expect_equal(poc_past_id(), 6L)
    s <- situation_clean()
    expect_equal(nrow(s), 2L)
    expect_equal(s[constituency_id == "c1", fgs_worst_rank], 3L)
    expect_true(all(is.na(s$fgs_escalated)))                                  # unknown, not 'none'
    expect_match(html_of(output$situation_kpis), "n/a")
  })
})

test_that("the Guide on the live server names the warehouse, and flags a table that is missing", {
  env <- make_server_env(list(fetch_changes = function(sid) stop(missing_changes)))
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)
    g <- html_of(output$guide_body)
    expect_match(g, "Live, reading the warehouse")
    expect_match(g, "Tables read from")
    expect_match(g, "Take Alpha")                                           # the worked example, from the stub situation
    expect_match(g, "Not built yet in this warehouse")                      # the changes card, because that table is missing
    expect_false(grepl("This demonstration", g))
  })
})

# ---- the changes log: 'nothing moved' versus 'not compared yet' ------------------------

no_changes <- function(sid) data.table(constituency_id = character(), constituency_name = character(), forecast_date = character(),
  day_index = integer(), source = character(), previous_rank = integer(), new_rank = integer(), rank_change = integer(),
  change_type = character())

log_row <- function(n = 0L, status = "compared")
  data.table(statement_id = 7L, previous_statement_id = 6L, previous_issued_at = "2026-10-14T09:30:00",
             days_compared_from = "2026-10-15", days_compared_to = "2026-10-18", n_changes = n, status = status)

changes_text <- function(log) {
  env <- make_server_env(list(fetch_changes = no_changes, fetch_changes_log = function(sid) log))
  out <- NULL
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)
    out <<- c(ov = html_of(output$ov_changes), strip = html_of(output$changes_strip))
  })
  out
}

test_that("a logged comparison with no differences says nothing moved, and against what", {
  t <- changes_text(log_row(0L))
  expect_match(t[["ov"]], "No change since statement #6 \\(issued Wed 14 Oct 09:30, Thu 15 to Sun 18\\)")
  expect_match(t[["strip"]], "No change since statement #6")
})

test_that("no log row means the comparison has not run, and the panel says so", {
  t <- changes_text(log_row()[0])
  expect_match(t[["ov"]], "Not yet compared with statement #6")
  expect_match(t[["strip"]], "Notebook 18 has not run")
  expect_false(grepl("No change since", t[["ov"]]))
})

test_that("statements sharing no days are reported as not compared", {
  expect_match(changes_text(log_row(0L, "no_shared_days"))[["ov"]], "share no forecast days")
})

test_that("an unavailable log table falls back to trusting the empty result, with no data gap", {
  env <- make_server_env(list(fetch_changes = no_changes, fetch_changes_log = function(sid) stop(sub("fgs_constituency_changes", "fgs_changes_log", missing_changes))))
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)
    expect_match(html_of(output$ov_changes), "No change since statement #6")
    expect_false(any(grepl("fgs_changes_log", html_of(output$data_gaps_banner))))
  })
})

# ---- potential warning and alert areas on Constituency situation ----------------------

potential_cells <- function(fetch) {
  env <- make_server_env(list(fetch_potential_areas = fetch))
  out <- NULL
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)
    session$setInputs(sit_min_rank = "0")
    out <<- situation_clean()[, .(constituency_id, pot_warning_areas, pot_alert_areas)]
  })
  out
}

test_that("areas under the forecast are attached per constituency, zero where none", {
  r <- potential_cells(function(sid, ...) data.table(constituency_id = c("c1", "c3"), warning_areas = c(2L, 0L), alert_areas = c(5L, 1L)))
  expect_equal(r$pot_warning_areas[r$constituency_id == "c1"], 2L)
  expect_equal(r$pot_alert_areas[r$constituency_id == "c3"], 1L)
  expect_equal(r$pot_warning_areas[r$constituency_id == "c2"], 0L)          # c2 is under no polygon
  expect_equal(r$pot_alert_areas[r$constituency_id == "c2"], 0L)
})

test_that("an empty result means none under any polygon, and a failed read shows n/a", {
  none <- potential_cells(function(sid, ...) data.table(constituency_id = character(), warning_areas = integer(), alert_areas = integer()))
  expect_true(all(none$pot_warning_areas == 0L))
  failed <- suppressMessages(potential_cells(function(sid, ...) stop(missing_changes)))
  expect_true(all(is.na(failed$pot_warning_areas)))
})

test_that("the minimum forecast risk filter is passed to the potential-areas query", {
  seen <- NULL
  env <- make_server_env(list(fetch_potential_areas = function(sid, min_rank = 0L) { seen <<- c(seen, min_rank); data.table(constituency_id = character(), warning_areas = integer(), alert_areas = integer()) }))
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)          # base sit_min_rank is "3"
    situation_clean()
    session$setInputs(sit_min_rank = "0")
    situation_clean()
  })
  expect_equal(seen, c(3L, 0L))
})
