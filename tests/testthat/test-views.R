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
    fetch_exposure = fake$exposure, fetch_warning_history = function() data.table(), fetch_rt_register = function() data.table(), fetch_warning_episodes = function(cid) data.table(), fetch_changes = fake$changes, fetch_changes_log = function(sid) NULL,
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
  env <- make_server_env(list(fetch_potential_areas = function(sid, min_rank = 0L, sources = NULL) { seen <<- c(seen, min_rank); data.table(constituency_id = character(), warning_areas = integer(), alert_areas = integer()) }))
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)          # base sit_min_rank is "3"
    situation_clean()
    session$setInputs(sit_min_rank = "0")
    situation_clean()
  })
  expect_equal(seen, c(3L, 0L))
})

test_that("the Overview shows 30 to none beside a no-change result, and nothing extra without the counts", {
  lg <- log_row(0L)[, `:=`(flagged_previous = 30L, flagged_new = 0L, medium_plus_previous = 0L, medium_plus_new = 0L)]
  t <- changes_text(lg)
  expect_match(t[["ov"]], "No change since statement #6")
  expect_match(t[["ov"]], "30 in statement #6, none in this one")
  expect_match(t[["strip"]], "30 in statement #6, none in this one")
  expect_false(grepl("forecast risk: ", changes_text(log_row(0L))[["ov"]]))       # an older log: no counts line
})


# ---- the Nations filter ---------------------------------------------------------

nation_situation <- function() {
  d <- fake$situation()
  d[, constituency_id := c("E14000001", "W07000001", "S14000001")]
  d
}

test_that("the Nations filter limits every constituency view, and clearing it shows all four", {
  env <- make_server_env(list(fetch_situation = nation_situation))
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)
    session$setInputs(sit_min_rank = "0", nations = "E14")
    expect_equal(situation_filtered()$constituency_id, "E14000001")
    expect_equal(situation_latest_clean()$constituency_id, "E14000001")          # the Overview follows it too
    session$setInputs(nations = c("E14", "W07"))
    expect_equal(sort(situation_filtered()$constituency_id), c("E14000001", "W07000001"))
    session$setInputs(nations = NULL)                                           # cleared: every nation
    expect_equal(nrow(situation_filtered()), 3L)
  })
})

test_that("the Nations filter limits the changes, the briefing drafts and the exposure history", {
  chg <- function(sid) { x <- fake$changes(sid); x[, constituency_id := c("E14000001", "W07000001", "E14000001")]; x }
  brf <- function() { x <- fake$briefings(); x[, constituency_id := "W07000001"]; x }
  exp_ <- function(n) { x <- fake$exposure(n); x[, constituency_id := c("E14000001", "E14000001", "S14000001")]; x }
  env <- make_server_env(list(fetch_situation = nation_situation, fetch_changes = chg, fetch_briefing_drafts = brf, fetch_exposure = exp_))
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)
    session$setInputs(nations = "E14")
    expect_equal(sort(unique(changes()$constituency_id)), "E14000001")
    expect_equal(nrow(briefing_effective()), 0L)                                 # the only draft is a Welsh one
    expect_equal(unique(exposure_scoped()$constituency_id), "E14000001")
    session$setInputs(nations = "W07")
    expect_equal(unique(changes()$constituency_id), "W07000001")
    expect_equal(nrow(briefing_effective()), 1L)
  })
})


# ---- Warning history tab -------------------------------------------------------

hist_rows <- function() data.table(
  constituency_id = c("E14000001", "E14000001", "E14000001", "W07000001"),
  ea_area_code    = c("011FWFA", "011FWFA", "011WAFB", "011FWFA"),
  ea_area_name    = c("River A", "River A", "Coast B", "River A"),
  ea_area_type    = c("flood_warning", "flood_warning", "flood_alert", "flood_warning"),
  issued_date     = as.Date(c("2024-01-10", "2025-06-01", "2025-06-02", "2025-06-01")),
  n_issued        = c(2L, 1L, 4L, 3L),
  n_severe        = c(1L, 0L, 0L, 0L))

test_that("warning history counts alerts, warnings, severe and the total per constituency", {
  s <- warning_constituency_counts(hist_rows())
  e <- s[constituency_id == "E14000001"]
  expect_equal(c(e$alerts, e$warnings, e$severe, e$total), c(4L, 3L, 1L, 7L))
  expect_equal(s$constituency_id[1L], "E14000001")                               # highest total first
  w <- warning_constituency_counts(warning_history_window(hist_rows(), "2025-01-01", "2025-12-31"))
  expect_equal(w[constituency_id == "E14000001"]$total, 5L)                      # the 2024 issues fall outside
  expect_equal(nrow(warning_constituency_counts(NULL)), 0L)
  a <- warning_area_counts(hist_rows(), "E14000001")
  expect_equal(a$ea_area_code, c("011WAFB", "011FWFA"))                          # most issued first
  expect_equal(a$issued, c(4L, 3L))
  expect_equal(as.character(a$last_issued[2L]), "2025-06-01")
})

test_that("the Warning history tab follows the Nations filter and the period", {
  lk <- function() data.table(ea_area_code = "a", constituency_id = c("E14000001", "W07000001"),
                              constituency_name = c("Alpha", "Wylam"), constituency_overlap_pct = 1,
                              mp_name = c("A MP", "W MP"), mp_party = "p", mp_email = "e", mp_phone = "t")
  env <- make_server_env(list(fetch_warning_history = hist_rows, fetch_ea_constituency_mp = lk))
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)
    session$setInputs(nations = "E14", wh_range = as.Date(c("2020-01-01", "2026-01-01")), wh_search = "")
    expect_equal(wh_summary()$constituency_name, "Alpha")
    expect_equal(wh_summary()$total, 7L)
    session$setInputs(nations = NULL)
    expect_equal(wh_summary()$constituency_name, c("Alpha", "Wylam"))
    session$setInputs(wh_range = as.Date(c("2025-01-01", "2026-01-01")))
    expect_equal(wh_summary()$total, c(5L, 3L))
    session$setInputs(wh_search = "w mp")
    expect_equal(wh_summary()$constituency_name, "Wylam")
    session$setInputs(wh_search = "", wh_table_rows_selected = 1L)
    expect_equal(wh_area_table()$ea_area_code, c("011WAFB", "011FWFA"))
  })
})

test_that("the EA Area history tab groups by EA area and drills into its Target Areas", {
  ah <- function() data.table(ea_area_code = c("011FWFA", "011WAFB", "111FWFC"), ea_area_name = c("River A", "Coast B", "River C"),
                              ea_area = c("Wessex", "Wessex", "Thames"), ea_area_type = c("flood_warning", "flood_alert", "flood_warning"),
                              issued_date = as.Date(c("2025-06-01", "2025-06-02", "2025-06-03")), n_issued = c(2L, 4L, 1L), n_severe = c(1L, 0L, 0L),
                              minutes_timed = c(120, 300, 60), n_timed = c(2L, 4L, 1L), longest_minutes = c(90, 100, 60), n_open = c(0L, 0L, 0L))
  env <- make_server_env(list(fetch_area_history = ah))
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)
    session$setInputs(main_tabs = "EA Area history", eh_range = as.Date(c("2020-01-01", "2026-01-01")), eh_kind = "both", eh_search = "")
    expect_equal(eh_summary()$ea_area, c("Wessex", "Thames"))
    expect_equal(eh_summary()$areas, c(2L, 1L))
    session$setInputs(eh_kind = "flood_warning")
    expect_equal(eh_summary()$alerts, c(0L, 0L))
    session$setInputs(eh_kind = "both", eh_search = "tham")
    expect_equal(eh_summary()$ea_area, "Thames")
    session$setInputs(eh_search = "", eh_table_rows_selected = 1L)
    expect_equal(sort(eh_area_table()$ea_area_code), c("011FWFA", "011WAFB"))
  })
})

test_that("the EA area filter narrows Target Areas, and keeps each gauge's whole value on Gauge value", {
  owners <- function() data.table(area_id = c("A1", "A2", "A3"), ea_area = c("Wessex", "Thames", "Thames"))
  areas <- function() data.table(area_id = c("A1", "A2", "A3"), area_name = c("a", "b", "c"), kind = "flood_warning", county = "k",
                                 area_source = "fluvial", people = 1, risk_score = c(3, 2, 1), people_rs_high_medium = 1, share_people_high_medium = .1,
                                 households_rs_high_medium = 1, sites = 0L)
  env <- make_server_env(list(fetch_area_owners = owners, fetch_area_exposure = areas))
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)
    session$setInputs(main_tabs = "Target Area", areas_kind = "both", areas_source = "any", areas_sort = "risk_score", areas_county = "", areas_search = "", areas_ea = "")
    expect_equal(areas_table_data()$area_id, c("A1", "A2", "A3"))
    session$setInputs(areas_ea = "Thames")
    expect_equal(areas_table_data()$area_id, c("A2", "A3"))
    session$setInputs(areas_ea = "Nowhere")
    expect_equal(nrow(areas_table_data()), 0L)
  })
})

test_that("the EA area filter on Gauge value keeps a gauge's whole value across every EA area it drives", {
  owners <- function() data.table(area_id = c("A1", "A2", "A3"), ea_area = c("Wessex", "Thames", "Thames"))
  ex <- function() data.table(area_id = c("A1", "A2", "A3"), area_name = c("a", "b", "c"), kind = "flood_warning", county = "k", area_source = "fluvial",
                              people = c(500, 300, 100), households_rs_high = c(100, 40, 10), households_rs_medium = 0, households_rs_low = 0, households_rs_very_low = 0)
  links <- function() data.table(area_id = c("A1", "A2", "A2", "A3"), measure_uri = c("g1", "g1", "g2", "g3"), gauge = c("One", "One", "Two", "Three"), river = "R")
  p <- list(high = c(0.033, 0.05, 0.10), medium = c(0.01, 0.0182, 0.033), low = c(0.001, 0.00316, 0.01), very_low = c(0.0001, 0.000316, 0.001))
  assump <- function() rbind(
    do.call(rbind, lapply(names(p), function(b) data.frame(item = paste0("prob_", b), scenario = c("low", "central", "high"), value = p[[b]], unit = "", source = NA, price_year = NA, note = ""))),
    data.frame(item = "damage_per_household", scenario = c("low", "central", "high"), value = c(10000, 30000, 60000), unit = "", source = "test", price_year = "2024", note = ""))
  env <- make_server_env(list(fetch_area_owners = owners, fetch_area_exposure = ex, fetch_gauge_area_links = links, fetch_econ_assumptions = assump))
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)
    session$setInputs(main_tabs = "Gauge value", gv_ea = "")
    all_g <- gv_data()
    expect_setequal(all_g$measure_uri, c("g1", "g2", "g3"))
    session$setInputs(gv_ea = "Wessex")
    expect_equal(gv_data()$measure_uri, "g1")                                       # only g1 has an area in Wessex
    expect_equal(gv_data()$gross_central, all_g$gross_central[all_g$measure_uri == "g1"])   # and it keeps the value of its Thames area as well
    expect_equal(gv_data()$areas, 2L)
    session$setInputs(gv_ea = "Thames")
    expect_setequal(gv_data()$measure_uri, c("g1", "g2", "g3"))
    session$setInputs(gv_ea = "Nowhere")
    expect_equal(nrow(gv_data()), 0L)
    expect_equal(gv_value_of(c("g1", "g2")), round(all_g$split_central[match(c("g1", "g2"), all_g$measure_uri)]))   # the board is not narrowed by the filter
  })
})

test_that("a missing episodes table is named on screen", {
  env <- make_server_env(list(fetch_warning_history = function() stop("[TABLE_OR_VIEW_NOT_FOUND] The table or view `cat`.`sch`.`ea_warning_episodes` cannot be found")))
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)
    expect_match(html_of(output$wh_note), "ea_warning_episodes")
    expect_match(html_of(output$wh_note), "notebook 25")
  })
})


# ---- Hydrometrics tab --------------------------------------------------------------

test_that("the Hydrometrics tab loads the register on first open and fetches readings for the chosen measures", {
  reg <- function() data.table(station_label = c("Aldwyn", "Aldwyn"), station_reference = "S1", station_uri = "u/S1",
                               measure_uri = c("m/1", "m/2"), parameter = c("level", "flow"), unit_name = c("mASD", "m3/s"),
                               river_name = "Aldwyn", ea_area_name = "North")
  asked <- new.env(); asked$n <- 0L; asked$uris <- NULL
  rd <- function(uris, days, sum_uris = character()) { asked$n <- asked$n + 1L; asked$uris <- uris; asked$days <- days
    data.table(measure_uri = uris, date_time = Sys.time() - 600, value = 1) }
  env <- make_server_env(list(fetch_rt_register = reg, fetch_rt_readings = rd, fetch_rt_boundary = function() NULL,
                                  fetch_rt_span = function(uris) Sys.time() - 400 * 86400 + 60))
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)
    expect_null(rt_reg())                                                    # not loaded until the tab is opened
    session$setInputs(main_tabs = "Hydrometrics")
    expect_equal(nrow(rt_reg()), 2L)
    expect_match(html_of(output$rt_message), "Choose one or more measures")
    expect_equal(asked$n, 0L)                                                # nothing fetched until measures are chosen
    session$setInputs(rt_measures = c("m/1", "m/2"), rt_days = 3L, rt_refresh = 0)
    expect_equal(asked$uris, c("m/1", "m/2"))
    # The FFFS box is server-side: updating it sends back a blank and then the value. That echo must not set off more syncs.
    n0 <- rt_state$syncs
    session$setInputs(rt_fffs = ""); session$setInputs(rt_fffs = "All"); session$elapse(600)
    session$setInputs(rt_fffs = ""); session$setInputs(rt_fffs = "All"); session$elapse(600)
    expect_lte(rt_state$syncs - n0, 2L)                                       # one per settled change, never a runaway
    n1 <- rt_state$syncs
    session$elapse(5000)
    expect_equal(rt_state$syncs, n1)                                          # and nothing more once it is quiet
    expect_match(html_of(output$rt_summary), "2 readings")
    session$setInputs(rt_days = "0")                                          # all available: sized from the earliest reading
    expect_equal(asked$days, 400L)
    session$setInputs(rt_measures = sprintf("m/%d", 1:9))
    expect_match(html_of(output$rt_message), "Pick 8 or fewer")
  })
})



test_that("choosing an FFFS ID in Hydrometrics re-syncs the drop-downs without error, and an unknown one falls back to All", {
  reg <- function() data.table(station_label = c("Aldwyn", "Aldwyn", "Brenn"), station_reference = c("S1", "S1", "S2"),
                               station_uri = c("u/S1", "u/S1", "u/S2"), measure_uri = c("m/1", "m/2", "m/3"),
                               parameter = c("level", "flow", "level"), unit_name = c("mASD", "m3/s", "mASD"), river_name = c("Aldwyn", "Aldwyn", "Brenn"),
                               ea_area_name = "North", fffs_id = c("F1", "F1", "F2"))
  env <- make_server_env(list(fetch_rt_register = reg, fetch_rt_boundary = function() NULL))
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)
    session$setInputs(main_tabs = "Hydrometrics")
    expect_equal(rt_options(rt_reg(), fffs = "F1")$stations, c("All", "Aldwyn"))
    expect_no_error(session$setInputs(rt_fffs = "F1"))
    expect_no_error(session$setInputs(rt_fffs = "NOPE"))
    expect_no_error(session$setInputs(rt_fffs = ""))
  })
})

test_that("ticking Detailed lists each alert and warning with its date and time", {
  lk <- function() data.table(ea_area_code = "a", constituency_id = "E14000001", constituency_name = "Alpha",
                              constituency_overlap_pct = 1, mp_name = "A MP", mp_party = "p", mp_email = "e", mp_phone = "t")
  eps <- function(cid) data.table(episode_id = c("e1", "e2"), ea_area_code = c("A", "B"), ea_area_name = c("Alder", "Beech"),
    ea_area_type = c("flood_warning", "flood_alert"), start_time = as.POSIXct(c("2025-06-01 10:00", "2025-06-02 08:30"), tz = "UTC"),
    end_time = as.POSIXct(c("2025-06-01 14:00", "2025-06-02 09:30"), tz = "UTC"), duration_minutes = c(240, 60),
    peak_level = c("flood_warning", "flood_alert"), had_severe = FALSE, stale_open = FALSE)
  env <- make_server_env(list(fetch_warning_history = hist_rows, fetch_ea_constituency_mp = lk, fetch_warning_episodes = eps))
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)
    session$setInputs(nations = "E14", wh_range = as.Date(c("2020-01-01", "2026-01-01")), wh_search = "", wh_table_rows_selected = 1L)
    expect_equal(names(wh_second_table())[1:3], c("Area", "Code", "Type"))                 # the area counts by default
    expect_true("Last issued" %in% names(wh_second_table()))
    session$setInputs(wh_detailed = TRUE)
    t <- wh_second_table()
    expect_equal(t$Issued, c("02 Jun 2025 08:30", "01 Jun 2025 10:00"))
    expect_equal(t$Ended[2L], "01 Jun 2025 14:00")
    session$setInputs(wh_range = as.Date(c("2025-06-02", "2025-06-30")))                  # the slider narrows it
    expect_equal(nrow(wh_second_table()), 1L)
  })
})



test_that("Warnings vs FGS shows the shares, the warnings and the statements behind a selected warning", {
  seen <- new.env(); seen$args <- NULL; seen$detail <- NULL
  span <- function() list(first = as.Date("2024-01-01"), last = as.Date("2026-10-20"))
  summ <- function(from, to, type = "any") {
    seen$args <- list(from = from, to = to, type = type)
    data.table(threshold = c(1L, 1L, 1L, 3L), category = c("before", "later", "none", "none"), lead_days = c(2L, NA, NA, NA), n = c(6L, 2L, 2L, 10L))
  }
  lst <- function(from, to, type = "any", min_rank = 1L, limit = 20000L) data.table(
    episode_id = c("e1", "e2"), ea_area_code = c("A1", "B1"), ea_area_name = c("Alder", "Beech"), area_type = c("flood_warning", "flood_alert"),
    start_time = c("2026-10-10T14:00:00", "2026-10-09T08:00:00"), end_time = c("2026-10-11T02:00:00", NA), peak_level = c("flood_warning", "flood_alert"),
    had_severe = FALSE, stale_open = FALSE, issue_date = as.Date(c("2026-10-10", "2026-10-09")), max_rank = c(3L, 0L),
    issue_day_rank = c(3L, 0L), later_rank = c(3L, 0L), first_cover = as.Date(c("2026-10-08", NA)), sources = c("river", NA))
  det <- function(code, from, to) {
    seen$detail <- list(code = code, from = from, to = to)
    data.table(statement_id = 5L, issued_at = "2026-10-08T09:30:00", forecast_date = "2026-10-10", day_index = 3L, source = "river",
               risk_level = "Medium", impact_label = "Minor", likelihood_label = "Medium", intersection_pct = 0.4, link_type = "intersects")
  }
  env <- make_server_env(list(fetch_coverage_span = span, fetch_coverage_summary = summ, fetch_coverage_warnings = lst, fetch_coverage_detail = det))
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)
    session$setInputs(main_tabs = "Warnings vs FGS", cov_range = as.Date(c("2026-01-01", "2026-10-20")), cov_type = "warnings", cov_rank = 1L)
    expect_equal(seen$args$type, "warnings")
    s <- cov_shares_data()[threshold == 1L]
    expect_equal(c(s$before, s$later, s$none, s$total), c(6, 2, 2, 10))
    expect_equal(s$covered_pct, 0.6)
    expect_match(html_of(output$cov_kpis), "60%")                                           # before is 6 of 10
    tbl <- cov_table_data()
    expect_equal(tbl$`FGS coverage`, c("Before (2 days)", "Not covered"))
    expect_equal(tbl$`Highest FGS risk`, c("Medium", "None"))
    session$setInputs(cov_table_rows_selected = 1L)
    d <- cov_detail_data()
    expect_equal(seen$detail$code, "A1"); expect_equal(as.character(seen$detail$from), "2026-10-10"); expect_equal(as.character(seen$detail$to), "2026-10-11")
    expect_equal(d$`Compared with the warning`, "Before")
    expect_equal(d$Timing, "2 d 4 h before")
    expect_match(html_of(output$cov_detail_title), "Alder")
  })
})

test_that("a missing coverage table is named on screen", {
  env <- make_server_env(list(
    fetch_coverage_span = function() NULL,
    fetch_coverage_summary = function(...) stop("[TABLE_OR_VIEW_NOT_FOUND] The table or view `cat`.`sch`.`ea_warning_fgs_coverage` cannot be found")))
  testServer(env$server, {
    do.call(session$setInputs, base_inputs)
    session$setInputs(main_tabs = "Warnings vs FGS", cov_range = as.Date(c("2026-01-01", "2026-10-20")), cov_type = "any", cov_rank = 1L)
    cov_summary()
    expect_match(html_of(output$cov_note), "ea_warning_fgs_coverage")
    expect_match(html_of(output$cov_note), "notebook 26")
  })
})
