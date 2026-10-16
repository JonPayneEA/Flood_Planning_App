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
  # server.R sources data.R by relative path, so load it from the project root.
  old_wd <- setwd(app_root); on.exit(setwd(old_wd), add = TRUE)
  source("server.R", local = env)   # also sources data.R
  setwd(old_wd)

  env$calls <- new.env()
  env$calls$statements <- 0L; env$calls$health <- 0L; env$calls$signature <- 0L
  stubs <- list(
    db_query = function(sql) data.table(),
    fetch_recent_statements = function(n) { env$calls$statements <- env$calls$statements + 1L; fake$statements(n) },
    fetch_pipeline_health = function() { env$calls$health <- env$calls$health + 1L; fake$health() },
    fetch_situation = fake$situation, fetch_briefing_drafts = fake$briefings,
    fetch_exposure = fake$exposure, fetch_changes = fake$changes,
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
