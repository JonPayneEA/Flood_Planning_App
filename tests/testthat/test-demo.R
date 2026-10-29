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
    ae <- demo_env$fetch_area_exposure()
    expect_true(all(c("kind", "area_id", "people", "people_rs_high_medium", "people_unique", "people_split", "share_people_shared") %in% names(ae)))
    expect_true(all(ae$people_rs_high_medium <= ae$people) && all(ae$people_unique <= ae$people))
    expect_true(all(ae$people_rs_very_low + ae$people_rs_low + ae$people_rs_medium + ae$people_rs_high <= ae$people))
    expect_true(all(c("flood_warning", "flood_alert") %in% ae$kind))
    if (requireNamespace("sf", quietly = TRUE)) {
      gr <- demo_env$fetch_risk_grid(-1.56, 53.79, -1.52, 53.82, 10, 500L)
      expect_true(nrow(gr) > 0L && nrow(gr) <= 500L && all(gr$risk_score >= 10) && !is.unsorted(rev(gr$risk_score)))
    }
    cv <- demo_env$fetch_area_coverage()
    expect_setequal(cv$kind, c("all", "flood_warning", "flood_alert"))
    ce <- demo_env$fetch_constituency_exposure()
    expect_true(all(ce$people_high_medium_not_in_warning_area >= 0) && all(ce$people_rs_high_medium <= ce$people))
    expect_true(all(c("households_rs_high_medium", "households_rs_high", "households_rs_low") %in% names(ae)))
    if (requireNamespace("sf", quietly = TRUE)) expect_s3_class(demo_env$fetch_area_shape(ae$area_id[1L]), "sf")
    big <- ae$area_id[which.max(ae$sites)]
    st <- demo_env$fetch_area_sites(big)
    expect_equal(nrow(st), ae$sites[ae$area_id == big]); expect_false(is.unsorted(rev(st$rofrs_rank)))
    geo <- demo_env$fetch_area_geographies(big)
    expect_true(all(names(RESPONDER_GEOGRAPHIES) %in% geo$geography))
    for (gg in setdiff(unique(geo$geography), "combined_authority")) expect_equal(sum(geo[geography == gg]$share_of_area_people), 1, tolerance = 0.01)
    expect_lt(sum(geo[geography == "combined_authority"]$share_of_area_people), 1)
    pr <- demo_env$fetch_area_overlap_pairs()
    expect_true(all(c("kind", "area_id_a", "area_id_b", "people") %in% names(pr)))
    epi <- demo_env$fetch_area_episodes(ae$area_id[1L])
    expect_true(all(c("episode_id", "ea_area_code", "ea_area_type", "start_time", "end_time", "duration_minutes", "peak_level", "had_severe", "stale_open") %in% names(epi)))
    expect_gt(nrow(epi), 0L)
    rows <- warning_episode_rows(cbind(data.table::as.data.table(epi), ea_area_name = "x"), as.Date("1900-01-01"), Sys.Date() + 1L)       # what the area page does
    expect_equal(nrow(rows), nrow(epi)); expect_true(all(c("Issued", "Ended", "Duration", "Highest level", "Severe") %in% names(rows)))
    pl <- demo_env$fetch_potential_area_list(4812L, 2L)
    expect_true(all(c("ea_area_code", "ea_area_name", "ea_area_type", "worst_rank", "first_day") %in% names(pl))); expect_true(all(pl$worst_rank >= 2L))
    cs <- demo_env$fetch_constituency_sites("DEMO-C001"); expect_true(all(c("name", "site_type", "band", "rofrs_rank") %in% names(cs)))
    expect_true(all(c("sites", "sites_rs_high_medium") %in% names(demo_env$fetch_constituency_exposure())))
    ov <- demo_env$fetch_area_overlaps(ae$area_id[1L])
    expect_true(all(c("area_id_a", "area_id_b", "people", "name_a", "name_b") %in% names(ov)))
    expect_no_error(demo_env$fetch_warning_episodes(demo_env$fetch_warning_history()$constituency_id[1L]))
    sp <- demo_env$fetch_coverage_span()
    cs <- demo_env$fetch_coverage_summary(sp$first, sp$last, "any")
    sh <- coverage_shares(cs)
    expect_true(all(sh$total == sh$total[1]) && sh$total[1] > 500L)                        # every threshold counts the same warnings
    expect_true(all(diff(sh$covered_pct) <= 1e-9))                                         # a stricter polygon never covers more
    w <- demo_env$fetch_coverage_warnings(sp$first, sp$last, "warnings", 2L)
    expect_gt(nrow(w), 100L)
    d <- demo_env$fetch_coverage_detail(w[!is.na(first_cover)]$ea_area_code[1L], as.Date(substr(w[!is.na(first_cover)]$start_time[1L], 1L, 10L)), Sys.Date())
    expect_gt(nrow(d), 0L)
    wh <- demo_env$fetch_warning_history()
    expect_true(nrow(wh) > 0L && all(c("constituency_id", "ea_area_name", "issued_date", "n_issued", "n_severe") %in% names(wh)))
    expect_no_error(demo_env$fetch_changes(4812L))
    rg <- demo_env$fetch_rain_gauges()
    expect_true(all(c("measure_uri", "lat", "long", "mm_1h", "mm_12h", "mm_24h", "last_time", "value_type", "fffs_id") %in% names(rg)))
    prepared <- rain_prepare(rg)
    expect_equal(nrow(prepared), 24L); expect_equal(sum(!prepared$placed), 1L); expect_true(all(is.finite(prepared$lat[prepared$placed]))); expect_equal(sum(rain_suspect(prepared, "mm_24h")), 2L); expect_true(any(prepared$stale)); expect_true(any(!prepared$stale & prepared$mm_24h > 5, na.rm = TRUE))
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

test_that("demo statements look like the warehouse's: newest first, times as text", {
  st <- demo_env$fetch_recent_statements(100L)
  expect_equal(nrow(st), 8L)
  expect_true(is.character(st$issued_at))                  # as the real table returns it
  expect_equal(st$statement_id, sort(st$statement_id, decreasing = TRUE))
  expect_equal(format(as_utc(st$issued_at[1]), "%H:%M"), "09:30")
  expect_equal(format(as.POSIXct(st$issued_at[1], tz = "UTC"), "%H:%M"), "00:00")   # the trap as_utc avoids
})

# The real look-back path (aggregate_situation) against the demo's own, independent
# model. The demo builds its situation table from the cell field and polygons; the
# real path rebuilds it from intersection rows alone. They must agree.
rebuild_from_intersections <- function(scenario, sid, min_pct = 0.25) {
  g <- demo_env$demo_geometry()
  inter <- rbindlist(lapply(1:5, function(d) {
    x <- demo_env$demo_const_inter(scenario, sid, d)
    if (nrow(x) == 0L) return(NULL)
    x[, day_index := d]
    x[, .(constituency_id, constituency_name, forecast_date = as.character(forecast_date), day_index, source, risk_level, intersection_pct)]
  }))
  mp <- g$table[, .(constituency_name, mp_name, mp_party, mp_email, mp_phone)]
  st <- demo_env$demo_stmt_row(scenario, sid)
  aggregate_situation(inter, demo_env$demo_changes_for(scenario, sid), mp,
                      g$table[, .(constituency_id, constituency_name)], sid, st$issued_at, min_pct = min_pct)
}

for (s in c("developing", "severe")) {
  test_that(paste("the real look-back path reproduces the demo's situation table:", s), {
    for (sid in c(4812L, 4810L, 4806L)) {
      mine <- rebuild_from_intersections(s, sid)
      demo <- demo_env$demo_situation_for(s, sid)
      cols <- c("constituency_id", "fgs_worst_rank", "fgs_worst_level", "fgs_worst_date", "fgs_worst_sources",
                paste0("fgs_rank_day", 1:5), "fgs_escalated", "fgs_changed", "mp_name", "mp_matched")
      setkey(mine, constituency_id); setkey(demo, constituency_id)
      expect_equal(as.data.frame(mine[, ..cols]), as.data.frame(demo[, ..cols]), info = paste(s, sid))
    }
  })
}

test_that("past statements have no warnings in force, the newest does", {
  past <- demo_env$demo_situation_for("severe", 4810L)
  expect_true(all(is.na(past$severe_warnings) & is.na(past$warnings) & is.na(past$alerts) & is.na(past$has_active_warning)))
  now <- demo_env$demo_situation_for("severe", 4812L)
  expect_false(any(is.na(now$warnings)))
  expect_gt(sum(now$has_active_warning), 0L)
})

test_that("look-back statements differ from today's, and an unknown id is refused cleanly", {
  a <- demo_env$demo_situation_for("developing", 4812L); b <- demo_env$demo_situation_for("developing", 4807L)
  expect_gt(sum(a$fgs_worst_rank >= 3L), sum(b$fgs_worst_rank >= 3L))        # the event builds up
  expect_equal(unique(b$fgs_statement_id), 4807L)
  expect_null(demo_env$fetch_situation_for_statement(9999L))
})

test_that("briefings exist for past statements and are built from that statement", {
  d <- demo_env$demo_briefings_for("developing", 4810L)
  if (nrow(d) > 0L) {
    expect_equal(unique(d$statement_id), 4810L)
    expect_false(any(grepl("NA", d$body, fixed = TRUE)))
    expect_true(all(grepl("not held for this statement", d$body)))
  }
  expect_equal(unique(demo_env$demo_briefings_for("severe", 4812L)$statement_id), 4812L)
})

test_that("demo potential areas are well formed and agree with the intersections they come from", {
  sid <- max(demo_env$demo_statements_table("developing")$statement_id)
  p <- demo_env$fetch_potential_areas(sid)                      # the default scenario is developing
  expect_true(all(c("constituency_id", "warning_areas", "alert_areas") %in% names(p)))
  expect_false(anyDuplicated(p$constituency_id) > 0L)
  expect_true(all(p$warning_areas >= 0L & p$alert_areas >= 0L))
  expect_gt(sum(p$warning_areas + p$alert_areas), 0L)           # a developing storm puts some areas under polygons
  ea <- data.table::rbindlist(lapply(1:5, function(d) demo_env$demo_ea_inter("developing", sid, d)))
  expect_lte(max(p$warning_areas), data.table::uniqueN(ea$ea_area_code[ea$ea_area_type == "flood_warning"]))
})
