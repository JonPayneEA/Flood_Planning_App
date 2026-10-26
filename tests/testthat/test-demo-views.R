skip_if_not_installed("leaflet")
skip_if_not_installed("DT")
skip_if_not_installed("sf")

# The genuine demo data layer behind the genuine server, no stubs. Built once.
demo_srv <- local({
  env <- new.env(parent = globalenv())
  consts <- load_app_constants()
  for (n in ls(consts, all.names = TRUE)) assign(n, get(n, consts), envir = env)
  assign("DEMO_MODE", TRUE, envir = env)
  assign("AUTO_REFRESH_SECONDS", 0, envir = env)
  assign("CACHE_TTL_SECONDS", 0, envir = env)
  old <- setwd(app_root); on.exit(setwd(old))
  source("guide.R", local = env)
  source("app_server.R", local = env)                  # sources data.R, which sources demo.R
  env$warehouse_calls <- 0L
  env$db_query <- function(sql) { env$warehouse_calls <- env$warehouse_calls + 1L; stop("blocked") }
  env
})

demo_inputs <- c(
  list(refresh_data = 0, statement_id = "4812", day_index = "1", sit_min_rank = "3", sit_search = "",
       sit_escalated = FALSE, sit_warning = FALSE, exp_source = "any",
       exp_metric = "days_medium_or_above", exp_top_n = 25,
       demo_scenario = "developing", demo_problem = FALSE,
       # the map's filters: every source, every matrix cell and both area types on
       src_river = TRUE, src_surface = TRUE, src_coastal = TRUE, src_ground = TRUE,
       ea_type_fwa = TRUE, ea_type_faa = TRUE, intersect_threshold = 25,
       layer_polygons = TRUE, layer_ea_areas = TRUE, layer_constituencies = FALSE),
  setNames(rep(list(TRUE), 16), paste0("mtx_", rep(1:4, each = 4), "_", rep(1:4, times = 4))))
txt <- function(x) gsub("\\s+", " ", as.character(x$html))

test_that("the Overview summarises the latest statement from the demo data", {
  testServer(demo_srv$server, {
    do.call(session$setInputs, demo_inputs)
    sit <- demo_srv$fetch_situation()
    h <- txt(output$ov_headline)
    expect_match(h, "Flood Guidance Statement #4812")
    expect_match(h, "Highest forecast risk")
    k <- txt(output$ov_kpis)
    expect_match(k, paste0(">", sum(sit$fgs_worst_rank >= 3L), "<"))
    expect_match(k, "Constituencies at Medium or above")
    expect_match(k, "MP briefings awaiting review")
    o <- txt(output$ov_outlook)
    expect_equal(lengths(regmatches(o, gregexpr('class="ov-day"', o))), 5L)
    w <- txt(output$ov_watch)
    expect_lte(lengths(regmatches(w, gregexpr("goto_constituency", w))), 8L)
    expect_match(w, "goto_constituency")
    expect_match(txt(output$ov_changes), "up,")
    f <- txt(output$ov_inforce)
    expect_match(f, "flood alerts.*flood warnings.*severe flood warnings")   # left to right: alerts, warnings, severe
    expect_match(f, "counted once")
    expect_false(grepl("id=\"ov_inforce_list\"", as.character(output$ov_inforce$html)))
    session$setInputs(ov_level = 3L)                                       # selecting a count lists those areas
    expect_match(txt(output$ov_inforce), "Flood alerts in force \\(\\d+\\)")
    expect_match(as.character(output$ov_inforce$html), "id=\"ov_inforce_list\"", fixed = TRUE)
    session$setInputs(ov_level = 3L)                                       # and selecting it again closes the list
    expect_false(grepl("id=\"ov_inforce_list\"", as.character(output$ov_inforce$html)))
  })
})

test_that("review actions change session state and nothing else", {
  testServer(demo_srv$server, {
    do.call(session$setInputs, demo_inputs)
    d <- situation_filtered()
    expect_gt(nrow(d), 2L)
    awaiting <- function() as.integer(sub('.*>([0-9]+)</div>\\s*<div class="stat-label">MP briefings.*', "\\1", txt(output$ov_kpis)))
    before <- awaiting()

    session$setInputs(situation_table_rows_selected = 1L)
    expect_match(txt(output$briefing_panel), "Status: draft")
    expect_match(txt(output$briefing_panel), "DRAFT ONLY")
    session$setInputs(rv_reviewed = 1)
    expect_equal(unname(review_status()[[d$constituency_id[1]]]), "reviewed")
    expect_match(txt(output$briefing_panel), "Status: reviewed")
    expect_match(txt(output$briefing_panel), "Recorded in this demonstration only")
    expect_false(grepl("DRAFT ONLY", txt(output$briefing_panel)))

    session$setInputs(situation_table_rows_selected = 2L)
    session$setInputs(rv_sent = 1)
    expect_equal(nrow(review_log()), 2L)
    expect_equal(review_log()$action, c("reviewed", "sent"))
    expect_equal(review_log()$constituency, d$constituency_name[1:2])
    expect_equal(awaiting(), before - 2L)
    expect_match(txt(output$ov_kpis), "2 actioned in this demo")

    session$setInputs(demo_reset = 1)
    expect_equal(nrow(review_log()), 0L)
    expect_length(review_status(), 0L)
    expect_equal(awaiting(), before)
  })
  expect_equal(demo_srv$warehouse_calls, 0L)
})

test_that("switching scenario rebuilds the data and clears review state", {
  testServer(demo_srv$server, {
    do.call(session$setInputs, demo_inputs)
    n_dev <- sum(situation_rv()$fgs_worst_rank >= 3L)
    session$setInputs(situation_table_rows_selected = 1L)
    session$setInputs(rv_reviewed = 1)
    expect_equal(nrow(review_log()), 1L)

    session$setInputs(demo_scenario = "severe")
    expect_gt(sum(situation_rv()$fgs_worst_rank >= 3L), n_dev)
    expect_equal(nrow(review_log()), 0L)

    session$setInputs(demo_scenario = "quiet")
    expect_equal(sum(situation_rv()$fgs_worst_rank >= 3L), 0L)
    expect_match(txt(output$ov_watch), "No constituency is forecast")
    expect_match(txt(output$ov_changes), "No change")
    expect_match(txt(output$ov_kpis), ">0<")
  })
  expect_equal(demo_srv$warehouse_calls, 0L)
})

test_that("the simulated pipeline problem raises the banner, and clears again", {
  testServer(demo_srv$server, {
    do.call(session$setInputs, demo_inputs)
    expect_null(output$stale_banner)
    session$setInputs(demo_problem = TRUE)
    expect_match(txt(output$stale_banner), "mp_details_age")
    expect_match(txt(output$stale_banner), "stale-bad")
    expect_match(txt(output$health_badge), "health-fail")
    session$setInputs(demo_problem = FALSE)
    expect_null(output$stale_banner)
  })
})

test_that("the map layers and the jump-to-constituency work on demo geometry", {
  testServer(demo_srv$server, {
    do.call(session$setInputs, demo_inputs)
    expect_no_error(session$setInputs(layer_const_risk = TRUE))
    expect_no_error(session$setInputs(layer_polygons = TRUE, layer_ea_areas = TRUE, layer_constituencies = TRUE))
    cid <- demo_srv$fetch_situation()$constituency_id[1]
    expect_no_error(session$setInputs(goto_constituency = cid))
    expect_no_error(session$setInputs(goto_constituency = "DEMO-C999"))     # unknown: handled, not a crash
    expect_gt(nrow(polygons()), 0L)
    expect_gt(nrow(ea_geometry()), 0L)
  })
  expect_equal(demo_srv$warehouse_calls, 0L)
})

test_that("the Rain gauges tab loads on first open, summarises, and takes its filters and a find", {
  testServer(demo_srv$server, {
    do.call(session$setInputs, demo_inputs)
    expect_null(rain_raw())                                                  # nothing is fetched until the tab is opened
    session$setInputs(main_tabs = "Rain gauges")
    expect_equal(nrow(rain_raw()), 24L)
    expect_match(txt(output$rain_summary), "23 gauges shown"); expect_match(txt(output$rain_summary), "1 more rain gauge has no position"); expect_match(txt(output$rain_summary), "without an FFFS ID")
    expect_null(output$rain_message)
    session$setInputs(rain_metric = "mm_1h"); expect_equal(rain_metric(), "mm_1h")
    session$setInputs(rain_metric = "nonsense"); expect_equal(rain_metric(), "mm_24h")      # an unknown window falls back
    session$setInputs(rain_area = "Demo North")
    expect_true(all(rain_view()$ea_area_name == "Demo North"))
    session$setInputs(rain_area = "All", rain_hide_stale = TRUE)
    expect_lt(nrow(rain_view()), 24L)
    session$setInputs(rain_hide_stale = FALSE)
    n_all <- nrow(rain_view())
    session$setInputs(rain_hide_suspect = TRUE)
    expect_equal(nrow(rain_view()), n_all - 2L)                                # the two suspect demo gauges are hidden
    expect_match(txt(output$rain_summary), "gauges shown")
    session$setInputs(rain_hide_suspect = FALSE)
    expect_equal(rain_cap(), 100)
    session$setInputs(rain_max_reading = 250); expect_equal(rain_cap(), 250)
    session$setInputs(rain_max_reading = 99999); expect_equal(rain_cap(), 1000)    # held to a sensible range
    session$setInputs(rain_max_reading = NA); expect_equal(rain_cap(), 100)
    expect_null(rain_chart_data())                                           # nothing is charted until a gauge is picked
    expect_match(txt(output$rain_chart_head), "Click gauges on the map")
    ok <- rain_raw()[stale == FALSE & placed == TRUE]$key
    click <- function(k) session$setInputs(rain_map_marker_click = list(id = k, lat = 52, lng = -1, group = "rain"))
    click(ok[1])
    cd <- rain_chart_data()
    expect_equal(cd$rows$key, ok[1]); expect_equal(length(cd$bars), 1L); expect_gt(nrow(cd$bars[[1]]), 0L); expect_equal(cd$step, 3600L)
    expect_lte(nrow(cd$bars[[1]]), 25L)                                      # 24 hours of hourly bars
    expect_match(txt(output$rain_chart_head), "in 24 hours, most in one hour")
    click(ok[2]); click(ok[3]); click(ok[2])                                  # a second click on a gauge changes nothing
    expect_equal(rain_picks(), ok[1:3]); expect_equal(nrow(rain_chart_data()$rows), 3L)
    expect_equal(length(rain_chart_data()$bars), 3L)
    session$setInputs(rain_bars = "900")
    expect_equal(rain_chart_data()$step, 900L); expect_gt(nrow(rain_chart_data()$bars[[1]]), 25L)
    session$setInputs(rain_bars = "junk"); expect_equal(rain_chart_data()$step, 3600L)
    for (k in ok[4:8]) click(k)                                               # the seventh and eighth drop the oldest
    expect_equal(length(rain_picks()), RAIN_MAX_CHARTED); expect_false(ok[1] %in% rain_picks()); expect_true(ok[8] %in% rain_picks())
    click("no-such-gauge"); expect_false("no-such-gauge" %in% rain_picks())
    session$setInputs(rain_charted = rain_picks()); session$elapse(600)       # the box repeating what was pushed to it: no change
    expect_equal(length(rain_picks()), RAIN_MAX_CHARTED)
    session$setInputs(rain_charted = rain_picks()[2:3]); session$elapse(600)  # removing gauges from the box un-charts them
    expect_equal(length(rain_picks()), 2L)
    session$setInputs(rain_charted = NULL); session$setInputs(rain_charted = rain_picks()); session$elapse(600)   # a blank then the value back, as a server-side box sends
    expect_equal(length(rain_picks()), 2L)                                    # is the echo of an update, not the user clearing it
    session$setInputs(rain_charted = c(rain_picks()[1], "bogus")); session$elapse(600); expect_equal(length(rain_picks()), 1L)
    session$setInputs(rain_clear = 1); expect_equal(length(rain_picks()), 0L); expect_null(rain_chart_data())
    session$setInputs(rain_find = ok[5]); expect_equal(rain_picks(), ok[5])    # finding a gauge charts it too
    expect_no_error(session$setInputs(rain_find = rain_raw()$key[1]))
    expect_no_error(session$setInputs(rain_find = "no-such-gauge"))
    expect_no_error(session$setInputs(rain_refresh = 1))
  })
  expect_equal(demo_srv$warehouse_calls, 0L)
})

test_that("the EA areas on the map and in the list follow the ticked risk matrix cells", {
  testServer(demo_srv$server, {
    do.call(session$setInputs, demo_inputs)
    all_areas <- ea_areas(); all_shape <- ea_geometry()
    expect_gt(nrow(all_areas), 0L)
    cell <- all_areas[1, .(x = risk_x, y = risk_y)]
    off <- setNames(rep(list(FALSE), 16), grep("^mtx_", names(demo_inputs), value = TRUE))
    do.call(session$setInputs, off)
    expect_equal(nrow(ea_areas()), 0L); expect_equal(nrow(ea_geometry()), 0L)             # no cell ticked: nothing shown
    do.call(session$setInputs, setNames(list(TRUE), paste0("mtx_", cell$x, "_", cell$y)))
    one <- ea_areas()
    expect_gt(nrow(one), 0L); expect_lte(nrow(one), nrow(all_areas))
    expect_true(all(one$risk_x == cell$x & one$risk_y == cell$y))
    shape <- ea_geometry()
    expect_gt(nrow(shape), 0L)
    expect_true(all(shape$risk_x == cell$x & shape$risk_y == cell$y))
  })
})

test_that("every other tab renders from the demo data", {
  testServer(demo_srv$server, {
    do.call(session$setInputs, demo_inputs)
    expect_match(txt(output$changes_strip), "Since the previous statement")
    expect_match(txt(output$exposure_grid), "exposure-grid")
    session$setInputs(exp_metric = "share")
    expect_match(txt(output$exposure_grid), "Overall")
    expect_gt(length(ea_areas()$ea_area_code), 0L)
    expect_no_error(output$download_ea_areas)
  })
  expect_equal(demo_srv$warehouse_calls, 0L)
})

test_that("POC look-back: an earlier statement is rebuilt, flagged, read-only and has no warnings", {
  testServer(demo_srv$server, {
    do.call(session$setInputs, demo_inputs)
    live <- situation_clean()
    expect_null(poc_past_id())
    expect_match(txt(output$situation_note), "Latest complete statement")

    session$setInputs(sit_statement = "4808", sit_min_rank = "0")
    expect_equal(poc_past_id(), 4808L)
    past <- situation_clean()
    expect_equal(unique(past$fgs_statement_id), 4808L)
    expect_false(identical(past$fgs_worst_rank, live$fgs_worst_rank))
    expect_true(all(is.na(past$has_active_warning)))
    n <- txt(output$situation_note)
    expect_match(n, "Look-back (proof of concept)", fixed = TRUE)
    expect_match(n, "not kept for past statements")
    expect_match(txt(output$situation_kpis), "n/a")

    # The Overview and the live reviews are untouched by looking back.
    expect_equal(txt(output$ov_headline), {session$setInputs(sit_statement = "latest"); txt(output$ov_headline)})
    session$setInputs(sit_statement = "4808", sit_min_rank = "3")
    session$setInputs(situation_table_rows_selected = 1L)
    expect_false(grepl("rv_reviewed", txt(output$briefing_panel), fixed = TRUE))
    session$setInputs(rv_reviewed = 1)
    expect_length(review_status(), 0L)
  })
  expect_equal(demo_srv$warehouse_calls, 0L)
})

test_that("POC look-back: choosing the newest statement is the live table, and a bad id falls back", {
  testServer(demo_srv$server, {
    do.call(session$setInputs, demo_inputs)
    session$setInputs(sit_statement = "4812"); expect_null(poc_past_id())     # newest = the live table, with its warnings
    session$setInputs(sit_statement = "99999"); expect_null(poc_past_id())
    session$setInputs(sit_statement = "latest"); expect_null(poc_past_id())
  })
})

test_that("POC look-back: briefing drafts for the past statement are shown when they exist", {
  testServer(demo_srv$server, {
    do.call(session$setInputs, demo_inputs)
    session$setInputs(sit_statement = "4811", sit_min_rank = "3")
    d <- situation_filtered()
    skip_if(nrow(d) == 0L)
    session$setInputs(situation_table_rows_selected = 1L)
    h <- txt(output$briefing_panel)
    expect_match(h, "briefing draft for")
    expect_match(h, "Warnings in force at the time are not held for this statement")
  })
})

test_that("POC look-back switches off completely with FGS_POC_FEATURES=0", {
  env <- new.env(parent = globalenv())
  consts <- load_app_constants()
  for (n in ls(consts, all.names = TRUE)) assign(n, get(n, consts), envir = env)
  assign("DEMO_MODE", TRUE, envir = env); assign("AUTO_REFRESH_SECONDS", 0, envir = env); assign("CACHE_TTL_SECONDS", 0, envir = env)
  assign("POC_FEATURES", FALSE, envir = env)
  old <- setwd(app_root); on.exit(setwd(old))
  source("app_ui.R", local = env)
  expect_false(grepl("sit_statement", as.character(env$make_situation_tab()), fixed = TRUE))
  assign("POC_FEATURES", TRUE, envir = env)
  expect_true(grepl("sit_statement", as.character(env$make_situation_tab()), fixed = TRUE))
  expect_true(grepl("POC only", as.character(env$make_situation_tab()), fixed = TRUE))

  # And the server ignores a look-back request when the switch is off.
  src <- new.env(parent = globalenv())
  for (n in ls(consts, all.names = TRUE)) assign(n, get(n, consts), envir = src)
  assign("DEMO_MODE", TRUE, envir = src); assign("AUTO_REFRESH_SECONDS", 0, envir = src); assign("CACHE_TTL_SECONDS", 0, envir = src)
  assign("POC_FEATURES", FALSE, envir = src)
  source("guide.R", local = src)
  source("app_server.R", local = src)
  testServer(src$server, {
    do.call(session$setInputs, demo_inputs)
    session$setInputs(sit_statement = "4808")
    expect_null(poc_past_id())
    expect_match(txt(output$situation_note), "Latest complete statement")
  })
})

test_that("every piece of look-back code is tagged so it can be found and removed", {
  files <- file.path(app_root, c("app_server.R", "app_ui.R", "data.R", "demo.R", "helpers.R", "global.R"))
  tagged <- vapply(files, function(f) sum(grepl("POC-ONLY", readLines(f, warn = FALSE, encoding = "UTF-8"), fixed = TRUE)), integer(1))
  expect_true(all(tagged >= 1L), info = paste(names(tagged)[tagged == 0L], collapse = ", "))
  # No look-back function is defined anywhere without a tag on or near it.
  for (f in c("data.R", "helpers.R")) {
    lines <- readLines(file.path(app_root, f), warn = FALSE, encoding = "UTF-8")
    for (fn in c("fetch_all_constituencies", "fetch_mp_lookup",
                 "fetch_situation_for_statement", "aggregate_situation", "normalise_name")) {
      i <- grep(paste0("^", fn, " *<- *"), lines)
      for (k in i) expect_true(any(grepl("POC-ONLY", lines[max(1L, k - 12L):k], fixed = TRUE)) ||
                                 any(grepl("POC-ONLY", lines[k:min(length(lines), k + 6L)], fixed = TRUE)),
                               info = paste(f, fn))
    }
  }
})

test_that("a quiet statement's empty table tells the user why, not just 'no match'", {
  testServer(demo_srv$server, {
    do.call(session$setInputs, demo_inputs)
    session$setInputs(sit_statement = "4805", sit_min_rank = "3")
    expect_equal(nrow(situation_filtered()), 0L)
    ranks <- situation_clean()$fgs_worst_rank
    msg <- empty_situation_message(ranks, 3L)
    expect_false(identical(msg, "No constituency matches these filters."))
    expect_match(msg, "Nothing in this statement is forecast at Medium or above")
  })
})

test_that("the Guide renders on the demo server with a worked example from the demo's own data", {
  testServer(demo_srv$server, {
    do.call(session$setInputs, demo_inputs)
    top <- situation_latest_clean()[order(-fgs_worst_rank, -severe_warnings, -warnings, constituency_name)][1L]
    g <- txt(output$guide_body)
    expect_match(g, "This demonstration")
    expect_match(g, paste0("Take ", top$constituency_name), fixed = TRUE)
    expect_match(g, "Statement look-back")
    expect_match(g, "Demonstration, on generated data")
    expect_false(grepl("Reading the warehouse|Tables read from", g))        # a demo has no warehouse to describe
    session$setInputs(demo_scenario = "quiet")
    expect_match(txt(output$guide_body), "no date|None")                    # a quiet statement still explains itself
  })
  expect_equal(demo_srv$warehouse_calls, 0L)
})

test_that("the Overview links to the Guide, and the Guide is a valid place to navigate to", {
  testServer(demo_srv$server, {
    do.call(session$setInputs, demo_inputs)
    expect_match(txt(output$ov_foot), "How each figure is calculated")
    expect_no_error(session$setInputs(nav_to = "Guide"))
  })
})


test_that("the situation tab narrows by flood source and impact, and says what the impact is", {
  testServer(demo_srv$server, {
    do.call(session$setInputs, demo_inputs)
    session$setInputs(sit_statement = "latest", sit_min_rank = "0")
    everyone <- nrow(situation_filtered())
    session$setInputs(sit_sources = "coastal")
    coastal <- situation_filtered()
    expect_lt(nrow(coastal), everyone)
    expect_true(all(coastal$fgs_worst_rank >= 1L))
    expect_true(all(grepl("coastal", coastal$fgs_worst_sources)))     # only coastal is left behind the risk
    expect_false(anyNA(coastal$fgs_worst_impact))
    session$setInputs(sit_sources = character(0), sit_min_impact = "3")
    impact <- situation_filtered()
    expect_gt(nrow(impact), 0L)
    expect_true(all(impact$fgs_worst_impact_x >= 3L))
    expect_true(all(impact$fgs_worst_impact %in% c("Significant", "Extreme")))
  })
})
