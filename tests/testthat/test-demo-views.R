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
