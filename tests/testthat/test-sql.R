test_that("global.R rejects an unknown geometry source", {
  expect_error(load_app_constants(list(FGS_GEOMETRY_SOURCE = "banana")), "risk_areas")
  expect_equal(get("GEOMETRY_SOURCE", load_app_constants(list(FGS_GEOMETRY_SOURCE = "aoc"))), "aoc")
  expect_equal(get("GEOMETRY_SOURCE", load_app_constants()), "risk_areas")
})

test_that("auto-refresh interval is validated", {
  expect_error(load_app_constants(list(FGS_AUTO_REFRESH_SECONDS = "10")), "at least 30")
  expect_error(load_app_constants(list(FGS_AUTO_REFRESH_SECONDS = "abc")), "number")
  expect_equal(get("AUTO_REFRESH_SECONDS", load_app_constants(list(FGS_AUTO_REFRESH_SECONDS = "0"))), 0)
})

for (src in c("risk_areas", "aoc")) {
  test_that(paste("every query on a geometry_source-tagged table filters on", src), {
    d <- load_data_layer(src)
    suppressWarnings({
      d$fetch_ea_areas_for_statement(1, 1)
      d$fetch_summary_counts(1, 1)
      d$fetch_ea_geometry(1, 1)
      d$fetch_constituency_geometry(1, 1)
      d$fetch_constituencies_for_statement(1, 1)
      d$fetch_changes(1)
      d$fetch_briefing_drafts()
      d$fetch_potential_areas(1)
    })
    expect_length(d$captured, 8L)
    for (q in d$captured) {
      expect_match(q, sprintf("geometry_source\\s*=\\s*'%s'", src), info = substr(q, 1, 120))
    }
    # The summary query touches two tagged tables and must filter both.
    counts <- d$captured[grepl("polygon_count", d$captured)]
    expect_equal(lengths(regmatches(counts, gregexpr("geometry_source", counts))), 2L)
  })
}

test_that("a constituency id from the browser cannot inject SQL", {
  d <- load_data_layer()
  suppressWarnings(d$fetch_constituency_shape("E1'; DROP TABLE x; --"))
  q <- d$captured[1L]
  expect_false(grepl(";", q, fixed = TRUE))
  expect_false(grepl("--", q, fixed = TRUE))
  # The id reaches the query only as plain letters and digits between its own quotes.
  expect_match(q, "constituency_id = 'E1DROPTABLEx'")
  # Quotes in the whole statement: the id's pair only.
  expect_equal(lengths(regmatches(q, gregexpr("'", q))), 2L)
})

test_that("queries read the configured catalog and schema, and no table by bare name", {
  d <- load_data_layer()
  suppressWarnings({ d$fetch_changes(1); d$fetch_situation(); d$fetch_pipeline_health(); d$fetch_exposure(12) })
  for (q in d$captured) expect_match(q, "cat\\.sch\\.")
})

test_that("the warning history query links areas by the overlap floor and counts by day of issue", {
  d <- load_data_layer()
  suppressWarnings(d$fetch_warning_history())
  q <- gsub("\\s+", " ", d$captured[length(d$captured)])
  expect_match(q, "cat\\.sch\\.ea_warning_episodes")
  expect_match(q, "l.intersection_pct >= ")
  expect_match(q, "CAST\\(e.start_time AS DATE\\)")
  expect_match(q, "SUM\\(CASE WHEN e.had_severe")
})

test_that("the potential-areas query counts distinct areas, with the share floor and the lookup join", {
  d <- load_data_layer("risk_areas")
  suppressWarnings(d$fetch_potential_areas(7, 3L))
  q <- gsub("\\s+", " ", d$captured[length(d$captured)])
  expect_match(q, "COUNT\\(DISTINCT CASE WHEN LOWER\\(e.ea_area_type\\) = 'flood_warning'")
  expect_match(q, "COUNT\\(DISTINCT CASE WHEN LOWER\\(e.ea_area_type\\) = 'flood_alert'")
  expect_match(q, "statement_id = 7")
  expect_match(q, "JOIN cat.sch.ea_area_constituency_lookup l ON l.ea_area_code = e.ea_area_code", fixed = FALSE)
  expect_match(q, "WHEN 'Medium' THEN 3 WHEN 'High' THEN 4 ELSE 0 END >= 3")      # min_rank reaches the SQL
  expect_match(q, "l.intersection_pct >= 0.01")
  expect_match(q, "GROUP BY l.constituency_id")
})


test_that("the nations are known and England is the default", {
  consts <- load_app_constants()
  expect_equal(unname(consts$NATIONS), c("E14", "W07", "S14", "N06"))
  expect_equal(names(consts$NATIONS), c("England", "Wales", "Scotland", "Northern Ireland"))
  expect_equal(consts$DEFAULT_NATIONS, "E14")
  expect_equal(get("DEFAULT_NATIONS", load_app_constants(list(FGS_DEFAULT_NATIONS = "E14, W07"))), c("E14", "W07"))
  expect_error(load_app_constants(list(FGS_DEFAULT_NATIONS = "X99")), "FGS_DEFAULT_NATIONS")
})

test_that("the EA area lookup carries the constituency id, so it can follow the Nations filter", {
  d <- load_data_layer()
  suppressWarnings(d$fetch_ea_constituency_mp())
  expect_match(d$captured[1L], "l\\.constituency_id")
})


test_that("the readings query takes only URL characters from a measure id, and bounds the window", {
  d <- load_data_layer()
  suppressWarnings(d$fetch_rt_readings(c("http://x/id/measures/1-level-i-15_min-mASD", "a'); DROP TABLE t; --"), 3))
  q <- gsub("\\s+", " ", d$captured[length(d$captured)])
  expect_match(q, "cat\\.sch\\.ea_rt_readings_all")
  expect_match(q, "INTERVAL 3 DAYS")
  expect_false(grepl(";", q, fixed = TRUE)); expect_false(grepl("'a'", q, fixed = TRUE))
  expect_equal(lengths(regmatches(q, gregexpr("'", q))), 4L)                           # two ids, each in its own pair of quotes
})


test_that("the episodes query is for one constituency, takes letters and digits only, and uses the overlap floor", {
  d <- load_data_layer()
  suppressWarnings(d$fetch_warning_episodes("E14000001'; DROP TABLE x; --"))
  q <- gsub("\\s+", " ", d$captured[length(d$captured)])
  expect_match(q, "l.constituency_id = 'E14000001DROPTABLEx'")
  expect_match(q, "l.intersection_pct >= ")
  expect_false(grepl(";", q, fixed = TRUE))
})


test_that("a long readings window is summarised in the warehouse, rainfall totalled and the rest averaged", {
  d <- load_data_layer()
  suppressWarnings(d$fetch_rt_readings(c("http://x/m/1", "http://x/m/2"), 365, sum_uris = "http://x/m/2"))
  q <- gsub("\\s+", " ", d$captured[length(d$captured)])
  expect_match(q, "GROUP BY measure_uri")
  expect_match(q, "CASE WHEN measure_uri IN \\('http://x/m/2'\\) THEN SUM\\(value\\) ELSE AVG\\(value\\) END")
  expect_match(q, "INTERVAL 365 DAYS")
  suppressWarnings(d$fetch_rt_readings("http://x/m/1", 3))
  q2 <- gsub("\\s+", " ", d$captured[length(d$captured)])
  expect_false(grepl("GROUP BY", q2, fixed = TRUE))                              # a short window is every reading
  suppressWarnings(d$fetch_rt_span(c("http://x/m/1", "a'); DROP TABLE t; --")))
  q3 <- d$captured[length(d$captured)]
  expect_match(q3, "MIN\\(date_time\\)"); expect_false(grepl(";", q3, fixed = TRUE))
})


test_that("the verification query holds the threshold, the outcome, the period, the nations and the lead, and nothing else", {
  d <- load_data_layer()
  suppressWarnings(d$fetch_verification_by_constituency(3L, "warnings", as.Date("2025-01-01"), as.Date("2025-12-31"), "E14", 2L))
  q <- gsub("\\s+", " ", d$captured[length(d$captured)])
  expect_match(q, "cat\\.sch\\.fgs_forecast_verification")
  expect_match(q, "forecast_rank >= 3 AND warnings > 0")
  expect_match(q, "forecast_rank >= 3 AND NOT \\(warnings > 0\\)")
  expect_match(q, "forecast_rank < 3 AND warnings > 0")
  expect_match(q, "BETWEEN '2025-01-01' AND '2025-12-31'")
  expect_match(q, "lead_day = 2"); expect_match(q, "GROUP BY constituency_id")
  expect_match(q, "substr\\(constituency_id, 1, 3\\) IN \\('E14'\\)")
  suppressWarnings(d$fetch_verification_by_lead(4L, "any", as.Date("2025-01-01"), as.Date("2025-12-31")))
  q2 <- gsub("\\s+", " ", d$captured[length(d$captured)])
  expect_match(q2, "\\(alerts \\+ warnings\\) > 0"); expect_match(q2, "GROUP BY lead_day")
  expect_false(grepl("lead_day =", q2, fixed = TRUE))
  expect_error(d$fetch_verification_by_lead(3L, "any'; DROP TABLE x; --", as.Date("2025-01-01"), as.Date("2025-12-31")))
  suppressWarnings(d$fetch_verification_by_lead(3L, "any", as.Date("2025-01-01"), as.Date("2025-12-31"), "E14'); DROP TABLE x; --"))
  q3 <- d$captured[length(d$captured)]
  expect_false(grepl(";", q3, fixed = TRUE)); expect_false(grepl("--", q3, fixed = TRUE))
})
