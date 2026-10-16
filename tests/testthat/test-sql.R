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
    })
    expect_length(d$captured, 7L)
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
