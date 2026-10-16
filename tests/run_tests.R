# Run from the project root:  Rscript tests/run_tests.R
# Needs: testthat, data.table, shiny, DT, htmltools, sf, geojsonsf, jsonlite.
# The view tests also need leaflet and skip themselves if it is missing.
# brickster and a Databricks connection are NOT needed: every query is stubbed.
stopifnot(requireNamespace("testthat", quietly = TRUE))
res <- testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = FALSE)
df <- as.data.frame(res)
if (sum(df$failed) + sum(df$error) > 0) quit(status = 1)
