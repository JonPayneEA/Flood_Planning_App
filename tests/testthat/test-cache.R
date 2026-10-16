make_counter <- function() {
  n <- 0L
  list(fn = function(x) { n <<- n + 1L; data.table(x = x, n = n) }, calls = function() n)
}

test_that("a repeated call is served from the cache", {
  d <- load_data_layer()
  ctr <- make_counter()
  f <- d$with_cache(ctr$fn, "t", ttl = 60)
  d$cache_clear()
  a <- f(1); b <- f(1)
  expect_equal(ctr$calls(), 1L)
  expect_equal(a, b)
  f(2)
  expect_equal(ctr$calls(), 2L)          # different argument, different entry
})

test_that("cached data.tables cannot be corrupted by the caller", {
  d <- load_data_layer(); d$cache_clear()
  f <- d$with_cache(function(x) data.table(v = 1:3), "t", ttl = 60)
  first <- f(1)
  first[, v := v * 100L]                  # by-reference edit
  expect_equal(f(1)$v, 1:3)
})

test_that("entries expire after the ttl", {
  d <- load_data_layer(); d$cache_clear()
  ctr <- make_counter()
  f <- d$with_cache(ctr$fn, "t", ttl = 0.2)
  f(1); Sys.sleep(0.35); f(1)
  expect_equal(ctr$calls(), 2L)
})

test_that("ttl of zero disables caching", {
  d <- load_data_layer(); d$cache_clear()
  ctr <- make_counter()
  f <- d$with_cache(ctr$fn, "t", ttl = 0)
  f(1); f(1)
  expect_equal(ctr$calls(), 2L)
})

test_that("errors are not cached", {
  d <- load_data_layer(); d$cache_clear()
  n <- 0L
  f <- d$with_cache(function(x) { n <<- n + 1L; if (n == 1L) stop("boom"); "ok" }, "t", ttl = 60)
  expect_error(f(1), "boom")
  expect_equal(f(1), "ok")
})

test_that("cache_clear empties it, and the cache stays within its size cap", {
  d <- load_data_layer(); d$cache_clear()
  f <- d$with_cache(function(x) x, "t", ttl = 60)
  for (i in seq_len(d$CACHE_MAX_ENTRIES + 25L)) f(i)
  expect_lte(length(ls(d$.fgs_cache, all.names = TRUE)), d$CACHE_MAX_ENTRIES)
  d$cache_clear()
  expect_length(ls(d$.fgs_cache, all.names = TRUE), 0L)
})
