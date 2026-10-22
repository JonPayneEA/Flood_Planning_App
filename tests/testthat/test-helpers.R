RANK_HEX_TEST <- c("#e9ecef", "#4f9d4f", "#f0d040", "#e08020", "#b02020")

test_that("rank_fill maps ranks to the band colours and tolerates junk", {
  expect_equal(rank_fill(c(0, 1, 2, 3, 4)), RANK_HEX_TEST)
  expect_equal(rank_fill(c(NA, -1, 9, "x")), rep(RANK_HEX_TEST[[1]], 4))
})

test_that("risk_layer_popup escapes free text and reads warnings in force", {
  shape <- data.frame(
    constituency_name = "<script>alert(1)</script>", fgs_worst_rank = 4L,
    fgs_worst_date = "2026-10-16", fgs_worst_sources = "river", fgs_escalated = TRUE,
    severe_warnings = 1L, warnings = 0L, alerts = 2L, mp_name = NA_character_,
    stringsAsFactors = FALSE)
  p <- risk_layer_popup(shape)
  expect_false(grepl("<script>", p, fixed = TRUE))
  expect_match(p, "&lt;script&gt;")
  expect_match(p, "Forecast: High")
  expect_match(p, "escalated")
  expect_match(p, "1 severe warning\\(s\\) 2 alert\\(s\\)")
  expect_match(p, "vacant or unmatched")
  shape$severe_warnings <- 0L; shape$alerts <- 0L
  expect_match(risk_layer_popup(shape), "In force: none")
})

health_table <- function(statuses, checked_at = "2026-10-15T13:00:00+00:00", overall = NULL) {
  checks <- data.table(check_name = paste0("c", seq_along(statuses)), status = statuses,
                       value = "", detail = "", checked_at = checked_at)
  if (is.null(overall)) overall <- if ("fail" %in% statuses) "fail" else if ("warn" %in% statuses) "warn" else "ok"
  rbind(checks, data.table(check_name = "overall", status = overall, value = "", detail = "", checked_at = checked_at))
}
now_utc <- as.POSIXct("2026-10-15 13:30:00", tz = "UTC")

test_that("stale_status: healthy, info-only and fresh gives no banner", {
  expect_equal(stale_status(health_table(c("ok", "ok")), now_utc)$level, "ok")
  expect_equal(stale_status(health_table(c("ok", "info")), now_utc)$level, "ok")
})

test_that("stale_status: warnings and failures name the checks", {
  w <- stale_status(health_table(c("ok", "warn")), now_utc)
  expect_equal(w$level, "warn"); expect_match(w$message, "c2")
  f <- stale_status(health_table(c("fail", "warn")), now_utc)
  expect_equal(f$level, "fail"); expect_match(f$message, "c1")
})

test_that("stale_status: a health table that stopped updating is stale, even if all ok", {
  old <- health_table(c("ok", "ok"), checked_at = "2026-10-15T06:00:00+00:00")
  s <- stale_status(old, now_utc, stale_hours = 3)
  expect_equal(s$level, "stale")
  expect_match(s$message, "may have stopped")
  expect_equal(stale_status(old, now_utc, stale_hours = 12)$level, "ok")
})

test_that("stale_status: stale outranks fail, and an unreadable table is unknown", {
  both <- health_table(c("fail"), checked_at = "2026-10-15T06:00:00+00:00")
  expect_equal(stale_status(both, now_utc)$level, "stale")
  expect_equal(stale_status(NULL, now_utc)$level, "unknown")
  expect_equal(stale_status(data.table(), now_utc)$level, "unknown")
})

test_that("stale_status copes with a data.frame, a missing overall row and odd timestamps", {
  h <- as.data.frame(health_table(c("ok", "warn")))
  expect_equal(stale_status(h, now_utc)$level, "warn")
  no_overall <- health_table(c("ok", "ok"))[check_name != "overall"]
  expect_equal(stale_status(no_overall, now_utc)$level, "ok")
  junk <- health_table(c("ok"), checked_at = "not a date")
  expect_equal(stale_status(junk, now_utc)$level, "ok")      # unparseable: no false alarm
})

test_that("parse_utc reads offsets, Z and fractional seconds, and returns NA for junk", {
  expect_equal(format(parse_utc("2026-10-15T13:02:00+00:00"), "%H:%M"), "13:02")
  expect_equal(format(parse_utc("2026-10-15T13:02:00Z"), "%H:%M"), "13:02")
  expect_equal(format(parse_utc("2026-10-15T13:02:00.123456"), "%H:%M"), "13:02")
  expect_true(is.na(parse_utc("junk")))
})

test_that("choose_selected_statement follows the newest only for a user already on it", {
  ch <- c("a" = "8", "b" = "7", "c" = "6")
  expect_equal(choose_selected_statement(ch, NULL, NULL), "8")             # fresh session
  expect_equal(choose_selected_statement(ch, "7", "7"), "8")               # was on newest: follows
  expect_equal(choose_selected_statement(ch, "6", "7"), "6")               # chose an older one: stays
  expect_equal(choose_selected_statement(ch, "7", NULL), "7")              # restored bookmark: kept
  expect_equal(choose_selected_statement(ch, "99", "7"), "8")              # gone from the list
  expect_equal(choose_selected_statement(ch, "6", "7", keep_selected = FALSE), "8")
})


# ---- statement times -------------------------------------------------------------

test_that("as_utc reads every form the warehouse can return, and never silently drops the time", {
  expect_equal(format(as_utc("2026-10-02T09:30:00"), "%H:%M"), "09:30")        # the bug: as.POSIXct() gives 00:00
  expect_equal(format(as.POSIXct("2026-10-02T09:30:00", tz = "UTC"), "%H:%M"), "00:00")   # documents why as_utc exists
  expect_equal(format(as_utc("2026-10-02 09:30:00"), "%H:%M"), "09:30")        # space instead of T
  expect_equal(format(as_utc("2026-10-02T09:30:00.123456+00:00"), "%H:%M"), "09:30")
  expect_equal(format(as_utc("2026-10-02T09:30:00Z"), "%H:%M"), "09:30")
  expect_equal(format(as_utc(as.POSIXct("2026-10-02 09:30:00", tz = "UTC")), "%H:%M"), "09:30")
  expect_equal(format(as_utc(as.Date("2026-10-02")), "%Y-%m-%d %H:%M"), "2026-10-02 00:00")
  expect_equal(format(as_utc("2026-10-02"), "%H:%M"), "00:00")                  # a bare date is midnight
  expect_true(is.na(as_utc("junk")))
  expect_true(is.na(as_utc(NA_character_)))
})

test_that("fmt_statement_time gives a calm fallback and never prints a time zone", {
  expect_equal(fmt_statement_time("2026-10-15T09:30:00"), "Thu 15 Oct 09:30")
  expect_equal(fmt_statement_time(c("2026-10-15T09:30:00", "junk")), c("Thu 15 Oct 09:30", "unknown time"))
  expect_equal(fmt_statement_time("2026-10-02T09:30:00", "%d %b %Y %H:%M"), "02 Oct 2026 09:30")
  expect_false(grepl("UTC|GMT|BST", fmt_statement_time("2026-10-15T09:30:00")))
})

# ---- the map layer guard ---------------------------------------------------------

test_that("with_layer_guard always clears the busy flag and only reports real errors", {
  flag <- function(v = TRUE) { env <- new.env(); env$v <- v; function(x) if (missing(x)) env$v else env$v <- x }
  told <- character()
  tell <- function(m) told <<- c(told, m)

  b <- flag(); with_layer_guard("x", b, { 1 + 1 }, notify = tell)
  expect_false(b())                                                          # normal end

  b <- flag(); with_layer_guard("polygons", b, stop("geometry parse failed"), notify = tell)
  expect_false(b())                                                          # error: no stuck spinner
  expect_match(told[1], "polygons layer could not be drawn: geometry parse failed")

  told <- character(); b <- flag()
  silent <- function() stop(structure(class = c("shiny.silent.error", "validation", "error", "condition"), list(message = "", call = NULL)))
  with_layer_guard("x", b, silent(), notify = tell)
  expect_false(b()); expect_length(told, 0L)                                 # req(): not a failure, no message

  b <- flag()
  f <- function() { with_layer_guard("x", b, { return("early") }, notify = tell); "late" }
  expect_equal(f(), "early"); expect_false(b())                              # early return() still clears it

  b <- flag(); expect_message(with_layer_guard("x", b, stop("boom"), notify = tell), "x layer could not be drawn")
})

# ---- look-back aggregation -------------------------------------------------------

test_that("normalise_name matches the way names actually differ", {
  expect_equal(normalise_name("Newcastle upon Tyne Central & West"), normalise_name("newcastle-upon-tyne central and west"))
  expect_equal(normalise_name("  St. Albans "), "stalbans")
})

agg_fixture <- function() {
  inter <- data.table(
    constituency_id = c("a", "a", "a", "b", "b", "c"), constituency_name = c("Alpha", "Alpha", "Alpha", "Beta", "Beta", "Gamma"),
    forecast_date = c("2026-10-15", "2026-10-15", "2026-10-16", "2026-10-15", "2026-10-17", "2026-10-15"),
    day_index = c(1L, 1L, 2L, 1L, 3L, 1L), source = c("river", "surface", "river", "river", "river", "river"),
    risk_level = c("Low", "Medium", "High", "Very Low", "Low", "High"),
    intersection_pct = c(1, 0.5, 0.2, 1, 1, 0.004))             # Gamma is below the 1% floor
  list(inter = inter,
       all_const = data.table(constituency_id = c("a", "b", "c", "d"), constituency_name = c("Alpha", "Beta", "Gamma", "Delta")),
       mp = data.table(constituency_name = c("alpha", "BETA!"), mp_name = c("A MP", "B MP"), mp_party = "P", mp_email = "e", mp_phone = "t"),
       ch = data.table(constituency_id = c("a", "b"), change_type = c("escalated", "cleared")))
}

test_that("aggregate_situation ranks, dates, sources and escalation", {
  x <- agg_fixture()
  s <- aggregate_situation(x$inter, x$ch, x$mp, x$all_const, 4907L, "2026-10-15T09:30:00", min_pct = 0.01)
  expect_equal(nrow(s), 4L)                                                  # quiet constituencies still get a row
  a <- s[constituency_id == "a"]
  expect_equal(a$fgs_worst_rank, 4L); expect_equal(a$fgs_worst_level, "High")
  expect_equal(a$fgs_worst_date, "2026-10-16"); expect_equal(a$fgs_worst_sources, "river")
  expect_equal(c(a$fgs_rank_day1, a$fgs_rank_day2, a$fgs_rank_day3), c(3L, 4L, 0L))   # worst across sources on day 1
  expect_true(a$fgs_escalated); expect_true(s[constituency_id == "b", fgs_changed]); expect_false(s[constituency_id == "b", fgs_escalated])
  expect_equal(s[constituency_id == "c", fgs_worst_rank], 0L)                # below the coverage floor: ignored
  expect_equal(s[constituency_id == "d", fgs_worst_level], "None")
  expect_equal(s[constituency_id == "c", fgs_worst_rank], 0L)
  expect_equal(s$fgs_statement_id[1], 4907L); expect_equal(s$fgs_issued_at[1], "2026-10-15T09:30:00")
})

test_that("aggregate_situation joins MPs on a normalised name and leaves warnings unknown", {
  x <- agg_fixture()
  s <- aggregate_situation(x$inter, x$ch, x$mp, x$all_const, 1L, "2026-10-15T09:30:00")
  expect_equal(s[constituency_id == "a", mp_name], "A MP")
  expect_equal(s[constituency_id == "b", mp_name], "B MP")                   # "BETA!" matches "Beta"
  expect_true(is.na(s[constituency_id == "c", mp_name])); expect_false(s[constituency_id == "c", mp_matched])
  expect_true(all(is.na(s$severe_warnings) & is.na(s$warnings) & is.na(s$alerts) & is.na(s$has_active_warning)))
})

test_that("aggregate_situation copes with nothing at all", {
  x <- agg_fixture()
  s <- aggregate_situation(x$inter[0], NULL, x$mp[0], x$all_const, 1L, "2026-10-15T09:30:00")
  expect_equal(nrow(s), 4L); expect_true(all(s$fgs_worst_rank == 0L)); expect_false(any(s$fgs_escalated))
  expect_true(all(is.na(s$mp_name)))
})

test_that("an empty situation table explains itself", {
  ranks <- c(0L, 0L, 1L, 2L, 2L, 0L)
  expect_match(empty_situation_message(ranks, 3L), "Nothing in this statement is forecast at Medium or above. 3 constituencies are forecast at a lower risk")
  expect_match(empty_situation_message(c(0L, 1L), 3L), "1 constituency is forecast at a lower risk: lower .* to see it")
  expect_match(empty_situation_message(rep(0L, 5), 3L), "no constituency is flagged at any risk")
  expect_equal(empty_situation_message(ranks, 3L, other_filters = TRUE), "No constituency matches these filters.")
  expect_equal(empty_situation_message(ranks, 0L), "No constituency matches these filters.")
})


# ---- warehouse errors the app can explain ---------------------------------------

# The message as Databricks returned it (from the console of a real session).
real_missing <- paste("[TABLE_OR_VIEW_NOT_FOUND] The table or view `prd_dash_lab`.`flood_forecasting_unrestricted`.`fgs_constituency_changes`",
                      "cannot be found. Verify the spelling and correctness of the schema and catalog. If you did not qualify the name with a schema,",
                      "verify the current_schema() output, or qualify the name with the correct schema and catalog. To tolerate the error on drop use DROP",
                      "VIEW IF EXISTS or DROP TABLE IF EXISTS. SQLSTATE: 42P01; line 12 pos 9")

test_that("missing_table_info names the table and the notebook that builds it", {
  i <- missing_table_info(real_missing)
  expect_equal(i$table, "fgs_constituency_changes"); expect_equal(i$job, 18L)
  expect_equal(missing_table_info(sub("fgs_constituency_changes", "mp_briefing_drafts", real_missing))$job, 20L)
  u <- missing_table_info(sub("fgs_constituency_changes", "something_else", real_missing))
  expect_equal(u$table, "something_else"); expect_true(is.na(u$job))
  expect_null(missing_table_info("Failed to connect to warehouse abc: Failed to perform HTTP request."))
  expect_null(missing_table_info("division by zero"))
})

test_that("is_connection_error recognises an unreachable warehouse, and nothing else", {
  expect_true(is_connection_error("Failed to connect to warehouse 0a86ae40313bb7db: Failed to perform HTTP request."))
  expect_true(is_connection_error("Could not resolve host: adb-123.azuredatabricks.net"))
  expect_false(is_connection_error(real_missing))
  expect_false(is_connection_error("syntax error at line 3"))
})

test_that("gaps_message lists every missing table with its notebook", {
  expect_equal(gaps_message(list()), "")
  m <- gaps_message(list(fgs_constituency_changes = 18L, mp_briefing_drafts = 20L), "prd.sch")
  expect_match(m, "not in prd.sch yet"); expect_match(m, "fgs_constituency_changes \\(pipeline notebook 18\\)")
  expect_match(m, "mp_briefing_drafts \\(pipeline notebook 20\\)"); expect_match(m, "Refresh statements")
  expect_match(gaps_message(list(odd_table = NA_integer_)), "odd_table")
})

test_that("a look-back without the changes table says escalation is unknown, not none", {
  x <- agg_fixture()
  s <- aggregate_situation(x$inter, NULL, x$mp, x$all_const, 1L, "2026-10-02T09:30:00", changes_available = FALSE)
  expect_true(all(is.na(s$fgs_escalated))); expect_true(all(is.na(s$fgs_changed)))
  expect_equal(s[constituency_id == "a", fgs_worst_rank], 4L)                   # the rest is unaffected
  s2 <- aggregate_situation(x$inter, NULL, x$mp, x$all_const, 1L, "2026-10-02T09:30:00", changes_available = TRUE)
  expect_false(any(s2$fgs_escalated))                                            # table exists, nothing changed: FALSE
})


# ---- retries --------------------------------------------------------------------------

net_err  <- "Failed to connect to warehouse 0a86ae40313bb7db: Failed to perform HTTP request."
sql_err  <- "[TABLE_OR_VIEW_NOT_FOUND] The table or view `c`.`s`.`t` cannot be found. SQLSTATE: 42P01; line 12 pos 9"

test_that("with_retries returns at once on success and never sleeps", {
  slept <- numeric()
  expect_equal(with_retries(function() 42, sleep = function(s) slept <<- c(slept, s)), 42)
  expect_length(slept, 0L)
  expect_null(with_retries(function() NULL))                       # a NULL result is a result
})

test_that("a network error is retried, with growing pauses, until it works", {
  n <- 0L; slept <- numeric(); seen <- list()
  r <- with_retries(function() { n <<- n + 1L; if (n < 3L) stop(net_err); "ok" }, retries = 2L,
                    sleep = function(s) slept <<- c(slept, s),
                    on_failure = function(attempt, total, msg, will_retry, pause) seen[[length(seen) + 1L]] <<- c(attempt, total, will_retry, pause))
  expect_equal(r, "ok"); expect_equal(n, 3L)
  expect_equal(slept, c(1.5, 3))
  expect_equal(seen[[1]], c(1, 3, 1, 1.5)); expect_equal(seen[[2]], c(2, 3, 1, 3))
})

test_that("once retries run out, the original error is raised with its message", {
  n <- 0L; slept <- numeric()
  e <- tryCatch(with_retries(function() { n <<- n + 1L; stop(net_err) }, retries = 2L, sleep = function(s) slept <<- c(slept, s)),
                error = function(e) e)
  expect_equal(n, 3L); expect_equal(slept, c(1.5, 3))
  expect_equal(conditionMessage(e), net_err)
})

test_that("a SQL error, such as a missing table, is never retried", {
  n <- 0L; slept <- numeric()
  expect_error(with_retries(function() { n <<- n + 1L; stop(sql_err) }, sleep = function(s) slept <<- c(slept, s)), "TABLE_OR_VIEW_NOT_FOUND")
  expect_equal(n, 1L); expect_length(slept, 0L)
})

test_that("retries = 0 switches retrying off, and the last pause is reused for long runs", {
  n <- 0L
  expect_error(with_retries(function() { n <<- n + 1L; stop(net_err) }, retries = 0L, sleep = function(s) NULL), "Failed to connect")
  expect_equal(n, 1L)
  slept <- numeric()
  try(with_retries(function() stop(net_err), retries = 4L, sleep = function(s) slept <<- c(slept, s)), silent = TRUE)
  expect_equal(slept, c(1.5, 3, 3, 3))
})

# ---- the outage log ---------------------------------------------------------------------

t0 <- as.POSIXct("2026-10-16 08:13:34", tz = "UTC")

test_that("the outage log speaks at the start, sparingly during, and once at the end", {
  s <- outage_new()
  r <- outage_failure(s, t0, net_err); s <- r$state
  expect_match(r$log, "Databricks warehouse unreachable: Failed to connect to warehouse")
  r <- outage_failure(s, t0 + 5, net_err); s <- r$state;  expect_null(r$log)          # too soon
  r <- outage_failure(s, t0 + 30, net_err); s <- r$state; expect_null(r$log)
  r <- outage_failure(s, t0 + 65, net_err); s <- r$state
  expect_match(r$log, "still unreachable since 08:13:34 \\(4 failed requests so far\\)")
  r <- outage_failure(s, t0 + 90, net_err); s <- r$state; expect_null(r$log)           # a minute since the reminder
  r <- outage_failure(s, t0 + 130, net_err); s <- r$state; expect_match(r$log, "still unreachable")
  r <- outage_success(s, t0 + 200); s <- r$state
  expect_match(r$log, "reachable again after 3 min 20 s \\(6 failed requests\\)")
  expect_null(outage_success(s, t0 + 300)$log)                                           # nothing to report once recovered
})

test_that("a single blip is reported as one failed request, in the singular", {
  r <- outage_failure(outage_new(), t0, net_err)
  expect_match(outage_success(r$state, t0 + 2)$log, "after 2 s \\(1 failed request\\)$")
})

test_that("durations read plainly", {
  expect_equal(fmt_duration(7), "7 s"); expect_equal(fmt_duration(125), "2 min 5 s"); expect_equal(fmt_duration(7260), "2 h 1 min")
})

test_that("a long error message is shortened in the log, not dumped", {
  r <- outage_failure(outage_new(), t0, paste(rep("word", 200), collapse = " "))
  expect_lte(nchar(r$log), 240)
})

test_that("look-back choices group a busy day and leave a quiet day flat", {
  older <- data.table(statement_id = c(5L, 4L, 3L),
                      issued_at = c("2026-10-02T09:30:00", "2026-10-02T15:00:00", "2026-10-01T09:30:00"))
  ch <- group_statement_choices(older)
  expect_equal(length(ch), 2L)
  expect_match(names(ch)[1], "02 Oct \\(2 statements\\)")
  expect_equal(unname(unlist(ch[[1]])), c("5", "4"))
  expect_equal(unname(ch[[2]]), "3")
  expect_equal(length(group_statement_choices(older[0])), 0L)
})

test_that("no_change_message names the statement it was compared with", {
  recent <- data.table(statement_id = c(4922L, 4921L), issued_at = c("2026-10-16T09:30:00", "2026-10-15T09:30:00"))
  expect_match(no_change_message(recent, 4922L), "No change since statement #4921 \\(issued Thu 15 Oct 09:30\\)")
  expect_match(no_change_message(recent, 4921L), "No earlier statement")
  expect_match(no_change_message(recent[0], 1L), "No earlier statement")
})

test_that("no_change_message reads job 18's log when it has one", {
  recent <- data.table(statement_id = c(4922L, 4921L), issued_at = c("2026-10-16T09:30:00", "2026-10-15T09:30:00"))
  lg <- data.table(statement_id = 4922L, previous_statement_id = 4921L, previous_issued_at = "2026-10-15T09:30:00",
                   days_compared_from = NA_character_, days_compared_to = NA_character_, n_changes = 0L, status = "compared")
  expect_match(no_change_message(recent, 4922L, lg), "No change since statement #4921 \\(issued Thu 15 Oct 09:30\\)")
  expect_match(no_change_message(recent, 4922L, lg[0]), "Not yet compared")
  expect_match(no_change_message(recent, 4921L, lg[0]), "No earlier statement")     # nothing to compare, whatever the log says
})

test_that("headline counts say 30 to none, and why no day-by-day change is listed", {
  lg <- data.table(statement_id = 4925L, previous_statement_id = 4924L, n_changes = 0L, status = "compared",
                   flagged_previous = 30L, flagged_new = 0L, medium_plus_previous = 0L, medium_plus_new = 0L)
  m <- headline_counts_message(lg, 0L)
  expect_match(m, "30 in statement #4924, none in this one")
  expect_match(m, "At Medium or above: none and none")
  expect_match(m, "days this statement no longer covers")
  expect_false(grepl("no longer covers", headline_counts_message(lg, 5L)))     # changes are listed, no explanation needed
  same <- copy(lg)[, `:=`(flagged_new = 30L)]
  expect_false(grepl("no longer covers", headline_counts_message(same, 0L)))   # counts agree, nothing to explain
})

test_that("headline counts are left out when the log or its columns are not there", {
  lg <- data.table(statement_id = 1L, previous_statement_id = 0L, n_changes = 0L, status = "compared")
  expect_null(headline_counts_message(lg, 0L))                      # an older log, without the counts
  expect_null(headline_counts_message(NULL, 0L))
  expect_null(headline_counts_message(lg[0], 0L))
  lg2 <- cbind(lg, data.table(flagged_previous = NA_integer_, flagged_new = 2L, medium_plus_previous = 0L, medium_plus_new = 0L))
  expect_null(headline_counts_message(lg2, 0L))                     # a missing count, not a made-up zero
})


test_that("polygons_only keeps the polygon parts of a collection and drops the rest", {
  poly  <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
  line  <- sf::st_linestring(rbind(c(5, 5), c(6, 6)))
  coll  <- sf::st_geometrycollection(list(poly, line))
  only_line <- sf::st_geometrycollection(list(line))
  shape <- sf::st_sf(code = c("a", "b", "c", "d"),
                     geometry = sf::st_sfc(poly, coll, only_line, line, crs = 4326))
  out <- polygons_only(shape)
  expect_equal(out$code, c("a", "b"))
  expect_true(all(as.character(sf::st_geometry_type(out)) %in% c("POLYGON", "MULTIPOLYGON")))
  # leaflet can now draw it, which is what the collection stopped
  expect_no_error(leaflet::addPolygons(leaflet::leaflet(), data = out))
})

test_that("polygons_only passes empty and polygon-only input through", {
  expect_equal(nrow(polygons_only(sf::st_sf(code = character(), geometry = sf::st_sfc(), crs = 4326))), 0L)
  poly <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 0))))
  one  <- sf::st_sf(code = "a", geometry = sf::st_sfc(poly, crs = 4326))
  expect_equal(polygons_only(one), one)
})


test_that("line_rows picks out line geometries and nothing else", {
  poly <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 0))))
  line <- sf::st_linestring(rbind(c(5, 5), c(6, 6)))
  multi <- sf::st_multilinestring(list(rbind(c(0, 0), c(1, 1)), rbind(c(2, 2), c(3, 3))))
  shape <- sf::st_sf(code = c("a", "b", "c"), geometry = sf::st_sfc(poly, line, multi, crs = 4326))
  expect_equal(line_rows(shape), c(FALSE, TRUE, TRUE))
  expect_equal(line_rows(shape[1, ]), FALSE)
  expect_equal(line_rows(sf::st_sf(code = character(), geometry = sf::st_sfc(), crs = 4326)), logical())
  expect_equal(line_rows(NULL), logical())
})


test_that("a missing intersection share (a coastal line) passes the threshold", {
  expect_equal(clears_threshold(c(0.5, 0.1, NA), 0.25), c(TRUE, FALSE, TRUE))
  expect_equal(clears_threshold(numeric(), 0.25), logical())
})

test_that("a missing share is never an NA row in an sf subset, which is what broke the layer", {
  poly <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 0))))
  shape <- sf::st_sf(code = c("a", "b", "c"), pct = c(0.5, NA, 0.1), geometry = sf::st_sfc(poly, poly, poly, crs = 4326))
  kept <- shape[clears_threshold(shape$pct, 0.25), ]
  expect_equal(kept$code, c("a", "b"))
  expect_false(anyNA(kept$code))
})

test_that("intersection_label says so when the share is missing", {
  expect_equal(intersection_label(c(0.5, NA, 1)), c("50%", "crossed by a coastal line", "100%"))
})


test_that("polygons_only keeps a collection's polygon when an empty collection is in the same batch", {
  poly  <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 0))))
  line  <- sf::st_linestring(rbind(c(5, 5), c(6, 6)))
  shape <- sf::st_sf(code = c("a", "b", "empty"),
                     geometry = sf::st_sfc(poly, sf::st_geometrycollection(list(poly, line)),
                                           sf::st_geometrycollection(), crs = 4326))
  out <- polygons_only(shape)
  expect_equal(out$code, c("a", "b"))
})


detail_inter <- function() {
  data.table::data.table(
    constituency_id = c("A", "A", "A", "A", "B", "B", "C"),
    forecast_date = c("2026-10-20", "2026-10-21", "2026-10-21", "2026-10-21", "2026-10-20", "2026-10-22", "2026-10-22"),
    day_index = c(1, 2, 2, 2, 1, 3, 3),
    source = c("river", "river", "coastal", "coastal", "surface", "surface", "ground"),
    risk_level = c("Low", "Medium", "Medium", "Low", "Very Low", "Low", "High"),
    risk_x = c(1, 2, 3, 1, 1, 2, 4), risk_y = c(1, 2, 1, 1, 1, 1, 2),
    impact_label = c("Minor", "Moderate", "Significant", "Minor", "Minor", "Moderate", "Extreme"),
    likelihood_label = c("Low", "Medium", "Low", "Low", "Low", "Low", "Medium"),
    intersection_pct = c(1, 1, 1, 1, 1, 1, 0.001))
}

test_that("situation_detail folds ranks, worst day, sources, impact and likelihood", {
  d <- situation_detail(detail_inter())[order(constituency_id)]
  expect_equal(d$constituency_id, c("A", "B"))                    # C is below the overlap floor
  expect_equal(d$fgs_worst_rank, c(3L, 2L))
  expect_equal(d$fgs_worst_date, c("2026-10-21", "2026-10-22"))
  expect_equal(d$fgs_worst_sources, c("coastal, river", "surface"))
  expect_equal(c(d$fgs_rank_day1[1], d$fgs_rank_day2[1], d$fgs_rank_day3[1]), c(2L, 3L, 0L))
})

test_that("two polygons at the worst risk on the worst day show the higher impact", {
  d <- situation_detail(detail_inter())[constituency_id == "A"]
  expect_equal(d$fgs_worst_impact, "Significant")                 # over Moderate, both Medium
  expect_equal(d$fgs_worst_likelihood, "Low")
  expect_equal(d$fgs_worst_impact_x, 3L)
})

test_that("a source filter recomputes the worst risk for those sources only", {
  d <- situation_detail(detail_inter(), "river")
  expect_equal(d$constituency_id, "A")                            # B has no river risk
  expect_equal(d$fgs_worst_rank, 3L)
  expect_equal(d$fgs_worst_impact, "Moderate")
  coastal <- situation_detail(detail_inter(), c("coastal"))
  expect_equal(coastal$fgs_worst_sources, "coastal")
  expect_equal(nrow(situation_detail(detail_inter(), "ground")), 0L)   # C is below the floor
  expect_equal(nrow(situation_detail(NULL)), 0L)
})

test_that("apply_situation_detail adds the columns, and narrows the risk when a filter is on", {
  base <- data.table::data.table(
    constituency_id = c("A", "B", "Z"), fgs_worst_rank = c(3L, 2L, 0L), fgs_worst_level = c("Medium", "Low", "None"),
    fgs_worst_date = c("x", "y", NA), fgs_worst_sources = c("s", "s", NA),
    fgs_rank_day1 = c(2L, 1L, 0L), fgs_rank_day2 = c(3L, 0L, 0L), fgs_rank_day3 = 0L, fgs_rank_day4 = 0L, fgs_rank_day5 = 0L)
  plain <- apply_situation_detail(base, situation_detail(detail_inter()), narrow = FALSE)
  expect_equal(plain$fgs_worst_impact, c("Significant", "Moderate", NA))
  expect_equal(plain$fgs_worst_rank, base$fgs_worst_rank)         # ranks untouched without a filter
  narrowed <- apply_situation_detail(base, situation_detail(detail_inter(), "river"), narrow = TRUE)
  expect_equal(narrowed$fgs_worst_rank, c(3L, 0L, 0L))            # B drops out with no river risk
  expect_equal(narrowed$fgs_worst_level, c("Medium", "None", "None"))
  expect_equal(narrowed$fgs_worst_sources, c("river", NA, NA))
  expect_equal(narrowed$fgs_rank_day1, c(2L, 0L, 0L))
  none <- apply_situation_detail(base, NULL, narrow = TRUE)       # intersections unreadable
  expect_true(all(is.na(none$fgs_worst_impact)))
  expect_equal(none$fgs_worst_rank, base$fgs_worst_rank)
})


test_that("situation_detail copes with intersections that carry no impact columns", {
  slim <- detail_inter()[, .(constituency_id, forecast_date, day_index, source, risk_level, intersection_pct)]
  d <- situation_detail(slim)
  expect_equal(d[constituency_id == "A"]$fgs_worst_rank, 3L)
  expect_true(all(is.na(d$fgs_worst_impact)))
})


test_that("notification_id gives a repeat the same id and a different failure its own", {
  a <- "The EA flood areas layer could not be drawn: Don't know how to get polygon data (1)"
  b <- "The EA flood areas layer could not be drawn: Don't know how to get polygon data (2)"
  c <- "The FGS risk polygons layer could not be drawn: something else"
  expect_equal(notification_id(a, "layer_"), notification_id(a, "layer_"))
  expect_equal(notification_id(a, "layer_"), notification_id(b, "layer_"))   # same layer, same words: one box
  expect_false(identical(notification_id(a, "layer_"), notification_id(c, "layer_")))
  expect_match(notification_id(a), "^note_[A-Za-z]+$")
})


test_that("in_nations keeps the chosen nations, keeps unknown ids, and no choice means all", {
  ids <- c("E14001549", "W07000041", "S14000001", "N06000001", "DEMO-C001", NA)
  expect_equal(in_nations(ids, "E14"),               c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE))
  expect_equal(in_nations(ids, c("E14", "W07")),     c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE))
  expect_equal(in_nations(ids, c("S14", "N06")),     c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE))
  expect_equal(in_nations(ids, NULL),                rep(TRUE, 6))
  expect_equal(in_nations(ids, character(0)),        rep(TRUE, 6))
  expect_equal(in_nations(character(0), "E14"),      logical(0))
})


# ---- Hydrometrics helpers ---------------------------------------------------------

rt_fake_register <- function() data.table(
  station_label = c("Aldwyn", "Aldwyn", "Rainfall station", "Rainfall station", NA),
  station_reference = c("S1", "S1", "R1", "R2", "S9"),
  station_uri = c("u/S1", "u/S1", "u/R1", "u/R2", "u/S9"),
  measure_uri = c("m/1", "m/2", "m/3", "m/4", "m/5"),
  parameter = c("level", "flow", "rainfall", "rainfall", "level"),
  qualifier = c("Stage", "Flow", "Gauge", "Gauge", NA),
  unit_name = c("mASD", "m3/s", "mm", "mm", NA),
  river_name = c("Aldwyn", "Aldwyn", NA, NA, "Brenn"),
  ea_area_name = c("North", "North", "North", "South", "South"),
  town = c(NA, NA, "Hexton", "Ashby", NA))

test_that("the register gets readable, unique station and measure names", {
  r <- rt_prepare_register(rt_fake_register())
  expect_equal(r$measure_name[1:2], c("level, Stage (mASD)", "flow, Flow (m3/s)"))
  expect_equal(r$station[3:4], c("Rainfall station (R1)", "Rainfall station (R2)"))   # same name, told apart
  expect_equal(r$station[5], "S9")                                                     # no label: the reference
  expect_equal(r$measure_name[5], "level")
  expect_equal(anyDuplicated(r$measure_label), 0L)
})

test_that("the drop-downs narrow each other and never offer an empty choice", {
  r <- rt_prepare_register(rt_fake_register())
  o <- rt_options(r)
  expect_equal(o$areas, c("All", "North", "South"))
  expect_equal(rt_options(r, river = "Brenn")$areas, c("All", "South"))
  expect_equal(rt_options(r, area = "North", param = "rainfall")$stations, c("All", "Rainfall station (R1)"))
  expect_equal(nrow(rt_filter_register(r, area = "South", param = "level")), 1L)
})

test_that("the archive boundary is midnight at the end of the last archived day", {
  expect_equal(format(rt_boundary(as.Date("2026-10-20")), "%Y-%m-%d %H:%M", tz = "UTC"), "2026-10-21 00:00")
  expect_null(rt_boundary(NULL)); expect_null(rt_boundary(NA))
})

test_that("the chart summary counts archived and live readings, and the shapes mark the live tail", {
  b <- as.POSIXct("2026-10-21 00:00:00", tz = "UTC"); now <- as.POSIXct("2026-10-21 12:00:00", tz = "UTC")
  rd <- data.table(measure_uri = "m/1", date_time = as.POSIXct(c("2026-10-20 23:45", "2026-10-21 00:00", "2026-10-21 11:45"), tz = "UTC"), value = 1:3)
  s <- rt_summary(rd, b, now)
  expect_equal(c(s$readings, s$archive, s$live, s$age_minutes), c(3, 1, 2, 15))
  expect_equal(rt_summary(rd, NULL, now)$live, 0L)
  expect_equal(rt_summary(rd[0], b, now)$readings, 0L)
  start <- now - 86400
  sh <- rt_live_shapes(b, start, now)
  expect_equal(vapply(sh, `[[`, "", "type"), c("rect", "line"))
  expect_equal(length(rt_live_shapes(b, now - 3600, now)), 1L)                         # window starts after the boundary: band only
  expect_equal(length(rt_live_shapes(NULL, start, now)), 0L)
  pan <- rt_panels(rd, rt_prepare_register(rt_fake_register()))
  expect_equal(pan$units, "mASD")
})


# ---- Warning episodes: the Detailed table and the concurrency graph --------------

ep_fake <- function() data.table(
  episode_id = c("e1", "e2", "e3", "e4"),
  ea_area_code = c("A", "B", "C", "D"),
  ea_area_name = c("Alder", "Beech", "Coast", "Dale"),
  ea_area_type = c("flood_warning", "flood_warning", "flood_alert", "flood_alert"),
  start_time = as.POSIXct(c("2025-01-01 10:00", "2025-01-01 12:00", "2025-01-01 11:00", "2024-01-01 09:00"), tz = "UTC"),
  end_time = as.POSIXct(c("2025-01-01 14:00", NA, "2025-01-01 13:00", NA), tz = "UTC"),
  duration_minutes = c(240, NA, 120, NA),
  peak_level = c("severe_flood_warning", "flood_warning", "flood_alert", "flood_alert"),
  had_severe = c(TRUE, FALSE, FALSE, FALSE),
  stale_open = c(FALSE, FALSE, FALSE, TRUE))

test_that("durations and levels read as words", {
  expect_equal(duration_label(c(45, 195, 1500, NA)), c("45 min", "3 h 15 min", "1 d 1 h", ""))
  expect_equal(level_label(c("flood_alert", "severe_flood_warning", "odd")), c("Flood alert", "Severe flood warning", "odd"))
})

test_that("the Detailed table lists every episode with its dates, newest first, inside the period", {
  t <- warning_episode_rows(ep_fake(), "2025-01-01", "2025-01-31")
  expect_equal(t$Code, c("B", "C", "A"))                                      # the 2024 episode is outside the period
  expect_equal(t$Issued[3L], "01 Jan 2025 10:00")
  expect_equal(t$Ended[3L], "01 Jan 2025 14:00"); expect_equal(t$Duration[3L], "4 h 0 min")
  expect_equal(t$Ended[1L], "In force")
  expect_equal(t$`Highest level`[3L], "Severe flood warning"); expect_equal(t$Severe, c("", "", "Yes"))
  all <- warning_episode_rows(ep_fake(), "2000-01-01", "2030-01-01")
  expect_equal(all$Ended[all$Code == "D"], "Still open (over 30 days)")
  expect_equal(nrow(warning_episode_rows(NULL, "2025-01-01", "2025-01-31")), 0L)
})

test_that("concurrent counts rise and fall as alerts and warnings start and end", {
  now <- as.POSIXct("2025-01-02 00:00", tz = "UTC")
  cc <- concurrent_counts(ep_fake(), now)
  at <- function(t) cc[time <= as.POSIXct(t, tz = "UTC")][.N]
  expect_equal(c(at("2025-01-01 10:30")$alerts, at("2025-01-01 10:30")$warnings), c(0L, 1L))
  expect_equal(c(at("2025-01-01 12:30")$alerts, at("2025-01-01 12:30")$warnings), c(1L, 2L))   # all three on at once
  expect_equal(c(at("2025-01-01 13:30")$alerts, at("2025-01-01 13:30")$warnings), c(0L, 2L))   # the alert ended; the open warning stays
  expect_equal(at("2025-01-01 14:30")$warnings, 1L)
  expect_equal(max(cc$alerts), 1L)                                                             # the stale open alert is left out
  expect_equal(nrow(concurrent_counts(NULL, now)), 0L)
})


test_that("long windows are bucketed to about 1,500 points, and short ones are not", {
  expect_equal(rt_bucket_seconds(1), 900L); expect_equal(rt_bucket_seconds(3), 900L); expect_equal(rt_bucket_seconds(15), 900L)
  expect_equal(rt_bucket_seconds(30) %% 900L, 0L)                                 # always whole readings
  expect_equal(rt_bucket_seconds(30), 1800L)
  for (d in c(30, 365, 1826, 4000)) expect_lte(d * 86400 / rt_bucket_seconds(d), 1500)
  expect_gt(rt_bucket_seconds(1826), rt_bucket_seconds(365))
  expect_equal(rt_bucket_label(900L), "")
  expect_equal(rt_bucket_label(1800L), "30 minute")
  expect_equal(rt_bucket_label(7200L), "2 hour")
  expect_equal(rt_bucket_label(86400L), "1 day")
  expect_equal(unname(RT_WINDOWS[["All available"]]), 0L)
})


# ---- Forecast against warnings -----------------------------------------------------

ver_fake <- function() data.table(
  constituency_id = c("E14000001", "E14000002", "E14000003", "E14000004", "W07000001", "E14000001"),
  forecast_date = as.Date(c("2026-10-10", "2026-10-10", "2026-10-10", "2026-10-10", "2026-10-10", "2026-09-01")),
  lead_day = c(1L, 1L, 1L, 1L, 1L, 2L),
  forecast_rank = c(3L, 4L, 0L, 2L, 3L, 3L),
  alerts   = c(0L, 0L, 1L, 0L, 0L, 0L),
  warnings = c(1L, 0L, 0L, 1L, 1L, 0L),
  severe   = c(0L, 0L, 0L, 0L, 0L, 0L))

test_that("hits, false alarms and misses are counted for the chosen threshold and outcome", {
  v <- ver_fake()
  c1 <- verification_counts(v, 3, "any", "2026-10-01", "2026-10-31", by = "lead_day")
  expect_equal(c1$lead_day, 1L)
  expect_equal(c(c1$hits, c1$false_alarms, c1$misses), c(2L, 1L, 2L))      # c1 and W hit, c2 false alarm, c3 and c4 miss
  c2 <- verification_counts(v, 2, "any", "2026-10-01", "2026-10-31", by = "lead_day")
  expect_equal(c(c2$hits, c2$misses), c(3L, 1L))                            # a lower threshold turns c4's miss into a hit
  w <- verification_counts(v, 3, "warnings", "2026-10-01", "2026-10-31", by = "lead_day")
  expect_equal(c(w$hits, w$false_alarms, w$misses), c(2L, 1L, 1L))         # the alert-only day no longer happened
  a <- verification_counts(v, 3, "alerts", "2026-10-01", "2026-10-31", by = "lead_day")
  expect_equal(c(a$hits, a$false_alarms, a$misses), c(0L, 3L, 1L))
})

test_that("the period, the nations and the lead day narrow the counts", {
  v <- ver_fake()
  expect_equal(verification_counts(v, 3, "any", "2026-09-01", "2026-09-30", by = "lead_day")$lead_day, 2L)
  e <- verification_counts(v, 3, "any", "2026-10-01", "2026-10-31", nations = "E14", by = "lead_day")
  expect_equal(c(e$hits, e$false_alarms, e$misses), c(1L, 1L, 2L))        # the Welsh hit is out
  b <- verification_counts(v, 3, "any", "2026-10-01", "2026-10-31", by = "constituency_id", lead = 1)
  expect_equal(b$constituency_id[b$hits == 1L], c("E14000001", "W07000001"))
  expect_equal(nrow(verification_counts(v, 3, "any", "2020-01-01", "2020-12-31", by = "lead_day")), 0L)
  expect_error(verification_counts(v, 3, "everything", "2026-10-01", "2026-10-31"))     # only the four outcomes
})

test_that("hit rate and precision divide by what could have been right, and are blank where nothing does", {
  s <- verification_scores(data.table(lead_day = 1:2, hits = c(2L, 0L), false_alarms = c(1L, 0L), misses = c(2L, 0L)))
  expect_equal(c(s$forecast_days[1], s$event_days[1]), c(3L, 4L))
  expect_equal(c(s$hit_rate[1], s$precision[1]), c(0.5, 2 / 3))
  expect_true(is.na(s$hit_rate[2]) && is.na(s$precision[2]))
})
