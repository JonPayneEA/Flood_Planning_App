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



# ---- Warnings against the FGS ------------------------------------------------------

cov_fake <- function() data.table(
  episode_id = paste0("e", 1:7),
  area_type = c("flood_warning", "flood_warning", "flood_alert", "flood_warning", "flood_alert", "flood_warning", "flood_warning"),
  had_severe = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
  issue_date = as.Date(c("2025-03-10", "2025-03-10", "2025-03-10", "2025-03-10", "2025-03-10", "2024-01-01", "2025-03-10")),
  first_cover_ge1 = as.Date(c("2025-03-07", "2025-03-10", NA, NA, "2025-03-09", "2024-01-01", NA)),
  first_cover_ge2 = as.Date(c("2025-03-08", "2025-03-10", NA, NA, NA, "2024-01-01", NA)),
  first_cover_ge3 = as.Date(c("2025-03-09", NA, NA, NA, NA, NA, NA)),
  first_cover_ge4 = as.Date(c(NA, NA, NA, NA, NA, NA, NA)),
  later_rank = c(4L, 3L, 2L, 0L, 0L, 0L, 3L))

test_that("a warning is before, same day, later days only or not covered by its issue day", {
  x <- cov_fake()
  expect_equal(coverage_category(x$issue_date, x$first_cover_ge1, x$later_rank, 1), c("before", "same_day", "later", "none", "before", "same_day", "later"))
  expect_equal(coverage_lead_days(x$issue_date, x$first_cover_ge1), c(3L, NA, NA, NA, 1L, NA, NA))
  expect_equal(coverage_label(x$issue_date, x$first_cover_ge1, x$later_rank, 1)[c(1, 2, 3, 4, 5)],
               c("Before (3 days)", "Same day", "Later days only", "Not covered", "Before (1 day)"))
  # At Medium, the same-day polygon was only Low or Very Low on the issue day, but a Medium polygon came on a later day.
  expect_equal(coverage_category(x$issue_date, x$first_cover_ge3, x$later_rank, 3)[1:3], c("before", "later", "none"))
  expect_equal(coverage_category(x$issue_date, x$first_cover_ge4, x$later_rank, 4)[1:2], c("later", "none"))
})

test_that("the summary counts each threshold, the period and the kind of warning", {
  x <- cov_fake()
  s <- coverage_summary_counts(x, "2025-01-01", "2025-12-31", "any")
  at1 <- s[threshold == 1L]
  expect_equal(sum(at1$n), 6L)                                           # the 2024 warning is outside the period
  expect_equal(at1[category == "before", sum(n)], 2L)
  expect_equal(at1[category == "before" & lead_days == 3L, n], 1L)
  expect_equal(at1[category == "later", sum(n)], 2L)
  w <- coverage_summary_counts(x, "2025-01-01", "2025-12-31", "warnings")
  expect_equal(sum(w[threshold == 1L]$n), 4L)
  sv <- coverage_summary_counts(x, "2025-01-01", "2025-12-31", "severe")
  expect_equal(sum(sv[threshold == 1L]$n), 1L)
  expect_error(coverage_summary_counts(x, "2025-01-01", "2025-12-31", "everything"))
  expect_equal(nrow(coverage_summary_counts(x, "2020-01-01", "2020-12-31")), 0L)
})

test_that("shares are of all warnings in the period, and covered on the issue day is before plus same day", {
  s <- coverage_shares(coverage_summary_counts(cov_fake(), "2025-01-01", "2025-12-31"))
  r1 <- s[threshold == 1L]
  expect_equal(c(r1$before, r1$same_day, r1$later, r1$none, r1$total), c(2, 1, 2, 1, 6))
  expect_equal(r1$covered_pct, 3 / 6)
  expect_equal(s[threshold == 4L]$later_pct, 1 / 6)                       # only e1 had a High polygon, and on a later day
  expect_true(is.na(coverage_shares(data.table(threshold = integer(), category = character(), lead_days = integer(), n = integer()))$covered_pct[1]))
})

test_that("timing says how long before or after the warning a statement was issued", {
  expect_equal(relative_timing("2026-10-10T09:30:00", "2026-10-10T14:00:00"), "4 h 30 min before")
  expect_equal(relative_timing("2026-10-07T12:00:00", "2026-10-10T14:00:00"), "3 d 2 h before")
  expect_equal(relative_timing("2026-10-11T09:30:00", "2026-10-10T14:00:00"), "19 h 30 min after")
  expect_equal(relative_timing("2026-10-10T14:00:00", "2026-10-10T14:00:00"), "at the same time")
})


test_that("a rain gauge the API calls only 'Rainfall station' takes its mapped name, and ids go on the label", {
  reg <- data.table(
    station_label = c("Rainfall station", "Rainfall station", "Aldwyn", "Aldwyn Gauge", NA),
    station_reference = c("R1", "R2", "S1", "S2", "S3"), station_uri = c("u/R1", "u/R2", "u/S1", "u/S2", "u/S3"),
    measure_uri = c("m/1", "m/2", "m/3", "m/4", "m/5"), parameter = c("rainfall", "rainfall", "level", "level", "rainfall"),
    qualifier = "Gauge", unit_name = "mm",
    gauge_name = c("Cwm Rheidol RG", NA, "Wrongly Named", NA, "Beer Quarry Lane RG"),
    wiski_id = c("520455", NA, "713207", NA, NA), fffs_id = c("F1", NA, NA, NA, "F5"))
  r <- rt_prepare_register(reg)
  expect_equal(r$station[1], "Cwm Rheidol RG")                              # a generic name is replaced
  expect_equal(r$station[2], "Rainfall station")                            # no mapped name: unchanged
  expect_equal(r$station[3], "Aldwyn")                                      # a real API name is kept, not overridden
  expect_equal(r$station[5], "Beer Quarry Lane RG")                         # a blank name takes the mapped one
  expect_match(r$measure_label[1], "WISKI 520455 · FFFS F1$")
  expect_match(r$measure_label[5], "· FFFS F5$"); expect_false(grepl("WISKI", r$measure_label[5], fixed = TRUE))
  expect_false(grepl("WISKI|FFFS", r$measure_label[2]))
  expect_equal(rt_generic_name(c("Rainfall station", "Rain gauge", "Aldwyn", "", NA)), c(TRUE, TRUE, FALSE, TRUE, TRUE))
  # Without the mapping columns at all, the register still prepares.
  expect_equal(nrow(rt_prepare_register(reg[, 1:7])), 5L)
})


test_that("a gauge's instantaneous and total measures are told apart by value type, and a datum pair by its unit", {
  reg <- data.table(
    station_label = c("Wingfield", "Wingfield", "Keadby", "Keadby"), station_reference = c("R1", "R1", "T1", "T1"),
    station_uri = c("u/R1", "u/R1", "u/T1", "u/T1"), measure_uri = c("m/i", "m/t", "m/a", "m/b"),
    parameter = c("rainfall", "rainfall", "level", "level"), qualifier = c("Gauge", "Gauge", "Tidal Level", "Tidal Level"),
    unit_name = c("mm", "mm", "mAOD", "m"), value_type = c("instantaneous", "total", "instantaneous", "instantaneous"),
    period = c(900L, 900L, 900L, 900L), fffs_id = c("F1", "F1", "F2", "F2"))
  r <- rt_prepare_register(reg)
  expect_equal(anyDuplicated(r$measure_label), 0L)
  expect_match(r$measure_label[1], "\\(instantaneous\\)"); expect_match(r$measure_label[2], "\\(total\\)")
  expect_match(r$measure_label[3], "\\(mAOD\\)"); expect_match(r$measure_label[4], "\\(m\\)")
  expect_false(any(grepl("\\[", r$measure_label)))                  # no URI tail was needed
})


# ---- Rain gauges --------------------------------------------------------------------

rain_fake <- function(now = as.POSIXct("2026-10-23 12:00:00", tz = "UTC")) data.table(
  measure_uri = c("m/a-i", "m/a-t", "m/b-t", "m/c-t", "m/d-t"), station_uri = c("u/A", "u/A", "u/B", "u/C", "u/D"),
  station_reference = c("A", "A", "B", "C", "D"), station_label = c("Aldwyn RG", "Aldwyn RG", "Rainfall station", "Cardell", "Dunmere"),
  ea_area_name = c("North", "North", "North", "South", "South"), lat = c(53, 53, 52.5, 51.5, 51.2), long = c(-2, -2, -1.5, -1, -0.5),
  unit_name = "mm", value_type = c("instantaneous", "total", "total", "total", "total"), period = "900", parameter = "rainfall",
  gauge_name = c(NA, NA, "Hexton RG", NA, NA), fffs_id = c("F1", "F1", "F2", NA, NA), wiski_id = NA_character_, n_bad = c(0, 0, 3, 0, NA),
  mm_1h = c(1, 0.4, 0, 0, NA), mm_12h = c(5, 4.2, 0, 0, NA), mm_24h = c(9, 8.1, 0, 0.1, NA), n_24h = c(96, 96, 96, 90, NA),
  last_time = now - c(600, 600, 300, 7200, 86400), last_value = c(0.2, 0.1, 0, 0, 0.3))

test_that("a gauge with two measures is drawn once, on its totals, and takes its mapped name", {
  now <- as.POSIXct("2026-10-23 12:00:00", tz = "UTC")
  r <- rain_prepare(rain_fake(now), now)
  expect_equal(nrow(r), 4L)
  a <- r[station_reference == "A"]
  expect_equal(a$measure_uri, "m/a-t"); expect_equal(a$mm_24h, 8.1)
  expect_equal(r[station_reference == "B"]$station, "Hexton RG")           # "Rainfall station" is replaced by the mapped name
  expect_equal(r[station_reference == "C"]$stale, FALSE)                   # two hours: still recent
  expect_equal(r[station_reference == "D"]$stale, TRUE)                    # a day old
  expect_equal(r[station_reference == "A"]$n_expected, 96)
  expect_equal(nrow(rain_prepare(NULL)), 0L)
})

test_that("rainfall is banded by window, and a stale gauge is grey whatever it last read", {
  expect_equal(rain_band(c(0, 0.2, 4.9, 5, 39, 40, NA), "mm_24h"), c(1L, 2L, 2L, 3L, 5L, 6L, NA))
  expect_equal(rain_band(0.5, "mm_1h"), 3L); expect_equal(rain_band(0.5, "mm_12h"), 2L)
  expect_length(rain_band_labels("mm_24h"), 6L); expect_equal(rain_band_labels("mm_1h")[1:2], c("Under 0.1 mm", "0.1 to 0.5 mm"))
  now <- as.POSIXct("2026-10-23 12:00:00", tz = "UTC")
  r <- rain_prepare(rain_fake(now), now)
  f <- rain_fill(r, "mm_24h")
  expect_equal(f[r$station_reference == "D"], RAIN_STALE_COLOUR)
  expect_equal(f[r$station_reference == "A"], RAIN_COLOURS[3L])            # 8.1 mm is the 5 to 10 band
})

test_that("the popup gives the latest reading and all three totals, and escapes names", {
  now <- as.POSIXct("2026-10-23 12:00:00", tz = "UTC")
  d <- rain_fake(now); d[station_reference == "A", station_label := "Ald<b>wyn"]
  r <- rain_prepare(d, now)
  p <- rain_popup(r[station_reference == "A"])
  expect_match(p, "Past 1 hour: 0.4 mm"); expect_match(p, "Past 12 hours: 4.2 mm"); expect_match(p, "Past 24 hours: 8.1 mm")
  expect_match(p, "Latest: 0.1 mm at Fri 23 Oct 11:50 UTC \\(10 min ago\\)"); expect_match(p, "FFFS F1"); expect_match(p, "96 of 96 readings")
  expect_false(grepl("<b>wyn", p, fixed = TRUE)); expect_match(p, "Ald&lt;b&gt;wyn")
  q <- rain_popup(r[station_reference == "D"])
  expect_match(q, "No recent reading"); expect_match(q, "Past 24 hours: no readings"); expect_match(q, "0 of 96 readings")
  expect_equal(rain_age_text(c(5, 120, 4000, NA)), c("5 min ago", "2.0 h ago", "3 days ago", ""))
})

test_that("the area and stale filters and the summary work", {
  now <- as.POSIXct("2026-10-23 12:00:00", tz = "UTC")
  r <- rain_prepare(rain_fake(now), now)
  expect_equal(nrow(rain_filter(r, "South")), 2L); expect_equal(nrow(rain_filter(r, RT_ALL, TRUE)), 3L)
  s <- rain_summary(r, "mm_24h")
  expect_equal(s$n, 4L); expect_equal(s$recent, 3L); expect_equal(s$wet, 2L)          # A (8.1) and C (0.1) had rain; B was dry; D is stale
  expect_equal(s$wettest$name, "Aldwyn RG"); expect_equal(s$wettest$mm, 8.1)
  expect_equal(rain_summary(r[0], "mm_24h")$n, 0L)
})


test_that("the FFFS ID narrows every Hydrometrics drop-down and the measure list", {
  reg <- data.table(
    station_label = c("Aldwyn", "Aldwyn", "Rainfall station", "Rainfall station"), station_reference = c("S1", "S1", "R1", "R2"),
    station_uri = c("u/S1", "u/S1", "u/R1", "u/R2"), measure_uri = c("m/1", "m/2", "m/3", "m/4"),
    parameter = c("level", "flow", "rainfall", "rainfall"), qualifier = c("Stage", "Flow", "Gauge", "Gauge"),
    unit_name = c("mASD", "m3/s", "mm", "mm"), river_name = c("Aldwyn", "Aldwyn", NA, NA), ea_area_name = c("North", "North", "North", "South"),
    fffs_id = c("F1", "F1", "F3", NA), gauge_name = c(NA, NA, "Hexton RG", NA))
  r <- rt_prepare_register(reg)
  expect_equal(nrow(rt_filter_register(r, fffs = "F1")), 2L)
  o <- rt_options(r, fffs = "F3")
  expect_equal(o$areas, c("All", "North")); expect_equal(o$parameters, c("All", "rainfall")); expect_equal(o$stations, c("All", "Hexton RG"))
  expect_equal(o$fffs, c("All", "F1", "F3"))                       # the FFFS choices are not narrowed by the FFFS ID itself
  expect_equal(rt_options(r, area = "South")$fffs, "All")           # no mapped gauge in the South: only All is offered
  expect_equal(rt_options(r, param = "flow")$fffs, c("All", "F1"))
})


test_that("in_selected_cells keeps the rows whose risk cell is ticked and nothing when none is", {
  cells <- data.table(x = c(2L, 3L), y = c(4L, 1L))
  expect_equal(in_selected_cells(c(2L, 3L, 2L, 1L), c(4L, 1L, 1L, 1L), cells), c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(in_selected_cells(c(2, 3), c(4, 1), cells), c(TRUE, TRUE))                  # numbers from the warehouse match integers
  expect_equal(in_selected_cells(c(2L, 3L), c(4L, 1L), cells[0]), c(FALSE, FALSE))
  expect_equal(in_selected_cells(integer(), integer(), cells), logical())
})


test_that("an impossible reading is left out of the value, named in the popup, and counted", {
  now <- as.POSIXct("2026-10-23 12:00:00", tz = "UTC")
  d <- rain_fake(now); d[station_reference == "B", last_value := 19999999]
  r <- rain_prepare(d, now)
  b <- r[station_reference == "B"]
  expect_true(b$last_bad); expect_true(is.na(b$last_value)); expect_equal(b$last_raw, 19999999)
  p <- rain_popup(b)
  expect_match(p, "an impossible reading \\(19,999,999 mm\\), ignored"); expect_match(p, "3 readings were ignored as impossible")
  expect_false(any(grepl("ignored as impossible", rain_popup(r[station_reference == "A"]))))
  expect_equal(if_else_s(c(1, 2), "was", "were"), c("was", "were"))
  expect_equal(RAIN_MAX_READING, 100)
})


test_that("rain bars sum the readings into hours or quarter hours and leave out impossible ones", {
  t0 <- as.POSIXct("2026-10-23 10:00:00", tz = "UTC")
  rd <- data.table(date_time = t0 + c(0, 900, 1800, 2700, 3600, 4500, 5400), value = c(0.2, 0.4, 0, 0.1, 1, 19999999, -3))
  h <- rain_bars(rd, 3600L)
  expect_equal(nrow(h$bars), 2L); expect_equal(h$bars$mm, c(0.7, 1)); expect_equal(h$ignored, 2L)
  expect_equal(h$bars$t, t0 + c(0, 3600))                                          # each bar is labelled by its start
  q <- rain_bars(rd, 900L)
  expect_equal(nrow(q$bars), 5L); expect_equal(q$bars$mm, c(0.2, 0.4, 0, 0.1, 1))
  expect_equal(nrow(rain_bars(NULL)$bars), 0L); expect_equal(rain_bars(rd[value > 1000])$ignored, 1L)
})


test_that("a gauge is suspect when it sent impossible readings or its total is too large to believe, and is kept out of the wettest", {
  now <- as.POSIXct("2026-10-23 12:00:00", tz = "UTC")
  d <- rain_fake(now)
  d[station_reference == "C", mm_24h := 412.5]                  # no bad reading, but 412 mm in a day is not believable
  r <- rain_prepare(d, now)
  sus <- rain_suspect(r, "mm_24h")
  expect_equal(sus[r$station_reference == "A"], FALSE)
  expect_equal(sus[r$station_reference == "B"], TRUE)           # n_bad is 3
  expect_equal(sus[r$station_reference == "C"], TRUE)           # over the 300 mm limit
  expect_equal(rain_suspect(r, "mm_1h")[r$station_reference == "C"], FALSE)   # the limit is per window; C's hour is fine
  expect_equal(rain_suspect_text(r[station_reference == "C"]), "<br><em>Suspect: the past 24 hours total (412.5 mm) is above 300 mm.</em>")
  expect_equal(rain_suspect_text(r[station_reference == "A"]), "")
  expect_match(rain_popup(r[station_reference == "C"]), "Suspect: the past 24 hours total")
  expect_equal(nrow(rain_filter(r, RT_ALL, FALSE, TRUE, "mm_24h")), 2L)       # B and C are hidden
  expect_equal(nrow(rain_filter(r, RT_ALL, FALSE, FALSE, "mm_24h")), 4L)
  s <- rain_summary(r, "mm_24h")
  expect_equal(s$suspect, 2L); expect_equal(s$wettest$name, "Aldwyn RG"); expect_equal(s$wet, 1L)   # C is not counted as the wettest
  # The limit is the caller's: a lower one marks a reading impossible that the default would not.
  expect_true(rain_prepare(transform(rain_fake(now), last_value = 60), now, max_mm = 50)$last_bad[1])
  expect_false(rain_prepare(transform(rain_fake(now), last_value = 60), now)$last_bad[1])
  expect_equal(rain_bars(data.table(date_time = now - c(600, 1200), value = c(60, 1)), 3600L, max_mm = 50)$ignored, 1L)
  expect_match(rain_popup(rain_prepare(transform(rain_fake(now), n_bad = 2), now, max_mm = 50)[1]), "over 50 mm")
})


test_that("a gauge given only as an easting and northing is placed, and one with no position is counted but not drawn", {
  skip_if_not_installed("sf")
  ll <- rain_latlong(c(53.1, NA, NA), c(-1.5, NA, NA), c(NA, 530000, NA), c(NA, 180000, NA))
  expect_equal(ll$lat[1], 53.1); expect_equal(ll$long[1], -1.5)                  # a given latitude and longitude is untouched
  expect_equal(ll$lat[2], 51.50, tolerance = 0.02); expect_equal(ll$long[2], -0.13, tolerance = 0.02)   # central London
  expect_true(is.na(ll$lat[3]) && is.na(ll$long[3]))
  now <- as.POSIXct("2026-10-23 12:00:00", tz = "UTC")
  d <- rain_fake(now)
  d[station_reference == "C", `:=`(lat = NA_real_, long = NA_real_, easting = 530000, northing = 180000)]
  d[station_reference == "D", `:=`(lat = NA_real_, long = NA_real_)]
  r <- rain_prepare(d, now)
  expect_equal(r[order(station_reference)]$placed, c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(r[station_reference == "C"]$lat, 51.50, tolerance = 0.02)
  expect_equal(nrow(rain_filter(r)), 3L)                                           # the unplaced gauge is not drawn
  expect_equal(nrow(rain_filter(r, hide_stale = TRUE)), 3L - 0L)
})


test_that("adding a gauge to the chart keeps the order, ignores repeats and unknowns, and drops the oldest when full", {
  valid <- c("a", "b", "c", "d")
  expect_equal(rain_add_pick(character(), "a", valid), "a")
  expect_equal(rain_add_pick("a", "b", valid), c("a", "b"))
  expect_equal(rain_add_pick(c("a", "b"), "a", valid), c("a", "b"))              # already there
  expect_equal(rain_add_pick("a", "zzz", valid), "a"); expect_equal(rain_add_pick("a", NA_character_, valid), "a"); expect_equal(rain_add_pick("a", "", valid), "a")
  expect_equal(rain_add_pick(c("a", "b", "c"), "d", valid, max = 3L), c("b", "c", "d"))
  expect_equal(RAIN_MAX_CHARTED, 6L)
})


test_that("charted gauges take their colours from the series palette in turn", {
  expect_equal(rain_series_colour(1), "#053D7E"); expect_equal(rain_series_colour(2), "#BE0553")
  expect_equal(rain_series_colour(9), rain_series_colour(1))                      # it wraps
})


test_that("rain_cumulative carries the total across missing intervals and flags them", {
  t0 <- as.POSIXct("2026-10-26 10:00:00", tz = "UTC")
  bars <- data.table(t = t0 + c(0, 3600, 3 * 3600), mm = c(1, 2, 4))
  cu <- rain_cumulative(bars, 3600L, t0, t0 + 4 * 3600)
  expect_equal(nrow(cu), 5L)
  expect_equal(cu$cum, c(1, 3, 3, 7, 7))
  expect_equal(cu$missing, c(FALSE, FALSE, TRUE, FALSE, TRUE))        # an hour with no reading adds nothing and is flagged
  expect_true(is.na(cu$mm[3]))
  none <- rain_cumulative(NULL, 3600L, t0, t0 + 2 * 3600)
  expect_equal(none$cum, c(0, 0, 0)); expect_true(all(none$missing))
  expect_equal(nrow(rain_cumulative(bars, 900L, t0, t0 + 3600)), 5L)   # quarter-hour grid
})


test_that("the export tables hold the rows and columns the Exports tab promises", {
  expect_equal(nation_label(c("E14000001", "W07000002", "S14000003", "N06000004", "DEMO-C001")), c("England", "Wales", "Scotland", "Northern Ireland", ""))

  links <- data.table(ea_area_code = c("A1", "A1", "A2"), ea_area_name = c("River A", "River A", "Coast B"),
                      area_kind = c("Flood warning", "Flood warning", "Flood alert"), type_text = c("Flood Warning Fluvial", "Flood Warning Fluvial", "Flood Alert Tidal"),
                      county = "Kent", river_or_sea = c("A", "A", "Sea"), constituency_id = c("E1", "E2", "E1"), constituency_name = c("One", "Two", "One"),
                      share_of_area_in_constituency = c(0.60, 0.004, 0.25), mp_name = c("Ann", "Bo", "Ann"), mp_party = "X", mp_email = c("a@x", "b@x", "a@x"))
  d <- exp_links_table(links, min_pct = 0.01)
  expect_equal(nrow(d), 2L)                                                         # the 0.4 per cent sliver is left out by default
  expect_equal(nrow(exp_links_table(links, include_all = TRUE, min_pct = 0.01)), 3L)
  expect_true(all(c("Area code", "Kind", "Constituency", "MP email") %in% names(d)))
  expect_equal(d[["Share of the area in the constituency (%)"]][d$`Area code` == "A1"], 60)
  expect_equal(nrow(exp_links_table(NULL)), 0L)

  sit <- data.table(constituency_id = c("W07000002", "E14000001"), constituency_name = c("Zed", "Abbey"), mp_name = c(NA, "Ann Lee"), mp_party = c(NA, "X"),
                    mp_email = c(NA, "ann@x"), mp_phone = c(NA, "020"))
  c2 <- exp_constituency_table(sit)
  expect_equal(c2$Constituency, c("Abbey", "Zed")); expect_equal(c2$Nation, c("England", "Wales"))
  expect_equal(c2$MP[2], "(vacant or unmatched)")

  b <- data.table(statement_id = 4812L, constituency_id = c("E1", "E2"), constituency_name = c("One", "Two"), mp_name = c("Ann", "Bo"), mp_email = c("a@x", NA),
                  worst_rank = c(3L, 4L), worst_level = c("Medium", "High"), worst_date = c("2026-10-28", "2026-10-27"), is_escalation = c(TRUE, FALSE),
                  has_active_warning = c(FALSE, TRUE), subject = c("s1", "s2"), body = c("b1\nline", "b2"), status = "draft", reviewed_by = NA)
  e <- exp_email_table(b)
  expect_equal(e$Constituency, c("Two", "One"))                                      # the highest risk first
  expect_equal(e$`Warning in force`, c("Yes", "No")); expect_equal(e$Escalated, c("No", "Yes"))
  expect_true(all(c("To (MP email)", "Subject", "Body", "Status") %in% names(e)))
})


test_that("a threshold line is named from the label prefix and the target area, once each, and starred when converted", {
  thr <- data.table(measure_uri = c("u1", "u1", "u1", "u2", "u3"), type = c("ACTCON", "RES", "RES", "RES", "RES"), purpose = "FW", season = "All",
                    label_prefix = c("ACTCON FW", "RES FW", "RES FW", "RES FW", NA), target_area = c("065FWF3007", "065FWF3007", "065FWF3007", "061FWF23Ewen", NA),
                    value = c(1.29, 1.94, 1.94, 5.5, 2.2), unit_name = "m", unit_match = c("exact", "converted", "converted", "exact", "exact"))
  l <- rt_threshold_lines(thr, "u1")
  expect_equal(l$value, c(1.29, 1.94))                                    # sorted by value, the repeat dropped
  expect_equal(l$short, c("ACTCON FW 065FWF3007", "RES FW 065FWF3007*"))    # prefix and target area; a star where converted
  expect_match(l$label[2], "RES FW 065FWF3007: 1.94 \\(converted")
  expect_false(grepl("converted", l$label[1]))
  expect_equal(rt_threshold_lines(thr, "u3")$short, "RES FW")             # no prefix and no area: Type and Purpose stand in
  expect_equal(nrow(rt_threshold_lines(thr, "nothing")), 0L); expect_equal(nrow(rt_threshold_lines(NULL, "u1")), 0L)
  expect_equal(rt_threshold_lines(thr, "u1", res_only = TRUE)$short, "RES FW 065FWF3007*")           # the RES filter drops ACTCON
  expect_equal(nrow(rt_threshold_lines(thr, "u1", res_only = FALSE)), 2L)
  old <- thr[, .(measure_uri, type, purpose, season, value, unit_name, unit_match)]   # a links table from before the prefix was carried
  expect_equal(rt_threshold_lines(old, "u2")$short, "RES FW")
})


test_that("the area popup carries the figures, the constituencies and a link to the detail, and is empty without a match", {
  ex <- data.frame(area_id = c("A", "B"), people = c(12345, 80), over65 = c(2000, 9), people_rs_high_medium = c(2000, 0), households_rs_high_medium = c(800, 0),
                   sites = c(14, 0), sites_rs_high_medium = c(8, 0), share_people_high_medium = c(0.162, 0))
  out <- area_exposure_popup(c("B", "A", "Z"), ex, c(A = "York Central, York Outer"))
  expect_match(out[2L], "12,345")
  expect_match(out[2L], "<b>14</b> \\(8 in High or Medium\\)")
  expect_match(out[2L], "York Central, York Outer")
  expect_match(out[2L], "Shiny.setInputValue\\('area_open','A'")
  expect_false(grepl("Constituencies", out[1L]))
  expect_equal(out[3L], "")
  expect_equal(area_exposure_popup(c("A", "B"), NULL), c("", ""))
})

test_that("band helpers: bars sum to the share, chips carry the band, links are safe, constituencies join by area", {
  x <- data.frame(people = 100, people_rs_high = 20, people_rs_medium = 20, people_rs_low = 30, people_rs_very_low = 10)
  bar <- band_bar_html(x)
  expect_match(bar, "High 20%, Medium 20%, Low 30%, Very Low 10%")
  expect_match(band_chip("High"), "#053D7E")
  expect_false(grepl("<script", area_open_link("A'\"<script>", "x<b>"), fixed = TRUE))
  links <- data.frame(ea_area_code = c("A", "A", "B"), constituency_name = c("One", "Two", "Three"), share_of_area_in_constituency = c(0.6, 0.4, 0.001))
  expect_equal(area_constituency_names(links, 0.01), c(A = "One, Two"))
  expect_equal(grid_band_colour(c(0, 1, 4, 5)), c("#F3F2F1", "#CFE3F6", "#053D7E", "#053D7E"))
})

test_that("grid helpers: the BNG box holds the view, corners come back as a rectangle, colours step by band", {
  skip_if_not_installed("sf")
  b <- bbox_to_bng(-1.56, 53.79, -1.52, 53.82)                      # central Leeds
  expect_true(b$xmin > 425000 && b$xmax < 435000 && b$ymin > 430000 && b$ymax < 440000)
  expect_true(b$xmax > b$xmin && b$ymax > b$ymin)
  cells <- grid_cell_corners(data.frame(easting = 430000, northing = 433000, risk_score = 60))
  expect_true(cells$lng2 > cells$lng1 && cells$lat2 > cells$lat1)
  expect_lt(abs(cells$lng1 - -1.54), 0.1)
  expect_equal(nrow(grid_cell_corners(data.frame(easting = numeric(), northing = numeric()))), 0L)
  expect_equal(grid_colour(c(0, 9.9, 10, 49, 75, 100)), GRID_COLOURS[c(1, 1, 2, 3, 5, 5)])
  expect_match(grid_popup(data.frame(risk_score = 61.4, people = 1234, haz_rs = 3.5, imd_decile = 2, critical_sites = 1)), "Risk score 61 of 100.*1,234")
})


test_that("grid cells are true squares on the British National Grid, and the step keeps the cell count down", {
  skip_if_not_installed("sf")
  sq <- grid_cells_sf(data.frame(easting = c(430000, 430100), northing = 433000, risk_score = c(10, 20)), 100)
  expect_s3_class(sq, "sf"); expect_equal(nrow(sq), 2L); expect_equal(as.character(sf::st_geometry_type(sq))[1L], "POLYGON")
  expect_equal(sf::st_crs(sq)$epsg, 4326L)
  shared <- sf::st_intersection(sf::st_transform(sq[1, ], 27700), sf::st_transform(sq[2, ], 27700))      # neighbours touch along one edge, no gap or overlap
  expect_equal(as.numeric(sf::st_length(shared)), 100, tolerance = 1e-3)
  expect_equal(nrow(grid_cells_sf(data.frame(easting = numeric(), northing = numeric()))), 0L)
  expect_equal(grid_step_for(1000), 100); expect_equal(grid_step_for(40000), 200); expect_equal(grid_step_for(300000), 500); expect_equal(grid_step_for(1e9), 2000)
})

test_that("the guide link opens the Guide, scrolls to the entry's card, and cannot be used to inject", {
  l <- guide_link_html("risk-grid", "How it is calculated")
  expect_match(l, "Shiny.setInputValue('nav_to','Guide'", fixed = TRUE)
  expect_match(l, "getElementById('guide-risk-grid')", fixed = TRUE)
  expect_false(grepl("<script", guide_link_html("a'\"<script>", "x<b>"), fixed = TRUE))
  expect_match(grid_popup(data.frame(risk_score = 50, people = 10, haz_rs = 2, imd_decile = 3, critical_sites = 0)), "guide-risk-grid", fixed = TRUE)
  expect_true(any(grepl('id = paste0("guide-", p$id)', readLines(file.path(app_root, "guide.R")), fixed = TRUE)))        # the anchor the link targets
  expect_true(any(grepl('id = "risk-grid"', readLines(file.path(app_root, "guide.R")), fixed = TRUE)))
})

test_that("shareable links drop table and map mechanics but keep real filters and text boxes", {
  nm <- c("main_tabs", "areas_kind", "areas_search", "areas_table_rows_selected", "areas_table_rows_all", "areas_table_rows_current",
          "areas_table_state", "areas_table_search", "areas_table_search_columns", "areas_table_cell_clicked", "areas_table_row_last_clicked",
          "area_grid_map_bounds", "area_grid_map_zoom", "area_grid_map_center", "area_grid_map_click", "area_grid_map_shape_click",
          "live_map_groups", "sit_search", "area_pick", "grid_min_score", "layer_risk_grid", "nations", "statement_id")
  drop <- bookmark_noise(nm)
  expect_true(all(c("areas_table_rows_selected", "areas_table_rows_all", "areas_table_rows_current", "areas_table_state", "areas_table_search",
                    "areas_table_search_columns", "areas_table_cell_clicked", "areas_table_row_last_clicked", "area_grid_map_bounds",
                    "area_grid_map_zoom", "area_grid_map_center", "area_grid_map_click", "area_grid_map_shape_click", "live_map_groups") %in% drop))
  expect_false(any(c("main_tabs", "areas_kind", "areas_search", "sit_search", "area_pick", "grid_min_score", "layer_risk_grid", "nations", "statement_id") %in% drop))
  expect_equal(bookmark_noise(character()), character())
})


brief_area <- function(...) {
  a <- data.frame(area_id = "A1", area_name = "River Ouse in Ynys M\u00f4n", type_text = "Flood warning area", county = "North Yorkshire", risk_score = 31.24,
                  people = 18400, households = 8150, over65 = 3260, share_people_imd_1_2 = 0.12, sites = 14, people_rs_high_medium = 5400,
                  share_people_high_medium = 0.2935, households_rs_high_medium = 2140, people_shared = 900, people_rs_high = 1980, people_rs_medium = 3420,
                  people_rs_low = 5100, people_rs_very_low = 7900, households_rs_high = 930, households_rs_medium = 1210, households_rs_low = 2040, households_rs_very_low = 3300)
  for (nm in names(list(...))) a[[nm]] <- list(...)[[nm]]
  a
}

test_that("the area brief content has the figures, keeps high and medium sites only, and degrades when the lists are missing", {
  sites <- data.frame(name = c("Rowan Court", "Far school"), site_type = c("Care home", "School"), band = c("High", "None"), rofrs_rank = c(4L, 0L))
  geos <- data.frame(geography = c("police_force_area", "police_force_area", "region"), name = c("North Yorkshire Police", NA, "Yorkshire"), code = c("E23", "E99", "E12"), people = c(15000, 300, 18400))
  k <- area_brief_content(brief_area(), sites, geos, "York Central, York Outer", generated = as.POSIXct("2026-10-28 12:00", tz = "UTC"))
  expect_equal(k$kpis$value[1:2], c("18,400", "8,150")); expect_match(k$subtitle, "risk score 31.2 of 100"); expect_match(k$summary, "5,400 people \\(29%\\)")
  expect_equal(k$sites$name, "Rowan Court")
  expect_match(k$responders$who[k$responders$what == "Police force"], "North Yorkshire Police (15,000); E99 (300)", fixed = TRUE)
  expect_equal(k$constituencies, "York Central, York Outer"); expect_equal(k$bands$band, c("High", "Medium", "Low", "Very Low"))
  bare <- area_brief_content(brief_area(), NULL, NULL, NA_character_)
  expect_match(bare$sites_note, "not available"); expect_match(bare$responders_note, "not available"); expect_equal(bare$constituencies, "Not available.")
  expect_false(grepl("risk score", area_brief_content(brief_area(risk_score = NA_real_))$subtitle))
  a2 <- brief_area(); a2$risk_score <- NULL                                                 # an exposure table built before the score existed
  expect_false(grepl("risk score", area_brief_content(a2)$subtitle))
})

test_that("the area brief is a one page PDF, including with accented names, long lists and nothing to list", {
  sites <- data.frame(name = sprintf("Site number %d with a fairly long descriptive name", 1:40), site_type = "School", band = rep(c("High", "Medium"), 20), rofrs_rank = rep(c(4L, 3L), 20))
  geos <- data.frame(geography = rep(names(RESPONDER_GEOGRAPHIES), each = 3), name = sprintf("Authority %d of the area", 1:21), code = sprintf("E%d", 1:21), people = 15000 / (1:21))
  f <- tempfile(fileext = ".pdf"); on.exit(unlink(f), add = TRUE)
  area_brief_pdf(f, area_brief_content(brief_area(), head(sites, 6), geos, "York Central, York Outer"))
  raw <- readBin(f, "raw", file.size(f))
  expect_equal(rawToChar(raw[1:4]), "%PDF"); expect_gt(file.size(f), 3000)
  # page counts need pdfinfo (poppler); the checks that do not are above and below it
  pages <- function(path) if (nzchar(Sys.which("pdfinfo"))) as.integer(sub("Pages:\\s+", "", grep("^Pages:", system2("pdfinfo", path, stdout = TRUE), value = TRUE))) else 1L
  expect_equal(pages(f), 1L)
  g <- tempfile(fileext = ".pdf"); on.exit(unlink(g), add = TRUE)
  area_brief_pdf(g, area_brief_content(brief_area(), sites, geos, "x"))                 # 40 sites are cut to the first 18, still one page
  expect_equal(pages(g), 1L)
  h <- tempfile(fileext = ".pdf"); on.exit(unlink(h), add = TRUE)
  area_brief_pdf(h, area_brief_content(brief_area(), NULL, NULL, NA_character_))
  expect_equal(pages(h), 1L)
})

test_that("older exposure tables get the area score columns empty, and the score bar fills with the score", {
  x <- ensure_area_columns(data.frame(area_id = "A", people = 5))
  expect_true(all(c("risk_score", "risk_score_max", "grid_cells") %in% names(x))); expect_true(is.na(x$risk_score))
  y <- ensure_area_columns(data.frame(area_id = "A", risk_score = 31.2))
  expect_equal(y$risk_score, 31.2)
  b <- score_bar_html(c(31.24, NA, 140))
  expect_match(b[1L], "<b>31.2</b>"); expect_match(b[1L], "width:31%"); expect_equal(b[2L], "-"); expect_match(b[3L], "width:100%")
})

test_that("the in-force summary counts warnings against warnings, takes shared people off once, and tolerates missing figures", {
  f <- data.frame(flood_area_id = c("W1", "W2", "S1", "A1", "A9"), severity_level = c(2L, 2L, 1L, 3L, 3L))
  ex <- data.frame(kind = c("flood_warning", "flood_warning", "flood_warning", "flood_alert"), area_id = c("W1", "W2", "S1", "A1"),
                   people = c(1000, 500, 200, 4000), households = c(400, 200, 80, 1600), people_rs_high_medium = c(100, 50, 20, 400))
  pairs <- data.frame(kind = c("flood_warning", "flood_warning", "flood_alert"), area_id_a = c("W1", "W1", "A1"), area_id_b = c("W2", "ZZ", "A2"), people = c(100, 999, 50))
  s <- inforce_exposure_summary(f, ex, pairs)
  w <- s[s$kind == "flood_warning", ]; a <- s[s$kind == "flood_alert", ]
  expect_equal(w$areas, 3L); expect_equal(w$matched, 3L)
  expect_equal(w$people, 1700 - 100)                          # 1,700 gross, W1 and W2 share 100; the pair with an area not in force is ignored
  expect_equal(w$people_at_risk, 170 * (1600 / 1700))
  expect_equal(a$areas, 2L); expect_equal(a$matched, 1L); expect_equal(a$people, 4000)
  expect_equal(nrow(inforce_exposure_summary(f[0, ], ex, pairs)), 0L)
  expect_equal(nrow(inforce_exposure_summary(f, NULL, pairs)), 0L)
  expect_equal(inforce_exposure_summary(f[f$severity_level == 3L, ], ex, NULL)$people, 4000)
})


test_that("the Who responds tables draw: one per geography present, biggest first, names or codes, and nothing leaks into attributes", {
  g <- data.frame(geography = c("police_force_area", "police_force_area", "region", "fire_and_rescue_authority"), code = c("P1", "P2", "R1", "F1"),
                  name = c("North Police", NA, "Yorkshire", "North Fire"), people = c(900, 100, 1000, 1000), share_of_area_people = c(0.9, 0.1, 1, 1), people_rs_high_medium = c(90, 10, 100, 100))
  ui <- responder_tables_ui(g)
  html <- as.character(ui)
  expect_equal(lengths(regmatches(html, gregexpr("<table", html, fixed = TRUE))), 3L)          # police, fire, region; the others have no rows
  expect_match(html, "North Police"); expect_match(html, "<td>P2</td>", fixed = TRUE); expect_match(html, "90%")
  expect_lt(regexpr("North Police", html), regexpr("<td>P2</td>", html, fixed = TRUE))          # biggest first
  expect_false(grepl("police_force_area=", html, fixed = TRUE))                                  # not written as an HTML attribute
  expect_equal(as.character(ui$attribs$class), "responder-grid"); expect_length(ui$children, 3L)
  expect_null(responder_tables_ui(g[0, ]))
  many <- data.frame(geography = "region", code = sprintf("R%d", 1:20), name = NA, people = 20:1, share_of_area_people = 0.05, people_rs_high_medium = 0)
  expect_match(as.character(responder_tables_ui(many)), "The first 12 of 20")
})


test_that("area issue summary and monthly counts: totals, severe, in force now, stale episodes, empty months kept", {
  now <- as.POSIXct("2026-10-28 12:00", tz = "UTC")
  ep <- data.frame(start_time = as.POSIXct(c("2026-10-27 09:00", "2026-03-02 10:00", "2025-01-10 08:00", "2026-03-20 06:00"), tz = "UTC"),
                   end_time = as.POSIXct(c(NA, "2026-03-03 10:00", "2025-01-11 08:00", NA), tz = "UTC"),
                   duration_minutes = c(NA, 1440, 1440, NA), ea_area_type = c("flood_warning", "flood_alert", "flood_warning", "flood_alert"),
                   had_severe = c(FALSE, FALSE, TRUE, FALSE), stale_open = c(FALSE, FALSE, FALSE, TRUE))
  s <- area_issue_summary(ep, now)
  expect_equal(s$total, 4L); expect_equal(s$last12, 3L); expect_equal(s$severe, 1L); expect_equal(s$median_minutes, 1440)
  expect_equal(format(s$in_force_since, "%Y-%m-%d %H:%M"), "2026-10-27 09:00")             # the stale one (still open after 30 days) is not "in force now"
  expect_equal(format(s$latest, "%Y-%m-%d"), "2026-10-27")
  expect_equal(area_issue_summary(NULL, now)$total, 0L); expect_true(is.na(area_issue_summary(ep[0, ], now)$in_force_since))
  m <- issue_counts_by_month(ep, now, 12L)
  expect_equal(length(unique(m$month)), 12L); expect_equal(nrow(m), 36L)
  expect_equal(m[m$month == as.Date("2026-10-01") & m$level == "Flood warning", "n"][[1L]], 1L)
  expect_equal(m[m$month == as.Date("2026-03-01") & m$level == "Flood alert", "n"][[1L]], 2L)
  expect_equal(sum(m$n), 3L)                                                                   # the January 2025 episode is outside the 12 months
  expect_equal(sum(issue_counts_by_month(NULL, now, 12L)$n), 0L)
})


test_that("the threshold board picks the lowest Response threshold, ranks at or above then close and rising, and drops stale readings", {
  now <- as.POSIXct("2026-10-28 12:00", tz = "UTC")
  raw <- data.frame(
    measure_uri = c("m1", "m1", "m2", "m3", "m4", "m5", "m6"), target_area = c("A1", "A1", "A2", "A3", "A4", "A5", "A6"),
    type = c("RES", "ACTCON", "RES", "RES", "ACTCON", "RES", "RES"), label_prefix = "RES FW",
    value_in_measure_unit = c(3.0, 2.0, 2.5, 5.0, 4.0, 2.0, 2.0),
    last_value = c(3.1, 3.1, 2.3, 4.9, 3.7, 0.9, 2.1), last_time = as.POSIXct(c("2026-10-28 11:50", "2026-10-28 11:50", "2026-10-28 11:40", "2026-10-28 11:45", "2026-10-28 11:30", "2026-10-28 11:30", "2026-10-28 06:00"), tz = "UTC"),
    value_3h_ago = c(3.0, 3.0, 2.0, 4.6, 3.7, 0.9, 2.0), gauge_station = "g", gauge_river = "r", stringsAsFactors = FALSE)
  ex <- data.frame(area_id = c("A1", "A2", "A3", "A4", "A5"), area_name = c("One", "Two", "Three", "Four", "Five"), people = 100, people_rs_high_medium = c(10, 50, 5, 30, 1))
  r <- threshold_board_rows(raw, ex, now, within_m = 1)
  expect_equal(r$area_id, c("A1", "A3", "A2", "A4"))                               # A5 is 1.1 m below: further; A6's reading is stale
  expect_equal(r$status, c("At or above", "Close and rising", "Close and rising", "Close"))
  expect_equal(r$threshold[r$area_id == "A1"], 3.0)                                  # RES (3.0) is used, not the lower ACTCON (2.0)
  expect_equal(r$threshold[r$area_id == "A4"], 4.0)                                  # no RES threshold: the lowest of any type
  a2 <- r[r$area_id == "A2", ]; expect_equal(a2$headroom, 0.2, tolerance = 1e-9); expect_equal(a2$rate, 0.1, tolerance = 1e-9); expect_equal(a2$eta_hours, 2, tolerance = 1e-9)
  expect_equal(r$area_name[1L], "One"); expect_equal(r$people_at_risk[2L], 5)
  expect_true(r$eta_hours[2L] < r$eta_hours[3L])                                      # sooner first among close and rising
  expect_equal(threshold_board_rows(raw, ex, now, within_m = 1, rising_only = TRUE)$area_id, c("A1", "A3", "A2"))
  expect_equal(nrow(threshold_board_rows(raw[0, ], ex, now)), 0L); expect_equal(nrow(threshold_board_rows(NULL, ex, now)), 0L)
  expect_equal(trend_label(c(0.12, -0.2, 0.001, NA)), c("\u2191 +0.12 m/h", "\u2193 -0.20 m/h", "steady", "-"))
})

test_that("constituency flags: areas in force and potential ones are those linked by the overlap rule, severest first, and a potential area already in force is not listed twice", {
  links <- data.frame(ea_area_code = c("W1", "W2", "A1", "A2", "W3", "A9"), constituency_id = c("C1", "C1", "C1", "C1", "C1", "C2"),
                      share_of_area_in_constituency = c(0.5, 0.4, 0.3, 0.0001, 0.2, 1), stringsAsFactors = FALSE)
  f <- data.frame(flood_area_id = c("W1", "A1", "A9"), severity_level = c(2L, 3L, 3L), description = c("River One", "Alert One", "Elsewhere"),
                  time_raised = as.POSIXct(c("2026-10-28 09:00", "2026-10-28 08:00", "2026-10-28 07:00"), tz = "UTC"), stringsAsFactors = FALSE)
  pot <- data.frame(ea_area_code = c("W1", "W2", "W3", "A2", "A9"), ea_area_name = c("River One", "River Two", "River Three", "Alert Two", "Elsewhere"),
                    ea_area_type = c("flood_warning", "flood_warning", "flood_warning", "flood_alert", "flood_alert"), worst_rank = c(3L, 2L, 4L, 3L, 4L), first_day = c(1L, 3L, 2L, 1L, 1L), stringsAsFactors = FALSE)
  fl <- constituency_flags(f, pot, links, "C1", min_pct = 0.01)
  expect_equal(fl$in_force$area, c("River One", "Alert One")); expect_equal(fl$in_force$level, c("Flood warning", "Flood alert"))
  expect_equal(fl$potential$area, c("River Three", "River Two"))           # W1 is in force so not potential; A2 overlaps less than 1 per cent; highest risk first
  expect_equal(fl$potential$risk, c("High", "Low")); expect_equal(fl$potential$first_day, c(2L, 3L))
  none <- constituency_flags(NULL, NULL, links, "C1"); expect_equal(nrow(none$in_force), 0L); expect_equal(nrow(none$potential), 0L)
  expect_equal(nrow(constituency_flags(f, pot, NULL, "C1")$in_force), 0L)
})

test_that("the constituency brief content and PDF: figures, MP, the in force and potential tables, a five day forecast, and one page", {
  cc <- data.frame(constituency_code = "E14001", constituency_name = "York Central", people = 100000, people_rs_high_medium = 8000, people_fz3 = 3000,
                   people_high_medium_not_in_warning_area = 2500, people_high_medium_not_in_alert_area = 1000, sites = 12, stringsAsFactors = FALSE)
  sit <- data.frame(mp_name = "A. Member", mp_party = "Party", fgs_issued_at = "2026-10-28 10:30:00", fgs_rank_day1 = 1L, fgs_rank_day2 = 2L, fgs_rank_day3 = 3L, fgs_rank_day4 = NA_integer_, fgs_rank_day5 = 0L, stringsAsFactors = FALSE)
  fl <- list(in_force = data.frame(area = "River One", level = "Flood warning", since = as.POSIXct("2026-10-28 09:00", tz = "UTC"), severity = 2L, stringsAsFactors = FALSE),
             potential = data.frame(area = "River Three", type = "Flood warning", risk = "High", first_day = 2L, rank = 4L, stringsAsFactors = FALSE))
  sites <- data.frame(name = c("Rowan Court", "Far"), site_type = c("Care home", "School"), band = c("High", "None"), rofrs_rank = c(4L, 0L))
  hist <- list(total = 14L, last12 = 5L, severe = 1L, median_minutes = 600, latest = Sys.time(), in_force_since = as.POSIXct(NA, tz = "UTC"))
  k <- constituency_brief_content(cc, sit, sites, fl, hist, "statement 4936 issued 2026-10-28 10:30", "Low", generated = as.POSIXct("2026-10-28 12:00", tz = "UTC"))
  expect_equal(k$title, "York Central"); expect_match(k$subtitle, "MP: A. Member (Party)", fixed = TRUE); expect_equal(k$kpis$value[c(1, 2, 3, 4, 5)], c("100,000", "8,000", "2,500", "12", "5"))
  expect_equal(k$forecast$risk, c("Very Low", "Low", "Medium", "None", "None")); expect_equal(k$sites$name, "Rowan Court"); expect_match(k$history, "5 alerts or warnings were issued")
  f <- tempfile(fileext = ".pdf"); on.exit(unlink(f), add = TRUE)
  constituency_brief_pdf(f, k)
  expect_equal(rawToChar(readBin(f, "raw", 4L)), "%PDF"); expect_gt(file.size(f), 3000)
  if (nzchar(Sys.which("pdfinfo"))) expect_equal(as.integer(sub("Pages:\\s+", "", grep("^Pages:", system2("pdfinfo", f, stdout = TRUE), value = TRUE))), 1L)
  g <- tempfile(fileext = ".pdf"); on.exit(unlink(g), add = TRUE)
  constituency_brief_pdf(g, constituency_brief_content(cc, NULL, NULL, list(in_force = fl$in_force[0, ], potential = fl$potential[0, ]), NULL))      # nothing known: still draws
  expect_gt(file.size(g), 2000)
})


test_that("the threshold board flags areas in force and potential ones, and does not call an area in force potential too", {
  now <- as.POSIXct("2026-10-28 12:00", tz = "UTC")
  raw <- data.frame(measure_uri = c("m1", "m2", "m3"), target_area = c("A1", "A2", "A3"), type = "RES", value_in_measure_unit = c(3, 3, 3), last_value = c(3.1, 2.9, 2.8),
                    last_time = as.POSIXct("2026-10-28 11:50", tz = "UTC"), value_3h_ago = c(3, 2.8, 2.8), gauge_station = "g", gauge_river = "r", stringsAsFactors = FALSE)
  f <- data.frame(flood_area_id = c("A1", "A1"), severity_level = c(3L, 2L))
  pot <- data.frame(ea_area_code = c("A1", "A2", "A2"), worst_rank = c(4L, 2L, 3L), first_day = c(1L, 3L, 2L))
  r <- threshold_board_rows(raw, NULL, now, within_m = 1, inforce = f, potential = pot)
  expect_equal(r$in_force[r$area_id == "A1"], "Flood warning")                         # the severest of the two
  expect_equal(r$potential[r$area_id == "A1"], "")                                       # in force, so not also potential
  expect_equal(r$potential[r$area_id == "A2"], "Medium risk, from day 2")
  expect_equal(r$potential[r$area_id == "A3"], ""); expect_equal(r$in_force[r$area_id == "A3"], "")
  plain <- threshold_board_rows(raw, NULL, now, within_m = 1)
  expect_true(all(plain$in_force == "") && all(plain$potential == ""))
})

test_that("an HTML error page from the warehouse gateway counts as a connection error, so it is retried and not fatal", {
  expect_true(is_connection_error("Error in `httr2::req_perform()`: ! Failed to parse error body with method defined in `req_error()`."))
  expect_true(is_connection_error("Caused by error in `httr2::resp_body_json()`: ! Unexpected content type \"text/html\"."))
  expect_false(is_connection_error("[TABLE_OR_VIEW_NOT_FOUND] The table or view cannot be found"))
})

test_that("the overlap queries read the columns job 35 writes (area_a and area_b)", {
  src <- paste(readLines(file.path(app_root, "data.R"), warn = FALSE), collapse = "\n")
  expect_false(grepl("o.area_id_a|o.area_id_b", src))
  expect_true(grepl("o.area_a AS area_id_a", src, fixed = TRUE) && grepl("area_a AS area_id_a, area_b AS area_id_b", src, fixed = TRUE))
})

test_that("plot styling puts height on the widget and does not use the deprecated layout height", {
  skip_if_not_installed("plotly")
  fig <- plotly::plot_ly(x = 1:3, y = 1:3, type = "scatter", mode = "lines")
  expect_no_warning(styled <- app_plot_style(fig, height = 240, margin = list(t = 10)))
  expect_equal(styled$height, 240)
})


test_that("an area's gauge rows keep every threshold, give headroom and status, and mark stale readings; gauge links are safe", {
  now <- as.POSIXct("2026-10-28 12:00", tz = "UTC")
  raw <- data.frame(measure_uri = c("m1", "m1", "m2", "m3"), target_area = "A1", type = c("ACTCON", "RES", "RES", "RES"), label_prefix = c("ACT", "RES FW", "RES FW", "RES FW"),
                    value_in_measure_unit = c(2.0, 3.0, 5.0, 1.0), last_value = c(2.6, 2.6, 3.0, 0.5),
                    last_time = as.POSIXct(c("2026-10-28 11:50", "2026-10-28 11:50", "2026-10-28 11:40", "2026-10-28 06:00"), tz = "UTC"),
                    value_3h_ago = c(2.3, 2.3, 3.0, 0.5), gauge_station = c("Alpha", "Alpha", "Beta", "Gamma"), gauge_river = "R", stringsAsFactors = FALSE)
  r <- area_gauge_rows(raw, now)
  expect_equal(nrow(r), 4L); expect_equal(r$gauge, c("Alpha", "Alpha", "Beta", "Gamma"))
  expect_equal(r$threshold[r$gauge == "Alpha"], c(2.0, 3.0))                       # the whole ladder, lowest first
  expect_equal(r$status[r$gauge == "Alpha"], c("At or above", "Close and rising"))
  expect_equal(r$headroom[r$gauge == "Alpha"], c(-0.6, 0.4), tolerance = 1e-9)
  expect_equal(r$status[r$gauge == "Beta"], "Further"); expect_equal(r$status[r$gauge == "Gamma"], "No recent reading"); expect_true(r$stale[r$gauge == "Gamma"])
  expect_equal(nrow(area_gauge_rows(NULL, now)), 0L); expect_equal(nrow(area_gauge_rows(raw[0, ], now)), 0L)
  l <- gauge_open_link("http://x/measures/abc-1'\"<script>", "A<b>")
  expect_match(l, "Shiny.setInputValue('goto_gauge'", fixed = TRUE); expect_false(grepl("<script", l, fixed = TRUE)); expect_false(grepl("A<b>", l, fixed = TRUE))
})

test_that("every fetch_ function the app calls is defined in data.R, so a merge cannot silently drop one and leave only the demo working", {
  code <- function(f) { l <- readLines(file.path(app_root, f), warn = FALSE); l <- sub("#.*$", "", l); paste(l, collapse = "\n") }
  used <- unique(unlist(regmatches(code("app_server.R"), gregexpr("\\bfetch_[a-z_0-9]+\\b", code("app_server.R")))))
  defined <- unique(sub("\\s*<-.*$", "", grep("^fetch_[a-z_0-9]+\\s*<-\\s*function", readLines(file.path(app_root, "data.R"), warn = FALSE), value = TRUE)))
  expect_equal(setdiff(used, defined), character())
})

test_that("an area link keeps its choice: the area picker is filled once, and the link passes the choices with the selection", {
  srv <- paste(readLines(file.path(app_root, "app_server.R"), warn = FALSE), collapse = "\n")
  expect_true(grepl("if (!isolate(area_picker_ready()))", srv, fixed = TRUE))                       # the tab-change observer cannot reset the selection
  expect_equal(lengths(regmatches(srv, gregexpr('updateSelectizeInput(session, "area_pick"', srv, fixed = TRUE))), 4L)
  picks <- regmatches(srv, gregexpr("updateSelectizeInput\\(session, \"area_pick\"[^\n]*", srv))[[1L]]
  expect_true(all(grepl("server = TRUE", picks, fixed = TRUE)) && all(grepl("area_choices_of", picks, fixed = TRUE)))        # a server-side picker ignores a selection sent without its choices
})


test_that("a copied Target Area link restores the chosen area: it is saved with the view, and applied once the picker and the saved value are both there", {
  srv <- paste(readLines(file.path(app_root, "app_server.R"), warn = FALSE), collapse = "\n")
  expect_true(grepl("state$values$area_pick <- isolate(input$area_pick)", srv, fixed = TRUE))            # saved when the link is made
  expect_true(grepl("onRestored(function(state) restored_area(state$values$area_pick))", srv, fixed = TRUE))
  expect_true(grepl("observeEvent(list(restored_area(), area_picker_ready())", srv, fixed = TRUE))       # applied whichever of the two arrives last
})

test_that("site_display_name names blank sites by their type", {
  expect_equal(site_display_name(c("Alford STW", "", NA), c("sewage works", "fire station", NA)),
               c("Alford STW", "Unnamed fire station", "Unnamed site"))
})

test_that("forecast_people_rows gives a floor and a ceiling for the people in the areas under a forecast", {
  ex <- data.table(area_id = c("W1", "W2", "W3", "A1"), kind = c("flood_warning", "flood_warning", "flood_warning", "flood_alert"),
                   people = c(100, 200, 900, 400), people_split = c(80, 150, 900, 300), people_rs_high_medium = c(10, 20, 90, 40))
  pot <- data.frame(ea_area_code = c("W1", "W2", "A1", "NOT_IN_TABLE"))
  r <- forecast_people_rows(ex, pot)
  expect_equal(r$Kind, c("Flood warning areas", "Flood alert areas"))
  expect_equal(r$areas, c(2L, 1L)); expect_equal(r$people_low, c(230, 300)); expect_equal(r$people_high, c(300, 400)); expect_equal(r$hm_high, c(30, 40))
  expect_true(all(r$people_low <= r$people_high))
  expect_equal(nrow(forecast_people_rows(ex, data.frame(ea_area_code = "none"))), 0L)
})

test_that("rapid flood guidance: the current update, the default view, and the flags", {
  u <- data.table(update_key = c("9", "none_a", "7"), is_present = c(FALSE, FALSE, TRUE))
  expect_null(rfg_current(u))                               # the newest row is a no-guidance state
  expect_equal(rfg_default_key(u), "7")                     # the view falls back to the newest update that had guidance
  u2 <- data.table(update_key = c("10", "9"), is_present = c(TRUE, TRUE))
  expect_equal(as.character(rfg_current(u2)$update_key), "10"); expect_equal(rfg_default_key(u2), "10")
  expect_null(rfg_current(NULL)); expect_true(is.na(rfg_default_key(NULL))); expect_true(is.na(rfg_default_key(u[0])))
  expect_true(is.na(rfg_default_key(data.table(update_key = "x", is_present = FALSE))))

  areas <- data.table(area_id = c("A", "A", "B"), people = c(300, 500, 40), polygon_key = c("10|1", "10|2", "10|1"))
  f <- rfg_area_flag(areas, "A", 600)
  expect_equal(f$people, 600); expect_equal(f$polygons, 2L); expect_equal(f$share, 1)           # capped at the area's own people
  expect_equal(rfg_area_flag(areas, "B")$people, 40); expect_true(is.na(rfg_area_flag(areas, "B")$share))
  expect_null(rfg_area_flag(areas, "Z", 100)); expect_null(rfg_area_flag(NULL, "A", 100))
  cons <- data.table(pcon = c("E1", "E2"), people = c(1200, 0), polygon_key = c("10|1", "10|1"))
  expect_equal(rfg_constituency_flag(cons, "E1", 5000)$share, 0.24); expect_null(rfg_constituency_flag(cons, "E2", 5000))
  expect_match(rfg_flag_text(f, "area"), "600 people of this area \\(100%\\) lie inside 2 guidance polygons")
  expect_match(rfg_flag_text(rfg_constituency_flag(cons, "E1"), "constituency"), "1,200 people of this constituency lie inside 1 guidance polygon\\.")
  expect_null(rfg_flag_text(NULL))
})

test_that("area_history_summary counts the issues and the time in force by area, inside the period", {
  h <- data.table(ea_area_code = c("A", "A", "A", "B"), ea_area_name = c("Area A", "Area A", "Area A", NA), ea_area_type = c("flood_warning", "flood_warning", "flood_warning", "flood_alert"),
                  issued_date = as.Date(c("2025-01-01", "2025-06-01", "2023-01-01", "2025-02-01")), n_issued = c(2L, 1L, 4L, 1L), n_severe = c(1L, 0L, 0L, 0L),
                  minutes_timed = c(300, 60, 999, 0), n_timed = c(2L, 1L, 4L, 0L), longest_minutes = c(200, 60, 500, NA), n_open = c(0L, 0L, 0L, 1L))
  s <- area_history_summary(h, "2024-01-01", "2025-12-31")
  expect_equal(s$ea_area_code, c("A", "B"))                                  # most issued first
  a <- s[ea_area_code == "A"]
  expect_equal(a$issued, 3L); expect_equal(a$severe, 1L); expect_equal(a$total_minutes, 360); expect_equal(a$mean_minutes, 120)
  expect_equal(a$longest_minutes, 200); expect_equal(a$last_issued, as.Date("2025-06-01"))        # the 2023 issues are outside the period
  b <- s[ea_area_code == "B"]
  expect_equal(b$ea_area_name, "B"); expect_equal(b$open, 1L); expect_true(is.na(b$mean_minutes)); expect_true(is.na(b$longest_minutes))   # an open issue has no time
  expect_equal(nrow(area_history_summary(h, "2030-01-01", "2030-12-31")), 0L); expect_equal(nrow(area_history_summary(NULL)), 0L)
  expect_equal(fmt_span(c(NA, 45, 200, 3000)), c("", "45 min", "3 h 20 min", "2 d 2 h"))
})

# ---- economics ------------------------------------------------------------------------------------------------------------
econ_assump <- function(damage = c(10000, 30000, 60000)) {
  p <- list(high = c(0.033, 0.05, 0.10), medium = c(0.01, 0.0182, 0.033), low = c(0.001, 0.00316, 0.01), very_low = c(0.0001, 0.000316, 0.001))
  rows <- do.call(rbind, lapply(names(p), function(b) data.frame(item = paste0("prob_", b), scenario = c("low", "central", "high"), value = p[[b]], source = NA, price_year = NA)))
  rbind(rows, data.frame(item = "damage_per_household", scenario = c("low", "central", "high"), value = damage, source = "MCM test", price_year = "2024"))
}

test_that("econ_params reads the table, and anything missing stays NA", {
  p <- econ_params(econ_assump())
  expect_true(p$has_prob && p$has_damage); expect_equal(p$prob["high", "central"], 0.05); expect_equal(p$damage[["high"]], 60000); expect_equal(p$source, "MCM test")
  q <- econ_params(econ_assump(damage = c(NA, NA, NA)))
  expect_true(q$has_prob); expect_false(q$has_damage)
  r <- econ_params(NULL); expect_false(r$has_prob); expect_false(r$has_damage)
  expect_false(econ_params(econ_assump()[-1, ])$has_prob)                      # one probability missing
})

test_that("expected annual damage is households x band probability x damage, in three scenarios that vary together", {
  ex <- data.table(area_id = c("A", "B"), households_rs_high = c(100, 0), households_rs_medium = c(200, 0), households_rs_low = c(0, 1000), households_rs_very_low = c(0, 0))
  e <- econ_area(ex, econ_params(econ_assump()))
  expect_equal(e$ehh_central[1], 100 * 0.05 + 200 * 0.0182)
  expect_equal(e$ead_central[1], (100 * 0.05 + 200 * 0.0182) * 30000)
  expect_equal(e$ead_low[1], (100 * 0.033 + 200 * 0.01) * 10000); expect_equal(e$ead_high[1], (100 * 0.10 + 200 * 0.033) * 60000)
  expect_true(all(e$ead_low <= e$ead_central & e$ead_central <= e$ead_high))
  n <- econ_area(ex, econ_params(econ_assump(damage = c(NA, NA, NA))))           # no damage figures: expected households, no pounds
  expect_true(all(is.na(n$ead_central))); expect_equal(n$ehh_central, e$ehh_central)
  expect_true(all(is.na(econ_area(ex, econ_params(NULL))$ehh_central)))
  expect_equal(nrow(econ_area(NULL, econ_params(econ_assump()))), 0L); expect_equal(nrow(econ_area(data.table(area_id = "A"), econ_params(econ_assump()))), 0L)
})

test_that("a gauge's value is gross (every area it drives) and split (an area shared between its gauges), and the split adds up", {
  ex <- data.table(area_id = c("A", "B", "C"), people = c(500, 300, 100), households_rs_high = c(100, 40, 0), households_rs_medium = 0, households_rs_low = 0, households_rs_very_low = 0)
  ea <- econ_area(ex, econ_params(econ_assump()))
  links <- data.table(area_id = c("A", "A", "B", "C"), measure_uri = c("g1", "g2", "g1", "g3"), gauge = c("One", "Two", "One", "Three"), river = "R")
  gv <- econ_gauge_value(links, ea, ex)
  g1 <- gv[measure_uri == "g1"]; g2 <- gv[measure_uri == "g2"]
  a <- ea[area_id == "A"]$ead_central; b <- ea[area_id == "B"]$ead_central
  expect_equal(g1$gross_central, a + b); expect_equal(g1$split_central, a / 2 + b)
  expect_equal(g2$gross_central, a); expect_equal(g2$split_central, a / 2)
  expect_equal(g1$areas, 2L); expect_equal(g1$sole_areas, 1L); expect_equal(g2$sole_areas, 0L); expect_equal(g1$people, 800)
  expect_equal(sum(gv$split_central), sum(ea$ead_central))                           # the split figures add up to the total
  expect_equal(gv$measure_uri[1], "g1")                                              # ranked by split, largest first
  expect_equal(nrow(econ_gauge_value(NULL, ea, ex)), 0L)
  none <- econ_gauge_value(links, econ_area(ex, econ_params(econ_assump(damage = c(NA, NA, NA)))), ex)
  expect_true(all(is.na(none$split_central))); expect_equal(none$measure_uri[1], "g1")   # ranked by expected households when there are no pounds
})

test_that("pounds are written short, and the range text falls back when the damage is not set", {
  expect_equal(fmt_gbp(c(NA, 850, 12300, 250000, 1260000)), c("", "£850", "£12.3k", "£250k", "£1.3m"))
  expect_equal(econ_range_text(4e5, 1.2e6, 3.1e6), "£1.2m (range £400k to £3.1m)")
  expect_match(econ_range_text(NA, NA, NA, 3.4), "3.4 households a year \\(no pound figure yet\\)"); expect_equal(econ_range_text(NA, NA, NA, NA), "")
  expect_match(econ_basis_text(econ_params(econ_assump())), "Indicative"); expect_match(econ_basis_text(econ_params(NULL)), "not available")
})

test_that("the area brief carries the indicative damage sentence when it is given", {
  k <- area_brief_content(brief_area(), NULL, NULL, NA_character_, econ = "Indicative expected annual damage: £1.2m.")
  expect_match(k$summary, "Indicative expected annual damage: £1.2m\\.")
  expect_false(grepl("Indicative expected", area_brief_content(brief_area())$summary))
})

test_that("typed-in damage figures are checked, and when used every figure says so", {
  expect_true(econ_try_validate(10000, 30000, 60000)$ok)
  expect_false(econ_try_validate(10000, NA, 60000)$ok); expect_false(econ_try_validate(-1, 30000, 60000)$ok); expect_false(econ_try_validate(50000, 30000, 60000)$ok)
  expect_false(econ_try_validate("abc", 1, 2)$ok)
  p <- econ_params(econ_assump(damage = c(NA, NA, NA)))
  expect_false(p$has_damage)
  o <- econ_apply_override(p, econ_try_validate(10000, 30000, 60000)$damage)
  expect_true(o$has_damage && isTRUE(o$override)); expect_equal(o$damage[["central"]], 30000)
  expect_match(econ_basis_text(o), "^YOUR OWN DAMAGE FIGURES, NOT SAVED OR AGREED"); expect_match(econ_basis_text(o), "typed in for this session")
  expect_no_match(econ_basis_text(econ_params(econ_assump())), "YOUR OWN")
  ex <- data.table(area_id = "A", households_rs_high = 100, households_rs_medium = 0, households_rs_low = 0, households_rs_very_low = 0)
  expect_equal(econ_area(ex, o)$ead_central, 100 * 0.05 * 30000)
  expect_identical(econ_apply_override(econ_params(NULL), c(low = 1, central = 2, high = 3))$has_damage, FALSE)   # no probabilities: nothing to apply to
})

test_that("provisional damage figures fill in only when none are set, and say so", {
  none <- econ_params(econ_assump(damage = c(NA, NA, NA)))
  p <- econ_with_provisional(none)
  expect_true(p$has_damage && isTRUE(p$provisional)); expect_equal(p$damage[["central"]], 30000)
  expect_match(econ_basis_text(p), "^PROVISIONAL DAMAGE FIGURES, NOT AGREED OR SOURCED"); expect_match(econ_basis_text(p), "not from the Multi-Coloured Handbook")
  set <- econ_with_provisional(econ_params(econ_assump()))                                  # figures in the table are used as they are
  expect_false(isTRUE(set$provisional)); expect_equal(set$damage[["central"]], 30000)
  expect_no_match(econ_basis_text(set), "PROVISIONAL")
  expect_false(isTRUE(econ_with_provisional(econ_params(NULL))$has_damage))               # no probabilities: nothing to apply to
  o <- econ_apply_override(p, c(low = 1, central = 2, high = 3))                          # the user's own figures replace the provisional ones, and say so
  expect_false(isTRUE(o$provisional)); expect_match(econ_basis_text(o), "^YOUR OWN")
  expect_true(all(ECON_PROVISIONAL_DAMAGE == sort(ECON_PROVISIONAL_DAMAGE)))
})
