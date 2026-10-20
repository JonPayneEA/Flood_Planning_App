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
