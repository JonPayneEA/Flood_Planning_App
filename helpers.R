# =============================================================================
# helpers.R
# Pure functions used by app_server.R. No Shiny, no database, no globals beyond
# the constants in global.R (RANK_HEX, RANK_LABELS), so each can be unit
# tested on plain data. Sourced by app.R after global.R.
# =============================================================================


# -----------------------------------------------------------------------------
# rank_fill()
#
# Fill colour for a forecast risk rank (0 none to 4 High). Anything missing
# or out of range reads as "none".
# -----------------------------------------------------------------------------

rank_fill <- function(rank) {
  r <- suppressWarnings(as.integer(rank))
  r[is.na(r) | r < 0L | r > 4L] <- 0L
  unname(RANK_HEX[r + 1L])
}


# -----------------------------------------------------------------------------
# risk_layer_popup()
#
# One popup string per row of the constituency risk layer. Every value that
# comes from the warehouse is HTML-escaped: names and MPs are free text.
# -----------------------------------------------------------------------------

risk_layer_popup <- function(shape) {
  esc <- htmltools::htmlEscape
  r   <- suppressWarnings(as.integer(shape$fgs_worst_rank))
  r[is.na(r) | r < 0L | r > 4L] <- 0L
  mp  <- ifelse(is.na(shape$mp_name), "vacant or unmatched", esc(shape$mp_name))
  warn_bits <- function(n, label) ifelse(n > 0L, paste0(n, " ", label), "")
  inforce <- paste0(warn_bits(shape$severe_warnings, "severe warning(s)"), " ",
                    warn_bits(shape$warnings,        "warning(s)"), " ",
                    warn_bits(shape$alerts,          "alert(s)"))
  inforce <- trimws(gsub("\\s+", " ", inforce))
  paste0(
    "<strong>", esc(shape$constituency_name), "</strong><br>",
    "Forecast: ", RANK_LABELS[r + 1L],
    " (", esc(shape$fgs_worst_date), "; ", esc(shape$fgs_worst_sources), ")",
    ifelse(as.logical(shape$fgs_escalated), " <em>escalated</em>", ""), "<br>",
    "In force: ", ifelse(nzchar(inforce), inforce, "none"), "<br>",
    "MP: ", mp
  )
}


# -----------------------------------------------------------------------------
# parse_utc()
#
# ISO-8601 timestamp string (with T, optional offset or Z) to POSIXct UTC.
# Returns NA for anything unparseable, never an error.
# -----------------------------------------------------------------------------

parse_utc <- function(x) {
  x <- sub(" ", "T", substr(as.character(x), 1L, 19L), fixed = TRUE)
  x <- ifelse(nchar(x) == 10L, paste0(x, "T00:00:00"), x)       # a bare date is midnight
  as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
}

# as_utc(): a statement time from the warehouse as POSIXct, whatever form it
# arrives in. The warehouse can hand back text ("2026-10-15T09:30:00"), and
# as.POSIXct() on ISO text with a "T" silently drops the time and returns
# midnight, which is how a 09:30 statement was shown as 00:00. Always use this.
as_utc <- function(x) {
  if (inherits(x, "POSIXct")) return(x)
  if (inherits(x, "Date"))    return(as.POSIXct(format(x), tz = "UTC"))
  parse_utc(x)
}

# fmt_statement_time(): "Thu 15 Oct 09:30", or a calm "unknown time" if the
# value cannot be read. No time zone is printed: the pipeline stores the issue
# time as the FGS API gave it, and the FGS is issued at 09:30 UK time, so
# calling it UTC would be wrong in summer.
fmt_statement_time <- function(x, format = "%a %d %b %H:%M") {
  t <- as_utc(x)
  ifelse(is.na(t), "unknown time", base::format(t, format))
}


# -----------------------------------------------------------------------------
# group_statement_choices()   POC-ONLY: remove with the look-back picker.
#
# Choices for the look-back picker, older statements only. A day with several
# statements becomes a group headed "Thu 02 Oct (3 statements)", so a busy day
# reads as one block. A day with one statement stays a flat entry.
# -----------------------------------------------------------------------------
group_statement_choices <- function(older) {
  if (nrow(older) == 0L) return(list())
  t    <- as_utc(older$issued_at)
  day  <- ifelse(is.na(t), "unknown date", base::format(t, "%a %d %b"))
  lab  <- sprintf("%s (#%d)", fmt_statement_time(older$issued_at), older$statement_id)
  ids  <- as.character(older$statement_id)
  out  <- list()
  for (d in unique(day)) {
    k <- which(day == d)
    if (length(k) == 1L) out[[lab[k]]] <- ids[k]
    else out[[sprintf("%s (%d statements)", d, length(k))]] <- setNames(ids[k], lab[k])
  }
  out
}


# -----------------------------------------------------------------------------
# stale_status()
#
# Decide what the stale-data banner should say, from the pipeline_health
# table (check_name, status, value, detail, checked_at).
#
# Returns list(level, message). level is one of:
#   "ok"      nothing to show
#   "warn"    one or more checks need attention
#   "fail"    one or more checks are failing
#   "stale"   the health table itself has stopped updating, so the pipeline
#             may not be running at all (the quiet failure)
#   "unknown" the health table could not be read
# Precedence: unknown, then stale, then fail, then warn. "info" is not a
# problem and never raises a banner.
# -----------------------------------------------------------------------------

stale_status <- function(health, now = Sys.time(), stale_hours = 3) {
  if (is.null(health) || nrow(health) == 0L) {
    return(list(level = "unknown",
                message = "Pipeline health could not be read. Figures may be out of date."))
  }

  health  <- as.data.frame(health, stringsAsFactors = FALSE)
  checks  <- health[health$check_name != "overall", , drop = FALSE]
  overall <- health[health$check_name == "overall", , drop = FALSE]

  stamp <- parse_utc(if (nrow(overall) > 0L) overall$checked_at[1L] else max(health$checked_at))
  if (!is.na(stamp)) {
    age <- as.numeric(difftime(now, stamp, units = "hours"))
    if (age > stale_hours) {
      return(list(level = "stale", message = sprintf(
        "Pipeline health has not updated for %.0f hours (last at %s UTC). The pipeline may have stopped, so figures may be out of date.",
        age, format(stamp, "%Y-%m-%d %H:%M"))))
    }
  }

  failing <- checks$check_name[checks$status == "fail"]
  warning <- checks$check_name[checks$status == "warn"]
  if (length(failing) > 0L) {
    return(list(level = "fail", message = sprintf(
      "Pipeline checks failing: %s. Figures may be out of date.", paste(failing, collapse = ", "))))
  }
  if (nrow(overall) > 0L && overall$status[1L] == "fail") {
    return(list(level = "fail", message = "Pipeline reports a failure. Figures may be out of date."))
  }
  if (length(warning) > 0L) {
    return(list(level = "warn", message = sprintf(
      "Pipeline checks need attention: %s.", paste(warning, collapse = ", "))))
  }
  list(level = "ok", message = "")
}


# -----------------------------------------------------------------------------
# choose_selected_statement()
#
# Which statement the picker should show after the list is (re)loaded.
#   choices      named character vector of statement ids, newest first
#   current      what the picker shows now (NULL on a fresh session)
#   prev_newest  the newest id the session knew about before this load
#                (NULL on the first load)
#   keep_selected  FALSE forces the newest
# A user on the newest statement follows to the new newest. A user who chose an
# older one stays on it. A restored bookmark (prev_newest NULL) is kept.
# -----------------------------------------------------------------------------

choose_selected_statement <- function(choices, current, prev_newest, keep_selected = TRUE) {
  newest <- unname(choices[1L])
  if (!keep_selected || is.null(current) || !(current %in% choices)) return(newest)
  if (!is.null(prev_newest) && identical(current, prev_newest)) return(newest)
  current
}


# -----------------------------------------------------------------------------
# summarise_changes()
#
# The pipeline stores one row per constituency, forecast day and flood source.
# People talk about a constituency ("Carlisle went from Medium to High on
# Friday"), so this reduces the rows to one per constituency: its highest new
# risk, whether that was a rise or a fall, and how many other cells changed.
# Rises come first. Used by the changes strip and the Overview.
# -----------------------------------------------------------------------------

summarise_changes <- function(ch) {
  empty <- data.table::data.table(
    constituency_id = character(), constituency_name = character(), forecast_date = character(),
    source = character(), previous_rank = integer(), new_rank = integer(), up = logical(), n_cells = integer())
  if (is.null(ch) || nrow(ch) == 0L) return(empty)
  ch <- data.table::as.data.table(ch)
  per <- ch[order(-new_rank, forecast_date)][, list(
    constituency_name = constituency_name[1L],
    forecast_date     = as.character(forecast_date[1L]),
    source            = source[1L],
    previous_rank     = previous_rank[1L],
    new_rank          = new_rank[1L],
    up                = change_type[1L] %in% c("new", "escalated"),
    n_cells           = .N
  ), by = constituency_id]
  per[order(-up, -new_rank, constituency_name)]
}


# -----------------------------------------------------------------------------
# no_change_message()
#
# Said when the changes table holds no rows for a statement. The pipeline stores
# differences only, so no rows means every constituency, day and source holds
# the same risk as in the statement before. A calm "nothing moved" is a result,
# so it names the statement it was compared with. `recent` is the statement
# list, newest first. With no earlier statement in it, nothing was compared.
# `log` (fetch_changes_log) says whether job 18 really made the comparison.
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# headline_counts_message()
#
# The day-by-day comparison only covers days both statements share, so a forecast
# that sits on a day that has rolled off shows no change there. "30 constituencies
# at Very Low yesterday, none today" is still a real change in the forecast, so job
# 18 also logs how many constituencies each statement flags over all five of its own
# days. This turns that log row into one line. NULL when the log, or those columns
# (an older pipeline), are not there. `n_changes` is the number of day-by-day
# changes found: when it is zero and the counts differ, the line says why.
# -----------------------------------------------------------------------------
headline_counts_message <- function(log, n_changes = NA_integer_) {
  need <- c("flagged_previous", "flagged_new", "medium_plus_previous", "medium_plus_new")
  if (is.null(log) || nrow(log) == 0L || !all(c(need, "previous_statement_id") %in% names(log))) return(NULL)
  lg <- log[1L]
  v  <- vapply(need, function(n) suppressWarnings(as.integer(lg[[n]])), integer(1))
  if (anyNA(v)) return(NULL)
  num  <- function(x) if (x == 0L) "none" else as.character(x)
  text <- sprintf("Constituencies with a forecast risk: %s in statement #%s, %s in this one. At Medium or above: %s and %s.",
                  num(v[["flagged_previous"]]), lg$previous_statement_id, num(v[["flagged_new"]]),
                  num(v[["medium_plus_previous"]]), num(v[["medium_plus_new"]]))
  differs <- v[["flagged_previous"]] != v[["flagged_new"]] || v[["medium_plus_previous"]] != v[["medium_plus_new"]]
  if (isTRUE(n_changes == 0L) && differs)
    text <- paste(text, "The difference is on days this statement no longer covers, so no individual change is listed.")
  text
}


no_change_message <- function(recent, statement_id, log = NULL) {
  none <- "No earlier statement to compare with, so no changes are recorded."
  if (is.null(recent) || nrow(recent) == 0L || is.null(statement_id)) return(none)
  pos <- match(as.character(statement_id), as.character(recent$statement_id))
  if (is.na(pos) || pos >= nrow(recent)) return(none)
  prev <- recent[pos + 1L]
  # `log` is job 18's record of the comparison: NULL when that table is not
  # there to ask, no rows when 18 has not compared this statement yet.
  if (!is.null(log)) {
    if (nrow(log) == 0L)
      return(sprintf("Not yet compared with statement #%s. Notebook 18 has not run for this statement.", prev$statement_id))
    lg <- log[1L]
    if (identical(as.character(lg$status), "no_shared_days"))
      return(sprintf("Not compared: this statement and #%s share no forecast days.", lg$previous_statement_id))
    days <- if (!is.na(lg$days_compared_from) && !is.na(lg$days_compared_to))
      sprintf(", %s to %s", format(as.Date(lg$days_compared_from), "%a %d"), format(as.Date(lg$days_compared_to), "%a %d")) else ""
    return(sprintf("No change since statement #%s (issued %s%s). Every constituency, day and source holds the same risk.",
                   lg$previous_statement_id, fmt_statement_time(lg$previous_issued_at), days))
  }
  sprintf("No change since statement #%s (issued %s). Every constituency, day and source holds the same risk.",
          prev$statement_id, fmt_statement_time(prev$issued_at))
}


# -----------------------------------------------------------------------------
# with_layer_guard()
#
# Runs one map layer's drawing code so that its "busy" flag is ALWAYS cleared,
# whichever way it ends: normally, by an early return(), by req() halting
# silently, or by an error. Before this, each layer cleared its flag on its last
# line, so any error in the query or the geometry parse left the spinner on the
# map forever, and the reason only reached the R console. Now the flag clears
# and a real error is shown to the user.
#
#   label   the layer's name, for the message
#   busy    the layer's reactiveVal flag
#   expr    the drawing code (return() inside it returns from the caller)
#   notify  how to tell the user; a function of one string
# -----------------------------------------------------------------------------

with_layer_guard <- function(label, busy, expr, notify = function(msg) invisible(NULL)) {
  tryCatch(
    expr,
    error = function(e) {
      if (inherits(e, "shiny.silent.error")) return(invisible(NULL))   # req(): not a failure
      msg <- sprintf("The %s layer could not be drawn: %s", label, conditionMessage(e))
      message(sprintf("[%s] %s", format(Sys.time(), "%H:%M:%S"), msg))
      notify(msg)
      invisible(NULL)
    },
    finally = busy(FALSE)
  )
}


# -----------------------------------------------------------------------------
# notification_id()
#
# A pop-up with an id REPLACES an earlier pop-up with the same id, where one without an id
# stacks. A layer that fails on every filter change used to pile up a new red box each time.
# The id comes from the message's opening words, so the same failure shows once and a
# different failure still gets its own box.
# -----------------------------------------------------------------------------

notification_id <- function(msg, prefix = "note_") {
  paste0(prefix, substr(gsub("[^A-Za-z]", "", as.character(msg)[1L]), 1L, 30L))
}


# -----------------------------------------------------------------------------
# in_nations()
#
# Which constituencies are in the chosen nations. A constituency's ID is its ONS code, and its
# first three characters give the nation (E14 England, W07 Wales, S14 Scotland, N06 Northern
# Ireland). An ID that starts with none of the known nation codes (the demonstration's, or
# anything unexpected) is always kept, so such data is never hidden by the filter. No chosen
# nation means all of them.
# -----------------------------------------------------------------------------

in_nations <- function(ids, codes, known = c("E14", "W07", "S14", "N06")) {
  ids <- as.character(ids)
  if (length(codes) == 0L) return(rep(TRUE, length(ids)))
  prefix <- substr(ids, 1L, 3L)
  !(prefix %in% known) | prefix %in% codes
}


# -----------------------------------------------------------------------------
# Warning history by constituency
#
# h is the output of fetch_warning_history(): one row per constituency, area and
# day of issue, with n_issued (alerts or warnings switched on that day) and
# n_severe (how many of those went on to severe). An "issue" is one episode: a
# run of an area being in force, whatever level it reached. A warning that goes
# severe and back is one issue, counted once in Warnings and once in Severe, so
# Severe is a subset of Warnings and the total is alerts plus warnings.
# -----------------------------------------------------------------------------

warning_history_window <- function(h, from, to) {
  if (is.null(h) || nrow(h) == 0L) return(h)
  h[as.Date(issued_date) >= as.Date(from) & as.Date(issued_date) <= as.Date(to)]
}

warning_constituency_counts <- function(h) {
  empty <- data.table(constituency_id = character(), alerts = integer(), warnings = integer(),
                      severe = integer(), total = integer())
  if (is.null(h) || nrow(h) == 0L) return(empty)
  h[, .(alerts   = sum(as.integer(n_issued)[ea_area_type == "flood_alert"]),
        warnings = sum(as.integer(n_issued)[ea_area_type == "flood_warning"]),
        severe   = sum(as.integer(n_severe)[ea_area_type == "flood_warning"])),
    by = constituency_id][, total := alerts + warnings][order(-total, constituency_id)]
}

warning_area_counts <- function(h, constituency_id) {
  empty <- data.table(ea_area_code = character(), ea_area_name = character(), ea_area_type = character(),
                      issued = integer(), severe = integer(), last_issued = as.Date(character()))
  if (is.null(h) || nrow(h) == 0L) return(empty)
  cid <- constituency_id
  x <- h[constituency_id == cid]
  if (nrow(x) == 0L) return(empty)
  x[, .(ea_area_name = ea_area_name[1L], issued = sum(as.integer(n_issued)),
        severe = sum(as.integer(n_severe)), last_issued = max(as.Date(issued_date))),
    by = .(ea_area_code, ea_area_type)][order(-issued, ea_area_code),
    .(ea_area_code, ea_area_name, ea_area_type, issued, severe, last_issued)]
}


# -----------------------------------------------------------------------------
# Forecast against warnings (fgs_forecast_verification, job 26)
#
# One row per constituency, forecast date and lead day (lead 1 is the statement issued on the
# day, lead 5 the one issued four days before) where something was forecast or in force:
# forecast_rank 0 to 4, and the areas with an alert, a warning and a severe warning in force.
# Sparse: a date with nothing forecast and nothing in force has no row.
#
# For a chosen forecast threshold and a chosen outcome:
#   hit          forecast at or above the threshold, and it happened
#   false alarm  forecast at or above the threshold, and it did not
#   miss         below the threshold (or not forecast), and it happened
# Correct quiet forecasts are not rows, so they cannot be counted and are not claimed.
# -----------------------------------------------------------------------------

VER_OUTCOMES <- c("Alert or warning" = "any", "Flood alert" = "alerts", "Flood warning" = "warnings",
                  "Severe flood warning" = "severe")

# SQL for "it happened" under the chosen outcome. Only the four known outcomes can reach the query.
verification_happened_sql <- function(outcome) {
  switch(match.arg(outcome, unname(VER_OUTCOMES)),
         any = "(alerts + warnings) > 0", alerts = "alerts > 0", warnings = "warnings > 0", severe = "severe > 0")
}

verification_happened <- function(v, outcome) {
  switch(match.arg(outcome, unname(VER_OUTCOMES)),
         any = (v$alerts + v$warnings) > 0, alerts = v$alerts > 0, warnings = v$warnings > 0, severe = v$severe > 0)
}

# The same counts the warehouse query gives, in R: used by the demo and by the tests, which hold the
# query to the same answer. by is "lead_day" or "constituency_id"; lead limits to one lead day.
verification_counts <- function(v, min_rank, outcome, from, to, nations = NULL, by = "lead_day", lead = NULL) {
  x <- v[as.Date(forecast_date) >= as.Date(from) & as.Date(forecast_date) <= as.Date(to)]
  if (length(nations)) x <- x[in_nations(constituency_id, nations)]
  if (!is.null(lead)) x <- x[lead_day == as.integer(lead)]
  empty <- data.table(grp = if (by == "lead_day") integer() else character(),
                      hits = integer(), false_alarms = integer(), misses = integer())
  setnames(empty, "grp", by)
  if (nrow(x) == 0L) return(empty)
  hit <- verification_happened(x, outcome)
  fc  <- x$forecast_rank >= as.integer(min_rank)
  x[, `:=`(h = fc & hit, f = fc & !hit, m = !fc & hit)]
  out <- x[, .(hits = sum(h), false_alarms = sum(f), misses = sum(m)), by = c(by)]
  out[, c("hits", "false_alarms", "misses") := lapply(.SD, as.integer), .SDcols = c("hits", "false_alarms", "misses")]
  setorderv(out, by)
  out[hits + false_alarms + misses > 0L]
}

# Adds the days forecast, the days it happened, the hit rate (of the days it happened, how many
# were forecast) and the precision (of the days forecast, how many happened). NA where there is nothing to divide.
verification_scores <- function(x) {
  x <- copy(x)
  x[, `:=`(forecast_days = hits + false_alarms, event_days = hits + misses)]
  x[, `:=`(hit_rate  = ifelse(event_days > 0L, hits / event_days, NA_real_),
           precision = ifelse(forecast_days > 0L, hits / forecast_days, NA_real_))]
  x[]
}


# -----------------------------------------------------------------------------
# Warning episodes for one constituency (the Detailed table and the concurrency graph)
#
# ep is the output of fetch_warning_episodes(): one row per alert or warning, from the
# time it was switched on to the time it ended, with end_time NA while still in force.
# Times are as the message history gives them, read as UTC.
# -----------------------------------------------------------------------------

duration_label <- function(minutes) {
  m <- suppressWarnings(as.numeric(minutes))
  vapply(m, function(x) {
    if (is.na(x)) return("")
    x <- round(x)
    d <- x %/% 1440; h <- (x %% 1440) %/% 60; mi <- x %% 60
    if (d > 0) sprintf("%d d %d h", d, h) else if (h > 0) sprintf("%d h %d min", h, mi) else sprintf("%d min", mi)
  }, character(1))
}

level_label <- function(level) {
  labels <- c(flood_alert = "Flood alert", flood_warning = "Flood warning", severe_flood_warning = "Severe flood warning")
  out <- unname(labels[as.character(level)])
  ifelse(is.na(out), as.character(level), out)
}

# One row per episode, newest first, for the Detailed table. Episodes that began in
# [from, to] are kept. An episode open for over 30 days is marked, because it most
# likely never received its "no longer in force" message.
warning_episode_rows <- function(ep, from, to) {
  if (is.null(ep) || nrow(ep) == 0L) return(data.table())
  x <- copy(ep)
  x[, start_time := as.POSIXct(start_time, tz = "UTC")]
  x <- x[as.Date(start_time) >= as.Date(from) & as.Date(start_time) <= as.Date(to)]
  if (nrow(x) == 0L) return(data.table())
  x[, end_time := as.POSIXct(end_time, tz = "UTC")]
  open <- is.na(x$end_time)
  stale <- open & !is.na(x$stale_open) & as.logical(x$stale_open)
  x[order(-as.numeric(start_time)), .(
    Area = ea_area_name, Code = ea_area_code,
    Type = ifelse(ea_area_type == "flood_alert", "Flood alert", "Flood warning"),
    Issued = format(start_time, "%d %b %Y %H:%M", tz = "UTC"),
    Ended = ifelse(is.na(end_time), ifelse(stale, "Still open (over 30 days)", "In force"), format(end_time, "%d %b %Y %H:%M", tz = "UTC")),
    Duration = ifelse(is.na(end_time), "", duration_label(duration_minutes)),
    `Highest level` = level_label(peak_level),
    Severe = ifelse(as.logical(had_severe), "Yes", ""))]
}

# Alerts and warnings in force at the same time: a step series of how many were on at
# each moment. Open episodes run to `now`, except those open over 30 days, which are
# left out because they most likely never received an end message.
concurrent_counts <- function(ep, now) {
  empty <- data.table(time = as.POSIXct(character(), tz = "UTC"), alerts = integer(), warnings = integer())
  if (is.null(ep) || nrow(ep) == 0L) return(empty)
  x <- copy(ep)
  x[, start_time := as.POSIXct(start_time, tz = "UTC")]
  x[, end_time := as.POSIXct(end_time, tz = "UTC")]
  stale <- is.na(x$end_time) & !is.na(x$stale_open) & as.logical(x$stale_open)
  x <- x[!stale]
  if (nrow(x) == 0L) return(empty)
  x[is.na(end_time), end_time := as.POSIXct(now, tz = "UTC")]
  ev <- rbind(data.table(time = x$start_time, type = x$ea_area_type, d = 1L),
              data.table(time = x$end_time,   type = x$ea_area_type, d = -1L))
  ev <- ev[, .(d = sum(d)), by = .(time, type)]
  w <- dcast(ev, time ~ type, value.var = "d", fill = 0L)
  for (col in c("flood_alert", "flood_warning")) if (!(col %in% names(w))) w[, (col) := 0L]
  setorder(w, time)
  w[, `:=`(alerts = cumsum(flood_alert), warnings = cumsum(flood_warning))]
  w[, .(time, alerts = as.integer(alerts), warnings = as.integer(warnings))]
}


# -----------------------------------------------------------------------------
# Hydrometrics: the measure register and the readings chart
#
# Port of explore/Explore_live_readings in FGS_Notebooks. The register has one row
# per measure (level, flow or rainfall at a gauge). All of this is plain data.table
# so it is tested without a warehouse; only the drawing needs plotly.
# -----------------------------------------------------------------------------

RT_ALL <- "All"
RT_MAX_MEASURES <- 8L
# How far back the chart can look. 0 means everything held for the chosen measures.
RT_WINDOWS <- c("1 day" = 1L, "3 days" = 3L, "7 days" = 7L, "14 days" = 14L, "30 days" = 30L, "90 days" = 90L,
                "6 months" = 183L, "1 year" = 365L, "2 years" = 730L, "5 years" = 1826L, "All available" = 0L)
RT_POINT_TARGET <- 1500L     # points drawn per measure; longer windows are summarised in the warehouse

# The seconds each plotted point covers. Readings are every 900 s (15 min), so a window of up to
# about 15 days is drawn as it is and anything longer is bucketed to keep the chart quick.
rt_bucket_seconds <- function(days, target = RT_POINT_TARGET, step = 900L) {
  raw <- as.numeric(days) * 86400 / target
  if (is.na(raw) || raw <= step) return(as.integer(step))
  as.integer(ceiling(raw / step) * step)
}

rt_bucket_label <- function(seconds) {
  if (seconds <= 900L) return("")
  m <- seconds / 60
  if (m < 120) sprintf("%d minute", as.integer(m)) else if (m < 1440) sprintf("%g hour", m / 60) else sprintf("%g day", round(m / 1440, 1))
}

rt_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

rt_clashes <- function(names, key) {
  d <- data.table(n = names, k = key)
  d[, clash := uniqueN(k) > 1L, by = n]
  d$clash
}

# Makes names unique per key. Where one name belongs to several keys (every rain gauge
# called "Rainfall station"), appends the first detail that helps, then the key's tail.
rt_disambiguate <- function(names, key, extras) {
  clash <- rt_clashes(names, key)
  for (extra in extras) {
    if (!any(clash)) return(names)
    add <- rt_text(extra)
    trial <- ifelse(clash & nzchar(add), paste0(names, " (", add, ")"), names)
    now <- rt_clashes(trial, key)
    if (sum(now) < sum(clash)) { names <- trial; clash <- now }
  }
  if (any(clash)) names[clash] <- paste0(names[clash], " [", sub("^.*/", "", sub("/+$", "", key[clash])), "]")
  names
}

rt_prepare_register <- function(register) {
  r <- as.data.table(copy(register))
  for (col in c("station_label", "label", "station_reference", "station_uri", "measure_uri", "parameter", "qualifier",
                "unit_name", "river_name", "ea_area_name", "period", "town", "catchment_name")) {
    if (!(col %in% names(r))) r[, (col) := NA_character_]
    r[, (col) := as.character(get(col))]
  }
  if (nrow(r) == 0L) return(r[, `:=`(station = character(), measure_name = character(), measure_label = character())])
  st <- rt_text(r$station_label)
  st <- ifelse(nzchar(st), st, rt_text(r$label))
  st <- ifelse(nzchar(st), st, rt_text(r$station_reference))
  st <- ifelse(nzchar(st), st, "(unnamed station)")
  skey <- ifelse(nzchar(rt_text(r$station_uri)), rt_text(r$station_uri),
                 ifelse(nzchar(rt_text(r$station_reference)), rt_text(r$station_reference), as.character(seq_len(nrow(r)))))
  r[, station := rt_disambiguate(st, skey, list(r$station_reference, r$town, r$river_name, r$catchment_name))]
  param <- ifelse(nzchar(rt_text(r$parameter)), rt_text(r$parameter), "measure")
  qual  <- ifelse(nzchar(rt_text(r$qualifier)), paste0(", ", rt_text(r$qualifier)), "")
  unit  <- ifelse(nzchar(rt_text(r$unit_name)), paste0(" (", rt_text(r$unit_name), ")"), "")
  r[, measure_name := paste0(param, qual, unit)]
  mkey <- ifelse(nzchar(rt_text(r$measure_uri)), rt_text(r$measure_uri), as.character(seq_len(nrow(r))))
  r[, measure_label := rt_disambiguate(paste0(station, ": ", measure_name), mkey, list(r$period))]
  r[]
}

rt_filter_register <- function(r, area = RT_ALL, river = RT_ALL, param = RT_ALL, stn = RT_ALL) {
  if (!identical(area, RT_ALL))  r <- r[r$ea_area_name %in% area]
  if (!identical(river, RT_ALL)) r <- r[r$river_name %in% river]
  if (!identical(param, RT_ALL)) r <- r[r$parameter %in% param]
  if (!identical(stn, RT_ALL))   r <- r[r$station %in% stn]
  r
}

rt_values <- function(r, column) c(RT_ALL, sort(unique(as.character(stats::na.omit(r[[column]])))))

# The choices for each drop-down, each narrowed by the other choices, so a selection
# can never lead to an empty list. Stations are narrowed by all three.
rt_options <- function(r, area = RT_ALL, river = RT_ALL, param = RT_ALL) {
  list(
    areas      = rt_values(rt_filter_register(r, river = river, param = param), "ea_area_name"),
    rivers     = rt_values(rt_filter_register(r, area = area, param = param),    "river_name"),
    parameters = rt_values(rt_filter_register(r, area = area, river = river),    "parameter"),
    stations   = rt_values(rt_filter_register(r, area = area, river = river, param = param), "station"))
}

# Midnight UTC at the end of the last day held in the archive, or NULL when nothing is archived.
rt_boundary <- function(max_archive_date) {
  if (is.null(max_archive_date) || length(max_archive_date) == 0L || is.na(max_archive_date[1L])) return(NULL)
  d <- as.Date(max_archive_date[1L])
  as.POSIXct(paste(format(d + 1L, "%Y-%m-%d"), "00:00:00"), tz = "UTC")
}

# Facts for under the chart: readings in the archive and in the live tail, and the newest.
rt_summary <- function(readings, boundary, now) {
  if (is.null(readings) || nrow(readings) == 0L) return(list(readings = 0L, archive = 0L, live = 0L, newest = NULL, age_minutes = NA_real_))
  t <- readings$date_time
  live <- if (is.null(boundary)) 0L else sum(t >= boundary)
  newest <- max(t)
  list(readings = nrow(readings), archive = nrow(readings) - live, live = live, newest = newest,
       age_minutes = round(as.numeric(difftime(now, newest, units = "mins")), 1))
}

# One panel per unit (metres and cubic metres a second do not share an axis).
rt_panels <- function(readings, chosen) {
  m <- chosen[!duplicated(chosen$measure_uri)]
  unit <- ifelse(nzchar(rt_text(m$unit_name)), rt_text(m$unit_name), "(no unit)")
  names(unit) <- m$measure_uri
  list(units = sort(unique(unit[unique(readings$measure_uri)])), unit_of = unit,
       label_of = stats::setNames(m$measure_label, m$measure_uri))
}

# The shaded live band and the dashed archive | live line, as plotly layout shapes.
rt_live_shapes <- function(boundary, start, now) {
  if (is.null(boundary) || boundary >= now) return(list())
  fmt <- function(x) format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  shapes <- list(list(type = "rect", xref = "x", yref = "paper", x0 = fmt(max(boundary, start)), x1 = fmt(now),
                      y0 = 0, y1 = 1, fillcolor = "#9ED8F8", opacity = 0.3, line = list(width = 0), layer = "below"))
  if (start < boundary) {
    shapes[[2L]] <- list(type = "line", xref = "x", yref = "paper", x0 = fmt(boundary), x1 = fmt(boundary),
                         y0 = 0, y1 = 1, line = list(color = "#053D7E", dash = "dash", width = 1.5))
  }
  shapes
}


# -----------------------------------------------------------------------------
# polygons_only()
#
# The polygon layers (EA flood areas, constituencies) are drawn with
# leaflet::addPolygons(), which only understands POLYGON and MULTIPOLYGON. A
# source geometry can arrive as a GEOMETRYCOLLECTION (a polygon with a stray line
# or point beside it), and one such row stops the whole layer with "Don't know how
# to get polygon data from object of class XY,GEOMETRYCOLLECTION,sfg". Collections
# are reduced to their polygon parts and every other non-polygon row is dropped, so
# one odd shape cannot take the layer down.
# -----------------------------------------------------------------------------

polygons_only <- function(shape) {
  if (is.null(shape) || nrow(shape) == 0L) return(shape)
  # An empty geometry (including an empty collection) cannot be drawn, and one empty
  # collection in the batch makes sf::st_collection_extract return nothing at all for
  # the rest, so empties go first.
  shape <- shape[!sf::st_is_empty(shape), ]
  if (nrow(shape) == 0L) return(shape)
  type <- as.character(sf::st_geometry_type(shape))
  coll <- type == "GEOMETRYCOLLECTION"
  if (any(coll)) {
    parts <- suppressWarnings(sf::st_collection_extract(shape[coll, ], "POLYGON"))
    shape <- rbind(shape[!coll, ], parts)
    type  <- as.character(sf::st_geometry_type(shape))
  }
  shape[type %in% c("POLYGON", "MULTIPOLYGON"), ]
}


# -----------------------------------------------------------------------------
# clears_threshold() and intersection_label()
#
# intersection_pct is the share of an area covered by a forecast polygon. A coastal
# forecast geometry is a line, which has no area, so the pipeline stores NULL for it
# and keeps the row whatever the threshold (job 07). The threshold slider therefore
# applies to areas only. Comparing NA with >= gives NA, which data.table silently
# drops and which sf turns into an all-NA row (an empty geometry the map cannot draw),
# so a coastal area either vanished or broke the layer. A missing share passes.
# -----------------------------------------------------------------------------

clears_threshold <- function(pct, minimum) {
  is.na(pct) | pct >= minimum
}

intersection_label <- function(pct) {
  ifelse(is.na(pct), "crossed by a coastal line", paste0(round(pct * 100), "%"))
}


# -----------------------------------------------------------------------------
# line_rows()
#
# TRUE for the rows of an sf object whose geometry is a line. Coastal FGS
# geometries are lines along the coast, not areas. Drawn with addPolygons() a line
# is closed into a filled shape, so the layer draws them with addPolylines() instead.
# -----------------------------------------------------------------------------

line_rows <- function(shape) {
  if (is.null(shape) || nrow(shape) == 0L) return(logical())
  as.character(sf::st_geometry_type(shape)) %in% c("LINESTRING", "MULTILINESTRING")
}


# -----------------------------------------------------------------------------
# normalise_name()                                              POC-ONLY
#
# Key for matching a constituency name to an MP record, where the two tables
# share no id. Lower case, "&" as "and", punctuation and spacing removed.
# -----------------------------------------------------------------------------

normalise_name <- function(x) {  # POC-ONLY
  x <- tolower(as.character(x))
  x <- gsub("&", "and", x, fixed = TRUE)
  gsub("[^a-z0-9]", "", x)
}


# -----------------------------------------------------------------------------
# aggregate_situation()                                         POC-ONLY
# (statement look-back: remove with the other POC-ONLY blocks when live)
#
# Rebuilds one row per constituency for ANY statement, from the intersection
# rows the pipeline keeps for every statement. The live constituency_flood_
# situation table only ever holds the latest one, so this is what makes a
# look-back possible. It mirrors what job 19 does for the forecast half:
#
#   - polygons covering less than min_pct of a constituency are ignored
#   - the worst rank per constituency, forecast day and source, then per day
#   - worst rank over the five days, the first date it occurs, the sources
#   - escalation from the changes table for that statement
#
# What it CANNOT rebuild, and so leaves NA: warnings and alerts in force
# (the pipeline does not keep a history of them).
#
#   inter      constituency_id, constituency_name, forecast_date, day_index,
#              source, risk_level, intersection_pct
#   changes    the statement's fgs_constituency_changes rows (or NULL)
#   changes_available  FALSE if that table does not exist: escalation is then NA
#   mp         constituency_name, mp_name, mp_party, mp_email, mp_phone
#   all_const  every constituency (id, name), so quiet ones still get a row
# -----------------------------------------------------------------------------

POC_RANK_LEVELS <- c("Very Low", "Low", "Medium", "High")   # POC-ONLY

aggregate_situation <- function(inter, changes, mp, all_const, statement_id, issued_at, min_pct = 0.01,
                                 changes_available = TRUE) {  # POC-ONLY
  inter <- data.table::as.data.table(inter)
  all_const <- data.table::as.data.table(all_const)[, list(constituency_id, constituency_name)]
  out <- data.table::copy(all_const)

  if (nrow(inter) > 0L) {
    inter <- inter[is.na(intersection_pct) | intersection_pct >= min_pct]
    inter[, rk := match(risk_level, POC_RANK_LEVELS)]
    inter <- inter[!is.na(rk)]
  }
  for (d in 1:5) out[, (paste0("fgs_rank_day", d)) := 0L]
  out[, `:=`(fgs_worst_rank = 0L, fgs_worst_date = NA_character_, fgs_worst_sources = NA_character_)]

  if (nrow(inter) > 0L) {
    cell <- inter[, list(rk = max(rk)), by = list(constituency_id, forecast_date, day_index, source)]
    byday <- cell[, list(rk = max(rk)), by = list(constituency_id, day_index)]
    for (d in 1:5) {
      v <- byday[day_index == d]
      if (nrow(v) > 0L) out[v, (paste0("fgs_rank_day", d)) := i.rk, on = "constituency_id"]
    }
    worst <- cell[, list(w = max(rk)), by = constituency_id]
    out[worst, fgs_worst_rank := i.w, on = "constituency_id"]
    top <- cell[worst, on = list(constituency_id), nomatch = NULL][rk == w]
    first <- top[, list(fgs_worst_date = as.character(min(as.Date(forecast_date))),
                        fgs_worst_sources = paste(sort(unique(source)), collapse = ", ")),
                 by = constituency_id]
    out[first, `:=`(fgs_worst_date = i.fgs_worst_date, fgs_worst_sources = i.fgs_worst_sources), on = "constituency_id"]
  }
  out[, fgs_worst_level := ifelse(fgs_worst_rank == 0L, "None", POC_RANK_LEVELS[pmax(1L, fgs_worst_rank)])]

  ch <- if (is.null(changes)) data.table::data.table(constituency_id = character(), change_type = character()) else data.table::as.data.table(changes)
  out[, `:=`(fgs_changed   = constituency_id %in% ch$constituency_id,
             fgs_escalated = constituency_id %in% ch[change_type %in% c("new", "escalated"), constituency_id])]
  # If the changes table is not there, escalation is UNKNOWN, not "none".
  if (!changes_available) out[, `:=`(fgs_changed = NA, fgs_escalated = NA)]

  # MP details are today's, matched on a normalised name.
  mp <- data.table::as.data.table(mp)
  out[, key := normalise_name(constituency_name)]
  if (nrow(mp) > 0L) {
    mp <- data.table::copy(mp)[, key := normalise_name(constituency_name)][!duplicated(key)]
    out[mp, `:=`(mp_name = i.mp_name, mp_party = i.mp_party, mp_email = i.mp_email, mp_phone = i.mp_phone), on = "key"]
  } else {
    out[, `:=`(mp_name = NA_character_, mp_party = NA_character_, mp_email = NA_character_, mp_phone = NA_character_)]
  }
  out[, key := NULL]
  out[, mp_matched := !is.na(mp_name)]

  # Not held for past statements.
  out[, `:=`(severe_warnings = NA_integer_, warnings = NA_integer_, alerts = NA_integer_, has_active_warning = NA)]
  out[, `:=`(fgs_statement_id = as.integer(statement_id), fgs_issued_at = as.character(issued_at))]
  data.table::setcolorder(out, c("constituency_id", "constituency_name", "mp_matched", "mp_name", "mp_party",
    "mp_email", "mp_phone", "fgs_statement_id", "fgs_issued_at", "fgs_worst_rank", "fgs_worst_level",
    "fgs_worst_date", "fgs_worst_sources", paste0("fgs_rank_day", 1:5), "fgs_changed", "fgs_escalated",
    "severe_warnings", "warnings", "alerts", "has_active_warning"))
  out[]
}


# -----------------------------------------------------------------------------
# situation_detail() and apply_situation_detail()
#
# The Constituency situation table can be narrowed to some flood sources, and shows the
# impact and likelihood behind each constituency's worst risk. Neither is held in
# constituency_flood_situation (job 19), which folds every source together, so both are
# worked out here from the stored intersections, for any statement.
#
# situation_detail() gives, per constituency, over the chosen sources only:
#   the worst rank on each of days 1 to 5, the worst rank over the five days, the first
#   date it occurs, the sources at that rank on any day (as job 19 does), and the impact
#   and likelihood on that first worst day. Where several polygons share the worst rank
#   on that day, the one with the highest impact is shown, then the highest likelihood.
#   Overlaps below min_pct of a constituency are ignored, as job 14 ignores them.
#   Constituencies with nothing under the chosen sources are absent from the result.
#
#   inter    constituency_id, forecast_date, day_index, source, risk_level, risk_x, risk_y,
#            impact_label, likelihood_label, intersection_pct (one statement)
#   sources  character vector of flood sources, or NULL for all
#
# apply_situation_detail() adds the impact and likelihood columns to a situation table. When
# `narrow` is TRUE (a source filter is on) it also replaces the risk columns with the
# detail's, so a constituency with nothing under the chosen sources drops to none.
# -----------------------------------------------------------------------------

SITUATION_DETAIL_COLUMNS <- c("fgs_worst_rank", "fgs_worst_date", "fgs_worst_sources",
                              "fgs_worst_impact", "fgs_worst_likelihood",
                              "fgs_worst_impact_x", "fgs_worst_likelihood_y")

situation_detail <- function(inter, sources = NULL, min_pct = 0.01) {
  none <- data.table::data.table(
    constituency_id = character(), fgs_worst_rank = integer(), fgs_worst_date = character(),
    fgs_worst_sources = character(), fgs_worst_impact = character(), fgs_worst_likelihood = character(),
    fgs_worst_impact_x = integer(), fgs_worst_likelihood_y = integer())
  for (d in 1:5) none[, (paste0("fgs_rank_day", d)) := integer()]
  if (is.null(inter) || nrow(inter) == 0L) return(none)

  x <- data.table::copy(data.table::as.data.table(inter))
  # Older or reduced callers may not carry the impact columns: they then show as n/a.
  for (col in c("risk_x", "risk_y")) if (!(col %in% names(x))) x[, (col) := NA_integer_]
  for (col in c("impact_label", "likelihood_label")) if (!(col %in% names(x))) x[, (col) := NA_character_]
  x <- x[is.na(intersection_pct) | intersection_pct >= min_pct]
  if (length(sources) > 0L) x <- x[tolower(source) %in% tolower(sources)]
  x[, rk := match(risk_level, POC_RANK_LEVELS)]
  x <- x[!is.na(rk)]
  if (nrow(x) == 0L) return(none)
  x[, `:=`(day = as.integer(day_index), fdate = as.Date(forecast_date),
           ix = suppressWarnings(as.integer(risk_x)), ly = suppressWarnings(as.integer(risk_y)))]

  out <- data.table::data.table(constituency_id = unique(x$constituency_id))
  byday <- x[, list(rk = max(rk)), by = list(constituency_id, day)]
  for (d in 1:5) {
    out[, (paste0("fgs_rank_day", d)) := 0L]
    v <- byday[day == d]
    if (nrow(v) > 0L) out[v, (paste0("fgs_rank_day", d)) := i.rk, on = "constituency_id"]
  }
  worst <- x[, list(fgs_worst_rank = max(rk)), by = constituency_id]
  out[worst, fgs_worst_rank := i.fgs_worst_rank, on = "constituency_id"]

  top <- x[worst, on = "constituency_id"][rk == fgs_worst_rank]
  first <- top[, list(first_date = min(fdate),
                      fgs_worst_sources = paste(sort(unique(source)), collapse = ", ")), by = constituency_id]
  out[first, `:=`(fgs_worst_date = as.character(i.first_date), fgs_worst_sources = i.fgs_worst_sources),
      on = "constituency_id"]

  on_day <- top[first, on = "constituency_id"][fdate == first_date]
  data.table::setorder(on_day, constituency_id, -ix, -ly, na.last = TRUE)
  pick <- on_day[, .SD[1L], by = constituency_id]
  pick[, `:=`(imp = data.table::fifelse(is.na(impact_label) | impact_label == "", NA_character_, as.character(impact_label)),
              lik = data.table::fifelse(is.na(likelihood_label) | likelihood_label == "", NA_character_, as.character(likelihood_label)))]
  out[pick, `:=`(fgs_worst_impact = i.imp, fgs_worst_likelihood = i.lik,
                 fgs_worst_impact_x = i.ix, fgs_worst_likelihood_y = i.ly), on = "constituency_id"]
  out[]
}

apply_situation_detail <- function(d, detail, narrow = FALSE) {
  d <- data.table::copy(d)
  d[, `:=`(fgs_worst_impact = NA_character_, fgs_worst_likelihood = NA_character_,
           fgs_worst_impact_x = NA_integer_, fgs_worst_likelihood_y = NA_integer_)]
  if (is.null(detail)) return(d[])
  m <- match(d$constituency_id, detail$constituency_id)
  d[, `:=`(fgs_worst_impact       = detail$fgs_worst_impact[m],
           fgs_worst_likelihood   = detail$fgs_worst_likelihood[m],
           fgs_worst_impact_x     = detail$fgs_worst_impact_x[m],
           fgs_worst_likelihood_y = detail$fgs_worst_likelihood_y[m])]
  if (narrow) {
    for (k in 1:5) {
      col <- paste0("fgs_rank_day", k)
      d[, (col) := data.table::fcoalesce(as.integer(detail[[col]][m]), 0L)]
    }
    d[, `:=`(fgs_worst_rank    = data.table::fcoalesce(as.integer(detail$fgs_worst_rank[m]), 0L),
             fgs_worst_date    = detail$fgs_worst_date[m],
             fgs_worst_sources = detail$fgs_worst_sources[m])]
    d[, fgs_worst_level := data.table::fifelse(fgs_worst_rank == 0L, "None", POC_RANK_LEVELS[pmax(1L, fgs_worst_rank)])]
  }
  d[]
}


# -----------------------------------------------------------------------------
# empty_situation_message()
#
# What the Constituency situation table says when no row survives the filters.
# On a quiet statement the default floor (Medium) hides everything, and a bare
# "no match" reads as a broken page. If the only reason is the risk floor, say
# so and say how many sit below it.
#
#   ranks          fgs_worst_rank for every constituency in the statement
#   floor          the minimum rank chosen (0 to 4)
#   other_filters  TRUE if a search, escalated or warning filter is also on
# -----------------------------------------------------------------------------

empty_situation_message <- function(ranks, floor, other_filters = FALSE) {
  generic <- "No constituency matches these filters."
  if (other_filters || floor <= 0L) return(generic)
  lower <- sum(ranks >= 1L & ranks < floor, na.rm = TRUE)
  if (lower == 0L) {
    return(sprintf("Nothing in this statement is forecast at %s or above, and no constituency is flagged at any risk.",
                   RANK_LABELS[floor + 1L]))
  }
  sprintf("Nothing in this statement is forecast at %s or above. %d constituenc%s forecast at a lower risk: lower \"Minimum forecast risk\" to see %s.",
          RANK_LABELS[floor + 1L], lower, if (lower == 1L) "y is" else "ies are", if (lower == 1L) "it" else "them")
}


# -----------------------------------------------------------------------------
# Warehouse errors the app can explain
#
# missing_table_info(): if an error message says a table does not exist, which
# table, and which pipeline notebook builds it. Returns NULL for any other error.
# is_connection_error(): the warehouse could not be reached at all.
# -----------------------------------------------------------------------------

PIPELINE_TABLE_JOBS <- c(
  fgs_constituency_changes = 18L, constituency_flood_situation = 19L, mp_briefing_drafts = 20L,
  fgs_constituency_exposure = 21L, pipeline_health = 22L, ea_warning_episodes = 25L, fgs_forecast_verification = 26L, ea_rt_measure_register = 9L, ea_rt_readings_all = 23L, ea_rt_readings_bronze = 10L, fgs_constituency_mp_contacts = 14L,
  flood_warning_constituency_contacts = 13L, fgs_constituency_intersections = 7L,
  fgs_ea_area_intersections = 7L, ea_area_constituency_lookup = 6L)

missing_table_info <- function(msg) {
  msg <- paste(as.character(msg), collapse = " ")
  if (!grepl("TABLE_OR_VIEW_NOT_FOUND|table or view .* cannot be found", msg)) return(NULL)
  m <- regmatches(msg, regexec("table or view `[^`]*`\\.`[^`]*`\\.`([^`]+)`", msg))[[1]]
  tbl <- if (length(m) == 2L) m[2L] else "(unknown table)"
  job <- unname(PIPELINE_TABLE_JOBS[tbl])
  list(table = tbl, job = if (length(job) == 0L || is.na(job)) NA_integer_ else job)
}

is_connection_error <- function(msg) {
  grepl("Failed to connect to warehouse|Failed to perform HTTP request|Could not resolve host|Timeout was reached|Connection (refused|reset)",
        paste(as.character(msg), collapse = " "))
}

# gaps_message(): one sentence naming the missing tables and the notebook to run.
gaps_message <- function(gaps, location = NULL) {
  if (length(gaps) == 0L) return("")
  one <- vapply(names(gaps), function(t) {
    j <- gaps[[t]]
    if (is.null(j) || is.na(j)) t else sprintf("%s (pipeline notebook %d)", t, as.integer(j))
  }, character(1))
  sprintf("Some pipeline tables are not %syet, so parts of the app are limited: %s. Press Refresh statements to retry.",
          if (is.null(location)) "available " else paste0("in ", location, " "), paste(one, collapse = "; "))
}


# -----------------------------------------------------------------------------
# with_retries()
#
# Runs fn(), and if it fails with an error that retry_if accepts, waits and tries
# again, up to `retries` more times. Any other error is raised at once, so a bad
# query or a missing table is never retried. The final error is raised as it
# came, with its class, so callers see the real message.
#
# Safe here because the app only reads. Do not use it around anything that
# writes.
#
#   pauses     seconds to wait before each retry (the last is reused)
#   sleep      how to wait; replaceable so tests need not
#   on_failure called after every failed attempt:
#              function(attempt, total, msg, will_retry, pause)
# -----------------------------------------------------------------------------

with_retries <- function(fn, retries = 2L, pauses = c(1.5, 3), retry_if = is_connection_error,
                         sleep = Sys.sleep, on_failure = function(attempt, total, msg, will_retry, pause) NULL) {
  attempt <- 0L
  repeat {
    attempt <- attempt + 1L
    res <- tryCatch(list(value = fn()), error = function(e) e)
    if (!inherits(res, "error")) return(res$value)
    msg <- conditionMessage(res)
    will_retry <- attempt <= retries && isTRUE(retry_if(msg))
    pause <- if (will_retry) pauses[min(attempt, length(pauses))] else NA_real_
    on_failure(attempt, retries + 1L, msg, will_retry, pause)
    if (!will_retry) stop(res)
    sleep(pause)
  }
}


# -----------------------------------------------------------------------------
# The outage log
#
# A console that prints each failure once cannot say how long a drop lasted, and
# one that prints every failure floods. These keep one state and return the line
# to print, if any: the first failure, then at most one reminder a minute while
# it continues, then one line when the warehouse answers again.
#
# outage_new()                      a fresh state
# outage_failure(state, now, msg)   -> list(state, log)
# outage_success(state, now)        -> list(state, log)
# -----------------------------------------------------------------------------

outage_new <- function() list(since = NULL, failures = 0L, last_logged = NULL)

fmt_duration <- function(secs) {
  secs <- as.numeric(secs)
  if (secs < 60) return(sprintf("%d s", as.integer(round(secs))))
  if (secs < 3600) return(sprintf("%d min %d s", as.integer(secs %/% 60), as.integer(round(secs %% 60))))
  sprintf("%d h %d min", as.integer(secs %/% 3600), as.integer((secs %% 3600) %/% 60))
}

outage_failure <- function(state, now, msg, quiet_secs = 60) {
  first <- is.null(state$since)
  if (first) state$since <- now
  state$failures <- state$failures + 1L
  log <- NULL
  if (first) {
    log <- sprintf("Databricks warehouse unreachable: %s", substr(gsub("\\s+", " ", msg), 1L, 200L))
  } else if (as.numeric(difftime(now, state$last_logged, units = "secs")) >= quiet_secs) {
    log <- sprintf("Databricks warehouse still unreachable since %s (%d failed requests so far)",
                   format(state$since, "%H:%M:%S"), state$failures)
  }
  if (!is.null(log)) state$last_logged <- now
  list(state = state, log = log)
}

outage_success <- function(state, now) {
  if (is.null(state$since)) return(list(state = state, log = NULL))
  log <- sprintf("Databricks warehouse reachable again after %s (%d failed request%s)",
                 fmt_duration(difftime(now, state$since, units = "secs")), state$failures,
                 if (state$failures == 1L) "" else "s")
  list(state = outage_new(), log = log)
}
