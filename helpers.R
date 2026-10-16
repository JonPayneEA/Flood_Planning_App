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
  fgs_constituency_exposure = 21L, pipeline_health = 22L, fgs_constituency_mp_contacts = 14L,
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
