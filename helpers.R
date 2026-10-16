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
  x <- substr(as.character(x), 1L, 19L)
  as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
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
