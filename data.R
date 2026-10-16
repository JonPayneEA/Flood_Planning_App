# =============================================================================
# data.R
# All data access for the dashboard, via brickster's DBI backend.
#
# brickster wraps the Databricks SQL Statement Execution API. From R's
# perspective it's a standard DBI driver: dbConnect, dbGetQuery, dbDisconnect.
# Authentication uses DATABRICKS_HOST and DATABRICKS_TOKEN from the environment.
#
# One function per query. Each runs one SQL statement through db_query().
#
# The connection is built once and reused. brickster opens every connection by
# sending a "SELECT 1 as test_connection" first, so a connection per query
# doubled the requests (the warehouse's query history shows the pair for every
# call), added about 300 ms to each, and doubled the chance of a network drop.
# The Statement Execution API is stateless, so one connection is safe to share.
# It is rebuilt after a network error and after DB_CONN_MAX_AGE seconds, and a
# network error is retried (see with_retries in helpers.R). The app only reads.
#
# All returned tables are data.tables. Geometry is parsed into sf before
# returning so leaflet can plot it directly.
# =============================================================================


# -----------------------------------------------------------------------------
# The connection and the query function
#
# db_connector, db_run, db_close and db_sleep are separate, tiny functions so a
# test can replace them. In normal use they are the obvious one-liners.
# -----------------------------------------------------------------------------

DB_CONN_MAX_AGE <- 1800        # seconds before the shared connection is rebuilt

.fgs_db <- new.env(parent = emptyenv())
.fgs_db$con    <- NULL
.fgs_db$made   <- NULL
.fgs_db$outage <- outage_new()

# The warehouse_id tells brickster which warehouse runs the statement. Host and
# token come from DATABRICKS_HOST and DATABRICKS_TOKEN, which keeps credentials
# out of any error trace.
db_connector <- function() {
  DBI::dbConnect(brickster::DatabricksSQL(), warehouse_id = Sys.getenv("DATABRICKS_WAREHOUSE_ID"))
}
db_run   <- function(con, sql) DBI::dbGetQuery(con, sql)
db_close <- function(con) tryCatch(DBI::dbDisconnect(con), error = function(e) NULL)
db_sleep <- function(seconds) Sys.sleep(seconds)

db_drop_connection <- function() {
  if (!is.null(.fgs_db$con)) db_close(.fgs_db$con)
  .fgs_db$con  <- NULL
  .fgs_db$made <- NULL
  invisible(NULL)
}

# The shared connection, built on first use and after DB_CONN_MAX_AGE.
db_connection <- function() {
  if (DEMO_MODE) stop("Demo mode: no warehouse connection is available, by design.")
  age <- if (is.null(.fgs_db$made)) Inf else as.numeric(difftime(Sys.time(), .fgs_db$made, units = "secs"))
  if (is.null(.fgs_db$con) || age > DB_CONN_MAX_AGE) {
    db_drop_connection()
    .fgs_db$con  <- db_connector()
    .fgs_db$made <- Sys.time()
  }
  .fgs_db$con
}

# One line to the console, stamped.
db_log <- function(text) message("[", format(Sys.time(), "%H:%M:%S"), "] ", text)

# -----------------------------------------------------------------------------
# db_query()
#
# Runs one read query and returns a data.table. A network error drops the shared
# connection and is retried up to DB_RETRIES times; any other error (a missing
# table, a SQL mistake) is raised at once. The console records an outage once
# when it starts, at most once a minute while it lasts, and once when it ends.
#
# Note on parameterisation:
#   brickster's DBI implementation does not support the params = argument to
#   dbGetQuery() that standard DBI drivers expose. Values are interpolated
#   into the SQL string via sprintf() in each fetch_* function below. Every
#   interpolated value is an integer we control, a value from a fixed set, or an
#   identifier reduced to letters, digits and underscores, never raw user input.
# -----------------------------------------------------------------------------

db_query <- function(sql) {
  if (DEMO_MODE) stop("Demo mode: no warehouse query can run, by design.")

  note <- function(r) { .fgs_db$outage <- r$state; if (!is.null(r$log)) db_log(r$log) }

  result <- with_retries(
    function() {
      con <- db_connection()
      tryCatch(db_run(con, sql), error = function(e) {
        if (is_connection_error(conditionMessage(e))) db_drop_connection()
        stop(e)
      })
    },
    retries = DB_RETRIES,
    sleep = function(s) db_sleep(s),
    on_failure = function(attempt, total, msg, will_retry, pause) {
      if (is_connection_error(msg)) note(outage_failure(.fgs_db$outage, Sys.time(), msg))
      else                          note(outage_success(.fgs_db$outage, Sys.time()))   # the warehouse answered: it is reachable
    }
  )
  note(outage_success(.fgs_db$outage, Sys.time()))
  setDT(result)
  result[]
}


# =============================================================================
# QUERIES
#
# Each function corresponds to one block of data the UI needs. SQL is kept
# verbose so it lifts straight into Databricks SQL for debugging.
# =============================================================================


# -----------------------------------------------------------------------------
# fetch_recent_statements()
#
# Returns the most recent N statements as a data.table for use in the
# statement picker dropdown. Caller composes display labels from issued_at
# and statement_id.
#
# Fetched once at session start (to populate the dropdown choices); also
# doubles as the lookup table selected_statement() reads from on every
# statement-picker change, so the per-source forecast text, the England-wide
# forecast narrative, and the PDF link are all available with no extra
# round trip.
# -----------------------------------------------------------------------------

fetch_recent_statements <- function(n = 20L) {
  db_query(sprintf("
    SELECT
      statement_id,
      issued_at,
      headline,
      source_coastal,
      source_surface,
      source_ground,
      source_river,
      england_forecast,
      pdf_url
    FROM %s
    ORDER BY issued_at DESC
    LIMIT %d
  ", TBL_STATEMENTS, as.integer(n)))
}


# -----------------------------------------------------------------------------
# fetch_latest_statement()
#
# Returns metadata for the most recently issued FGS statement, as a named
# list. The title banner uses this; every other query depends on the
# statement_id it provides.
#
# Returns NULL when the table is empty -- caller checks with is.null().
# -----------------------------------------------------------------------------

fetch_latest_statement <- function() {
  dt <- db_query(sprintf("
    SELECT
      statement_id,
      issued_at,
      headline,
      flood_risk_trend_day1,
      flood_risk_trend_day2,
      flood_risk_trend_day3,
      flood_risk_trend_day4,
      flood_risk_trend_day5
    FROM %s
    ORDER BY issued_at DESC
    LIMIT 1
  ", TBL_STATEMENTS))
  
  if (nrow(dt) == 0L) return(NULL)
  as.list(dt[1L])
}


# -----------------------------------------------------------------------------
# fetch_risk_polygons()
#
# Returns FGS risk polygons for a given statement-day as an sf object.
#
# Geometry is stored as GeoJSON strings in the Delta table, per the
# pipeline-wide choice (GeoJSON from Delta through to Leaflet, no
# conversion step). The string column is selected as-is and parsed in
# R by geojsonsf::geojson_sfc.
#
# The six risk matrix columns (risk_x, risk_y, impact_label, likelihood_label,
# risk_level, risk_colour) are pulled through directly so downstream filtering
# in app_server.R can work on the data.table representation before plotting.
# -----------------------------------------------------------------------------

fetch_risk_polygons <- function(statement_id, day_index) {
  dt <- db_query(sprintf("
    SELECT
      poly_id,
      source,
      day_index,
      forecast_date,
      risk_x,
      risk_y,
      impact_label,
      likelihood_label,
      risk_level,
      risk_colour,
      geometry
    FROM %s
    WHERE statement_id = %d
      AND day_index    = %d
  ", TBL_RISK_POLYGONS, as.integer(statement_id), as.integer(day_index)))
  
  # Empty result: return an empty sf carrying the full attribute schema, so
  # downstream code can read shape$risk_colour etc. without NULL errors.
  # Quiet FGSs (no polygons today) are normal -- this is an expected state,
  # not an error.
  if (nrow(dt) == 0L) {
    return(sf::st_sf(
      poly_id          = integer(),
      source           = character(),
      day_index        = integer(),
      forecast_date    = as.Date(character()),
      risk_x           = integer(),
      risk_y           = integer(),
      impact_label     = character(),
      likelihood_label = character(),
      risk_level       = character(),
      risk_colour      = character(),
      geometry         = sf::st_sfc(),
      crs              = 4326
    ))
  }
  
  # Geometry is stored as GeoJSON strings in the Delta table -- this is the
  # pipeline-wide format choice, GeoJSON from Delta through to Leaflet.
  # geojsonsf::geojson_sfc parses a character vector of GeoJSON strings into
  # an sfc geometry column in one vectorised call. CRS 4326 (WGS84 lat/lon)
  # is the GeoJSON default and what leaflet expects.
  geom <- geojsonsf::geojson_sfc(dt$geometry)
  dt[, geometry := NULL]
  sf::st_sf(dt, geometry = geom, crs = 4326)
}


# -----------------------------------------------------------------------------
# fetch_ea_areas_for_statement()
#
# Returns EA flood areas (FWA/FAA) intersecting any risk polygon in the
# statement-day, ordered by risk band descending then intersection size.
#
# The CASE expression on risk_colour gives a stable Red > Amber > Yellow >
# Green sort that doesn't depend on the textual risk_level field. Better
# done in SQL than in R because the database is already touching the rows.
# -----------------------------------------------------------------------------

fetch_ea_areas_for_statement <- function(statement_id, day_index) {
  db_query(sprintf("
    SELECT
      ea_area_code,
      ea_area_name,
      ea_area_type,
      source,
      risk_level,
      risk_colour,
      risk_x,
      risk_y,
      intersection_pct
    FROM %s
    WHERE statement_id    = %d
      AND day_index       = %d
      AND geometry_source = '%s'
    ORDER BY
      CASE risk_colour
        WHEN 'Red'    THEN 1
        WHEN 'Amber'  THEN 2
        WHEN 'Yellow' THEN 3
        WHEN 'Green'  THEN 4
        ELSE 5
      END,
      intersection_pct DESC
  ", TBL_EA_INTERSECT, as.integer(statement_id), as.integer(day_index),
     GEOMETRY_SOURCE))
}


# -----------------------------------------------------------------------------
# fetch_summary_counts()
#
# Three headline counts for the count cards: polygons, distinct EA areas,
# distinct constituencies. One round trip rather than three; the REST API
# overhead is modest but visible at startup if we make multiple calls.
# -----------------------------------------------------------------------------

fetch_summary_counts <- function(statement_id, day_index) {
  
  sid <- as.integer(statement_id)
  did <- as.integer(day_index)
  
  dt <- db_query(sprintf("
    SELECT
      (SELECT COUNT(*)
         FROM %s WHERE statement_id = %d AND day_index = %d) AS polygon_count,
      (SELECT COUNT(DISTINCT ea_area_code)
         FROM %s WHERE statement_id = %d AND day_index = %d
          AND geometry_source = '%s') AS ea_area_count,
      (SELECT COUNT(DISTINCT constituency_id)
         FROM %s WHERE statement_id = %d AND day_index = %d
          AND geometry_source = '%s') AS constituency_count
  ",
                         TBL_RISK_POLYGONS,   sid, did,
                         TBL_EA_INTERSECT,    sid, did, GEOMETRY_SOURCE,
                         TBL_CONST_INTERSECT, sid, did, GEOMETRY_SOURCE))
  
  if (nrow(dt) == 0L) {
    return(list(polygon_count = 0L, ea_area_count = 0L, constituency_count = 0L))
  }
  as.list(dt[1L])
}


# -----------------------------------------------------------------------------
# fetch_ea_geometry()
#
# Returns affected EA flood areas with their polygon geometry, ready to draw
# on the map. Unions the FWA and FAA geometry tables so the layer shows both
# types. The intersection table is the inner driver -- only areas affected
# by the current statement-day appear.
#
# Sort and risk-colour come from the intersection table (the polygon's risk
# colour, not the area's). One row per area per statement-day; if an area
# overlaps multiple polygons, the join surfaces all rows but the map will
# overdraw them so the strongest colour wins -- which is fine, the area
# count panel already deduplicates by code for the headline number.
#
# ea_area_code doubles as the Target Area ID forecasters refer to, so the
# map popup can show it directly with no extra join.
# -----------------------------------------------------------------------------

fetch_ea_geometry <- function(statement_id, day_index) {

  sid <- as.integer(statement_id)
  did <- as.integer(day_index)

  dt <- db_query(sprintf("
    WITH affected AS (
      SELECT
        i.ea_area_code,
        i.ea_area_name,
        i.ea_area_type,
        i.risk_colour,
        i.risk_level,
        i.intersection_pct,
        i.source
      FROM %s i
      WHERE i.statement_id    = %d
        AND i.day_index       = %d
        AND i.geometry_source = '%s'
    )
    SELECT
      a.ea_area_code,
      a.ea_area_name,
      a.ea_area_type,
      a.risk_colour,
      a.risk_level,
      a.intersection_pct,
      a.source,
      g.geometry
    FROM affected a
    LEFT JOIN (
      SELECT ea_area_code, geometry FROM %s
      UNION ALL
      SELECT ea_area_code, geometry FROM %s
    ) g
      ON g.ea_area_code = a.ea_area_code
    WHERE g.geometry IS NOT NULL
  ",
                         TBL_EA_INTERSECT, sid, did, GEOMETRY_SOURCE,
                         TBL_EA_FWA,
                         TBL_EA_FAA))

  if (nrow(dt) == 0L) {
    # Return an empty sf with the full attribute schema, not just a geometry
    # column. Downstream code reads shape$risk_colour etc.; an sf with only
    # geometry causes those lookups to return NULL and the observer to error.
    return(sf::st_sf(
      ea_area_code     = character(),
      ea_area_name     = character(),
      ea_area_type     = character(),
      risk_colour      = character(),
      risk_level       = character(),
      intersection_pct = numeric(),
      source           = character(),
      geometry         = sf::st_sfc(),
      crs              = 4326
    ))
  }

  # Geometry comes back as GeoJSON strings, same as the risk polygons.
  geom <- geojsonsf::geojson_sfc(dt$geometry)
  dt[, geometry := NULL]
  sf::st_sf(dt, geometry = geom, crs = 4326)
}


# -----------------------------------------------------------------------------
# fetch_constituency_geometry()
#
# Returns affected parliamentary constituencies with their polygon geometry.
# Same join pattern as fetch_ea_geometry -- intersection table for the
# affected set, constituency table for the shapes.
#
# source, risk_x, risk_y and intersection_pct are pulled through (not just
# risk_colour/risk_level) so app_server.R can apply the same source, matrix-cell,
# and threshold filters used for polygons and EA areas, rather than showing
# every constituency touched by any polygon in the statement-day.
# -----------------------------------------------------------------------------

fetch_constituency_geometry <- function(statement_id, day_index) {

  sid <- as.integer(statement_id)
  did <- as.integer(day_index)

  dt <- db_query(sprintf("
    WITH affected AS (
      SELECT DISTINCT
        i.constituency_id,
        i.constituency_name,
        i.source,
        i.risk_x,
        i.risk_y,
        i.risk_colour,
        i.risk_level,
        i.intersection_pct
      FROM %s i
      WHERE i.statement_id    = %d
        AND i.day_index       = %d
        AND i.geometry_source = '%s'
    )
    SELECT
      a.constituency_id,
      a.constituency_name,
      a.source,
      a.risk_x,
      a.risk_y,
      a.risk_colour,
      a.risk_level,
      a.intersection_pct,
      c.geometry
    FROM affected a
    LEFT JOIN %s c
      ON c.constituency_id = a.constituency_id
    WHERE c.geometry IS NOT NULL
  ",
                         TBL_CONST_INTERSECT, sid, did, GEOMETRY_SOURCE,
                         TBL_CONSTITUENCIES))

  if (nrow(dt) == 0L) {
    return(sf::st_sf(
      constituency_id   = character(),
      constituency_name = character(),
      source            = character(),
      risk_x            = integer(),
      risk_y            = integer(),
      risk_colour       = character(),
      risk_level        = character(),
      intersection_pct  = numeric(),
      geometry          = sf::st_sfc(),
      crs               = 4326
    ))
  }

  geom <- geojsonsf::geojson_sfc(dt$geometry)
  dt[, geometry := NULL]
  sf::st_sf(dt, geometry = geom, crs = 4326)
}


# -----------------------------------------------------------------------------
# fetch_constituencies_for_statement()
#
# Metadata-only counterpart to fetch_constituency_geometry() -- no geometry
# join, so it's cheap enough to run on every filter change rather than only
# when the constituency map layer is switched on. Backs the constituency
# stat card, mirroring how fetch_ea_areas_for_statement() backs the EA area
# count independently of the EA map layer toggle.
# -----------------------------------------------------------------------------

fetch_constituencies_for_statement <- function(statement_id, day_index) {
  db_query(sprintf("
    SELECT
      constituency_id,
      constituency_name,
      source,
      risk_x,
      risk_y,
      risk_colour,
      risk_level,
      intersection_pct
    FROM %s
    WHERE statement_id    = %d
      AND day_index       = %d
      AND geometry_source = '%s'
  ", TBL_CONST_INTERSECT, as.integer(statement_id), as.integer(day_index),
     GEOMETRY_SOURCE))
}


# -----------------------------------------------------------------------------
# fetch_ea_constituency_mp()
#
# Returns, for every EA flood area, the constituency(ies) it overlaps and
# that constituency's MP contact details. Backs the "Export CSV" button on
# the Affected Target Areas panel.
#
# The two source tables share no ID column -- ea_area_constituency_lookup
# and mp_contact_details are joined on constituency_name.
# -----------------------------------------------------------------------------

fetch_ea_constituency_mp <- function() {
  db_query(sprintf("
    SELECT
      l.ea_area_code,
      l.constituency_name,
      l.intersection_pct AS constituency_overlap_pct,
      m.name              AS mp_name,
      m.party             AS mp_party,
      m.email             AS mp_email,
      m.phone             AS mp_phone
    FROM %s l
    LEFT JOIN %s m
      ON m.constituency_name = l.constituency_name
  ", TBL_EA_CONSTITUENCY_LOOKUP, TBL_MP_CONTACTS))
}



# =============================================================================
# PIPELINE PRODUCTS (FGS_Notebooks jobs 18 to 22)
#
# Plain tables built by the pipeline, so these queries do no spatial work and
# no geometry parsing. Ranks: 1 Very Low, 2 Low, 3 Medium, 4 High, 0 nothing.
# =============================================================================


# -----------------------------------------------------------------------------
# fetch_pipeline_health()
#
# One row per health check plus an `overall` row (job 22). Backs the status
# badge in the header. The table is rebuilt on every poll, so it is tiny.
# -----------------------------------------------------------------------------

fetch_pipeline_health <- function() {
  db_query(sprintf("
    SELECT check_name, status, value, detail, checked_at
    FROM %s
  ", TBL_HEALTH))
}


# -----------------------------------------------------------------------------
# fetch_changes()
#
# What changed in a statement against the one before it (job 18), per
# constituency, forecast day and flood source. Unchanged cells are not
# stored. History is kept, so the statement_id picks the right batch. Rows
# are limited to the active geometry source.
# -----------------------------------------------------------------------------

fetch_changes <- function(statement_id) {
  db_query(sprintf("
    SELECT
      constituency_id,
      constituency_name,
      forecast_date,
      day_index,
      source,
      previous_rank,
      new_rank,
      rank_change,
      change_type
    FROM %s
    WHERE statement_id    = %d
      AND geometry_source = '%s'
    ORDER BY new_rank DESC, rank_change DESC, constituency_name
  ", TBL_CHANGES, as.integer(statement_id), GEOMETRY_SOURCE))
}


# -----------------------------------------------------------------------------
# fetch_situation()
#
# One row per constituency (job 19): worst forecast risk over five days, risk
# by day, escalation flag, warnings and alerts in force, and the MP. The
# table holds the pipeline's latest complete statement only, whichever
# statement the picker shows, so the caller should say so on screen.
# Forecast and warnings stay in separate columns by design; no combined score.
# -----------------------------------------------------------------------------

fetch_situation <- function() {
  db_query(sprintf("
    SELECT
      constituency_id,
      constituency_name,
      mp_matched,
      mp_name,
      mp_party,
      mp_email,
      mp_phone,
      fgs_statement_id,
      fgs_issued_at,
      fgs_worst_rank,
      fgs_worst_level,
      fgs_worst_date,
      fgs_worst_sources,
      fgs_rank_day1, fgs_rank_day2, fgs_rank_day3, fgs_rank_day4, fgs_rank_day5,
      fgs_changed,
      fgs_escalated,
      severe_warnings,
      warnings,
      alerts,
      has_active_warning
    FROM %s
  ", TBL_SITUATION))
}


# -----------------------------------------------------------------------------
# fetch_briefing_drafts()
#
# DRAFT briefings for MPs whose constituency reaches the pipeline's minimum
# risk (job 20). Nothing is sent from here or from the pipeline: the app only
# displays the text so a person can review and send it by their own route.
# Returns the newest batch (highest statement_id) for the active geometry
# source.
# -----------------------------------------------------------------------------

fetch_briefing_drafts <- function(statement_id = NULL) {
  # NULL means the newest batch. A statement id (POC-ONLY look-back) reads that
  # statement's batch: the pipeline keeps older statements' drafts as history.
  sid_sql <- if (is.null(statement_id)) {
    sprintf("(SELECT MAX(statement_id) FROM %s WHERE geometry_source = '%s')", TBL_BRIEFINGS, GEOMETRY_SOURCE)
  } else as.character(as.integer(statement_id))
  db_query(sprintf("
    SELECT
      statement_id,
      constituency_id,
      constituency_name,
      mp_name,
      mp_email,
      worst_rank,
      worst_level,
      worst_date,
      is_escalation,
      has_active_warning,
      subject,
      body,
      status,
      reviewed_by
    FROM %s
    WHERE geometry_source = '%s'
      AND statement_id = %s
    ORDER BY worst_rank DESC, constituency_name
  ", TBL_BRIEFINGS, GEOMETRY_SOURCE, sid_sql))
}


# -----------------------------------------------------------------------------
# fetch_exposure()
#
# Days each constituency sat at each risk rank, by calendar month and flood
# source (job 21; source = 'any' is the worst across sources). Sparse: a
# month with no flagged day has no row, and days_covered is the denominator.
# Counts day 1 of the last statement issued on each date, so each date is
# counted once. Limited to the last n_months calendar months.
# -----------------------------------------------------------------------------

fetch_exposure <- function(n_months = 12L) {
  db_query(sprintf("
    SELECT
      constituency_id,
      constituency_name,
      month,
      source,
      days_medium_or_above,
      days_flagged,
      worst_rank,
      days_covered
    FROM %s
    WHERE month >= date_format(add_months(current_date(), -%d), 'yyyy-MM')
  ", TBL_EXPOSURE, as.integer(n_months) - 1L))
}


# -----------------------------------------------------------------------------
# fetch_constituency_shape()
#
# One constituency's boundary, for the "show on map" jump from the pipeline
# views. The id comes from a browser input, so it is reduced to safe
# characters before it goes into the SQL.
# -----------------------------------------------------------------------------

fetch_constituency_shape <- function(constituency_id) {
  cid <- gsub("[^A-Za-z0-9_]", "", as.character(constituency_id))
  dt <- db_query(sprintf("
    SELECT constituency_id, name AS constituency_name, geometry
    FROM %s
    WHERE constituency_id = '%s'
      AND geometry IS NOT NULL
  ", TBL_CONSTITUENCIES, cid))
  if (nrow(dt) == 0L) return(NULL)
  geom <- geojsonsf::geojson_sfc(dt$geometry)
  dt[, geometry := NULL]
  sf::st_sf(dt, geometry = geom, crs = 4326)
}


# -----------------------------------------------------------------------------
# fetch_constituency_risk_geometry()
#
# Boundaries of every constituency with a forecast risk in the pipeline's
# latest complete statement (worst of the five days), joined to the situation
# table for the colour and the popup. Only flagged constituencies are
# returned, so the geometry stays small. This follows the situation table,
# not the statement or day pickers.
# -----------------------------------------------------------------------------

fetch_constituency_risk_geometry <- function() {
  dt <- db_query(sprintf("
    SELECT
      s.constituency_id,
      s.constituency_name,
      s.fgs_worst_rank,
      s.fgs_worst_level,
      s.fgs_worst_date,
      s.fgs_worst_sources,
      s.fgs_escalated,
      s.severe_warnings,
      s.warnings,
      s.alerts,
      s.mp_name,
      c.geometry
    FROM %s s
    JOIN %s c
      ON c.constituency_id = s.constituency_id
    WHERE s.fgs_worst_rank >= 1
      AND c.geometry IS NOT NULL
  ", TBL_SITUATION, TBL_CONSTITUENCIES))

  if (nrow(dt) == 0L) {
    return(sf::st_sf(
      constituency_id = character(), constituency_name = character(),
      fgs_worst_rank = integer(), fgs_worst_level = character(),
      fgs_worst_date = character(), fgs_worst_sources = character(),
      fgs_escalated = logical(), severe_warnings = integer(),
      warnings = integer(), alerts = integer(), mp_name = character(),
      geometry = sf::st_sfc(), crs = 4326))
  }
  geom <- geojsonsf::geojson_sfc(dt$geometry)
  dt[, geometry := NULL]
  sf::st_sf(dt, geometry = geom, crs = 4326)
}


# -----------------------------------------------------------------------------
# fetch_data_signature()
#
# One cheap query that changes whenever the pipeline publishes something new:
# a new or amended statement, or a fresh health check (rebuilt every poll, so
# it also moves when the situation tables are rebuilt). The auto-refresh
# observer compares this string between ticks and reloads only on a change.
# -----------------------------------------------------------------------------

fetch_data_signature <- function() {
  dt <- db_query(sprintf("
    SELECT
      (SELECT COALESCE(CAST(MAX(issued_at)        AS STRING), '') FROM %s) AS newest_statement,
      (SELECT COUNT(*)                                            FROM %s) AS n_statements,
      (SELECT COALESCE(CAST(MAX(last_modified_at) AS STRING), '') FROM %s) AS last_modified,
      (SELECT COALESCE(CAST(MAX(checked_at)       AS STRING), '') FROM %s) AS health_checked
  ", TBL_STATEMENTS, TBL_STATEMENTS, TBL_STATEMENTS, TBL_HEALTH))
  if (nrow(dt) == 0L) return(NA_character_)
  paste(unlist(dt[1L]), collapse = "|")
}


# -----------------------------------------------------------------------------
# POC-ONLY: statement look-back (remove when the app goes live)
#
# The live constituency_flood_situation table holds only the latest statement.
# To look back at an earlier one, the situation is rebuilt here from the
# intersection rows the pipeline keeps for every statement
# (fgs_constituency_intersections), using aggregate_situation() in helpers.R.
# Warnings and alerts in force are not kept historically, so they come back NA.
# MP details are today's. Switched off by FGS_POC_FEATURES=0.
# -----------------------------------------------------------------------------

fetch_all_constituencies <- function() {  # POC-ONLY
  db_query(sprintf("SELECT constituency_id, name AS constituency_name FROM %s", TBL_CONSTITUENCIES))
}

fetch_mp_lookup <- function() {  # POC-ONLY
  db_query(sprintf("
    SELECT constituency_name, name AS mp_name, party AS mp_party, email AS mp_email, phone AS mp_phone
    FROM %s
  ", TBL_MP_CONTACTS))
}

fetch_constituency_intersections <- function(statement_id) {  # POC-ONLY
  db_query(sprintf("
    SELECT constituency_id, constituency_name, forecast_date, day_index, source, risk_level, intersection_pct
    FROM %s
    WHERE statement_id    = %d
      AND geometry_source = '%s'
  ", TBL_CONST_INTERSECT, as.integer(statement_id), GEOMETRY_SOURCE))
}

fetch_situation_for_statement <- function(statement_id, issued_at = NA) {  # POC-ONLY
  # The changes table comes from pipeline notebook 18 and may not exist yet. The
  # look-back must degrade (escalation unknown), not fail with it.
  changes <- tryCatch(fetch_changes(statement_id), error = function(e) NULL)
  aggregate_situation(
    inter = fetch_constituency_intersections(statement_id),
    changes = changes, changes_available = !is.null(changes),
    mp = fetch_mp_lookup(),
    all_const = fetch_all_constituencies(),
    statement_id = statement_id, issued_at = issued_at, min_pct = CONTACT_MIN_PCT)
}


# =============================================================================
# QUERY CACHE
#
# Results are shared by every session in the R process and reused for
# CACHE_TTL_SECONDS. The data changes at most every poll (30 to 60 minutes)
# and an issued statement is immutable until amended, so switching between
# statements, days and filters should not go back to the warehouse each time.
# The whole cache is cleared when the auto-refresh signature changes or
# someone presses Refresh, so an amended statement is picked up promptly.
#
# data.tables are copied on the way out, so a caller that modifies one by
# reference cannot corrupt the cached copy. Errors are never cached.
# =============================================================================

.fgs_cache     <- new.env(parent = emptyenv())
.fgs_neg_cache <- new.env(parent = emptyenv())
CACHE_MAX_ENTRIES <- 200L
NEG_CACHE_SECONDS <- 300     # how long a "table does not exist" answer is remembered

cache_clear <- function() {
  rm(list = ls(.fgs_cache,     all.names = TRUE), envir = .fgs_cache)
  rm(list = ls(.fgs_neg_cache, all.names = TRUE), envir = .fgs_neg_cache)
  invisible(NULL)
}

# A missing table stays missing until someone runs a pipeline notebook, so asking
# again on every statement switch only costs a round trip and fills the console.
# That one kind of error is remembered for NEG_CACHE_SECONDS, and forgotten on
# Refresh or when the pipeline publishes. Network errors are never remembered:
# they are usually brief and the next attempt should try again.
with_cache <- function(fn, name, ttl = CACHE_TTL_SECONDS) {
  force(fn); force(name); force(ttl)
  function(...) {
    args <- vapply(list(...), function(a) paste(format(a), collapse = ","), character(1))
    key  <- paste(c(name, args), collapse = "|")
    now  <- Sys.time()

    bad <- .fgs_neg_cache[[key]]
    if (!is.null(bad) && as.numeric(difftime(now, bad$at, units = "secs")) < NEG_CACHE_SECONDS) stop(bad$msg, call. = FALSE)

    if (ttl > 0) {
      hit <- .fgs_cache[[key]]
      if (!is.null(hit) && as.numeric(difftime(now, hit$at, units = "secs")) < ttl) {
        return(if (is.data.table(hit$value)) copy(hit$value) else hit$value)
      }
    }

    t0 <- Sys.time()
    value <- tryCatch(fn(...), error = function(e) {
      if (!is.null(missing_table_info(conditionMessage(e)))) {
        assign(key, list(at = Sys.time(), msg = conditionMessage(e)), envir = .fgs_neg_cache)
      }
      stop(e)
    })
    took <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    # A slow query otherwise looks like a hung map. Say so in the R console.
    if (took > 5) message(sprintf("[%s] %s took %.0f s", format(Sys.time(), "%H:%M:%S"), name, took))
    if (ttl <= 0) return(value)

    keys <- ls(.fgs_cache, all.names = TRUE)
    if (length(keys) >= CACHE_MAX_ENTRIES) {
      ages <- vapply(keys, function(k) as.numeric(.fgs_cache[[k]]$at), numeric(1))
      rm(list = keys[which.min(ages)], envir = .fgs_cache)
    }
    assign(key, list(at = now, value = value), envir = .fgs_cache)
    if (is.data.table(value)) copy(value) else value
  }
}

# Statement-scoped queries and reference lookups. Not cached: the statement
# list, the health table and the signature, which must always be fresh.
fetch_risk_polygons                <- with_cache(fetch_risk_polygons,                "risk_polygons")
fetch_ea_areas_for_statement       <- with_cache(fetch_ea_areas_for_statement,       "ea_areas")
fetch_summary_counts               <- with_cache(fetch_summary_counts,               "summary_counts")
fetch_ea_geometry                  <- with_cache(fetch_ea_geometry,                  "ea_geometry")
fetch_constituency_geometry        <- with_cache(fetch_constituency_geometry,        "const_geometry")
fetch_constituencies_for_statement <- with_cache(fetch_constituencies_for_statement, "const_meta")
fetch_changes                      <- with_cache(fetch_changes,                      "changes")
fetch_constituency_shape           <- with_cache(fetch_constituency_shape,           "const_shape")
fetch_all_constituencies           <- with_cache(fetch_all_constituencies,           "all_constituencies", ttl = max(CACHE_TTL_SECONDS, 0) * 6)  # POC-ONLY
fetch_mp_lookup                    <- with_cache(fetch_mp_lookup,                    "mp_lookup")                                                 # POC-ONLY
fetch_situation_for_statement      <- with_cache(fetch_situation_for_statement,      "situation_for_statement")                                   # POC-ONLY
fetch_constituency_risk_geometry   <- with_cache(fetch_constituency_risk_geometry,   "const_risk_geometry", ttl = min(CACHE_TTL_SECONDS, 120))

# Fresh on every call (these are reloaded each poll), but a missing table is remembered.
fetch_pipeline_health              <- with_cache(fetch_pipeline_health,              "health",    ttl = 0)
fetch_situation                    <- with_cache(fetch_situation,                    "situation", ttl = 0)
fetch_briefing_drafts              <- with_cache(fetch_briefing_drafts,              "briefings", ttl = 0)
fetch_exposure                     <- with_cache(fetch_exposure,                     "exposure",  ttl = 0)
fetch_data_signature               <- with_cache(fetch_data_signature,               "signature", ttl = 0)


# =============================================================================
# DEMO MODE
#
# Replaces every fetch_* function with a generator over synthetic data. Sourced
# last so it overrides the warehouse versions and bypasses their cache. db_query
# and db_connection already refuse to run in demo mode, so a fetcher that was
# missed would fail loudly instead of reaching for a warehouse.
# =============================================================================

if (DEMO_MODE) source("demo.R", local = TRUE)
