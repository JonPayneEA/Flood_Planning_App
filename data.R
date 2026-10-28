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
        i.risk_x,
        i.risk_y,
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
      a.risk_x,
      a.risk_y,
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
      risk_x           = integer(),
      risk_y           = integer(),
      risk_colour      = character(),
      risk_level       = character(),
      intersection_pct = numeric(),
      source           = character(),
      geometry         = sf::st_sfc(),
      crs              = 4326
    ))
  }

  # Geometry comes back as GeoJSON strings, same as the risk polygons. A collection
  # (a polygon with a stray line beside it) is reduced to its polygon parts.
  geom <- geojsonsf::geojson_sfc(dt$geometry)
  dt[, geometry := NULL]
  polygons_only(sf::st_sf(dt, geometry = geom, crs = 4326))
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
  polygons_only(sf::st_sf(dt, geometry = geom, crs = 4326))
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
      l.constituency_id,
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
# fetch_area_constituency_links()
#
# Every flood warning area and flood alert area with each constituency it overlaps and the MP, for the Exports tab. The share is
# intersection area over area (0 to 1). One row per area and constituency, slivers included; the tab filters.
# -----------------------------------------------------------------------------

fetch_area_constituency_links <- function() {
  db_query(sprintf("
    SELECT
      l.ea_area_code,
      a.ea_area_name,
      a.area_kind,
      a.type_text,
      a.county,
      a.river_or_sea,
      l.constituency_id,
      l.constituency_name,
      l.intersection_pct AS share_of_area_in_constituency,
      m.name             AS mp_name,
      m.party            AS mp_party,
      m.email            AS mp_email
    FROM %s l
    JOIN (
      SELECT ea_area_code, ea_area_name, type_text, county, river_or_sea, 'Flood warning' AS area_kind FROM %s
      UNION ALL
      SELECT ea_area_code, ea_area_name, type_text, county, river_or_sea, 'Flood alert'   AS area_kind FROM %s
    ) a ON a.ea_area_code = l.ea_area_code
    LEFT JOIN %s m ON m.constituency_name = l.constituency_name
  ", TBL_EA_CONSTITUENCY_LOOKUP, TBL_EA_FWA, TBL_EA_FAA, TBL_MP_CONTACTS))
}


# -----------------------------------------------------------------------------
# fetch_pipeline_health()
#
# One row per health check plus an `overall` row (job 22). Backs the status
# badge in the header. The table is rebuilt on every poll, so it is tiny.
# -----------------------------------------------------------------------------

# fetch_fw_thresholds(): the Observed flood warning thresholds linked to these measures (job 28, restricted schema), in each measure's own
# unit: value_in_measure_unit, which job 28 has moved to the gauge's datum where the file's unit differed. Only links the job made
# (linked or linked_chosen). Read only when the Hydrometrics tick is on; an error (no access, job 28 not run) is for the caller to explain.
fetch_fw_thresholds <- function(measure_uris) {
  uris <- gsub("[^A-Za-z0-9:/._#-]", "", as.character(measure_uris)); uris <- uris[nzchar(uris)]
  empty <- data.table(measure_uri = character(), type = character(), purpose = character(), season = character(), label_prefix = character(),
                      target_area = character(), value = numeric(), unit_name = character(), unit_match = character())
  if (length(uris) == 0L) return(empty)
  # SELECT * so a links table built before the label prefix was carried (job 28) still reads; the missing columns are filled in below.
  d <- as.data.table(db_query(sprintf("
    SELECT *
    FROM %s
    WHERE link_status IN ('linked', 'linked_chosen')
      AND measure_uri IN (%s)
      AND value_in_measure_unit IS NOT NULL
  ", TBL_FW_THRESHOLDS, paste0("'", uris, "'", collapse = ", "))))
  for (cn in c("type", "purpose", "season", "label_prefix", "target_area", "unit_name", "unit_match")) if (!(cn %in% names(d))) d[, (cn) := NA_character_]
  if (nrow(d) == 0L) return(empty)
  d <- d[, .(measure_uri, type, purpose, season, label_prefix, target_area, value = as.numeric(value_in_measure_unit), unit_name, unit_match)]
  unique(d)[order(measure_uri, value)]
}

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
# fetch_changes_log()
#
# One row if job 18 compared this statement with the one before it, none if it
# has not. n_changes = 0 is a real "nothing moved". flagged_* and medium_plus_*
# count the constituencies each statement flags over all five of its own days. The table is newer than the
# rest, so a caller treats an error (table missing) as "unknown", not a gap.
# -----------------------------------------------------------------------------

fetch_changes_log <- function(statement_id) {
  # SELECT * on purpose: the headline counts (flagged_*, medium_plus_*) arrive with a
  # later version of job 18. Until it has run, the log table has no such columns, and
  # naming them would fail the whole query and lose the rest of the row.
  db_query(sprintf("
    SELECT *
    FROM %s
    WHERE statement_id    = %d
      AND geometry_source = '%s'
  ", TBL_CHANGES_LOG, as.integer(statement_id), GEOMETRY_SOURCE))
}


# -----------------------------------------------------------------------------
# fetch_potential_areas()
#
# The flood warning and flood alert areas that sit under the statement's risk
# polygons, counted per constituency. These are POTENTIAL warnings and alerts:
# places where one could be issued, not ones in force. Works for any statement,
# because fgs_ea_area_intersections keeps every statement's rows (EA areas with
# less than 25% covered are already dropped there). An area is linked to a
# constituency when at least TARGET_AREA_MIN_PCT of the area lies inside it, the
# same rule the pipeline uses for warning contacts. An area spanning several
# constituencies counts once in each; an area under several days or polygons
# counts once. min_rank keeps only areas under polygons at that risk rank or
# above (0 keeps all; 1 Very Low, 2 Low, 3 Medium, 4 High). sources keeps only
# areas under polygons of those flood sources (NULL keeps all).
# -----------------------------------------------------------------------------

fetch_potential_areas <- function(statement_id, min_rank = 0L, sources = NULL) {
  source_sql <- if (length(sources) > 0L) {
    sprintf("AND LOWER(source) IN (%s)", paste0("'", gsub("[^a-z]", "", tolower(sources)), "'", collapse = ", "))
  } else ""
  db_query(sprintf("
    SELECT
      l.constituency_id,
      COUNT(DISTINCT CASE WHEN LOWER(e.ea_area_type) = 'flood_warning' THEN e.ea_area_code END) AS warning_areas,
      COUNT(DISTINCT CASE WHEN LOWER(e.ea_area_type) = 'flood_alert'   THEN e.ea_area_code END) AS alert_areas
    FROM (
      SELECT DISTINCT ea_area_code, ea_area_type
      FROM %s
      WHERE statement_id    = %d
        AND geometry_source = '%s'
        AND CASE risk_level WHEN 'Very Low' THEN 1 WHEN 'Low' THEN 2 WHEN 'Medium' THEN 3 WHEN 'High' THEN 4 ELSE 0 END >= %d
        %s
    ) e
    JOIN %s l ON l.ea_area_code = e.ea_area_code
    WHERE l.intersection_pct >= %s
    GROUP BY l.constituency_id
  ", TBL_EA_INTERSECT, as.integer(statement_id), GEOMETRY_SOURCE, as.integer(min_rank), source_sql,
     TBL_EA_CONSTITUENCY_LOOKUP, format(TARGET_AREA_MIN_PCT, scientific = FALSE)))
}


# -----------------------------------------------------------------------------
# fetch_warning_history()
#
# Every alert and warning the pipeline has recorded (job 25, one episode per run
# in force) linked to constituencies by the same rule as the contacts: an area
# belongs to a constituency when at least TARGET_AREA_MIN_PCT of it lies inside.
# Counted by day of issue so the app can narrow the period without another
# query. n_severe counts the warnings that reached severe at any point.
# -----------------------------------------------------------------------------

fetch_warning_history <- function() {
  db_query(sprintf("
    SELECT
      l.constituency_id,
      e.ea_area_code,
      MAX(n.ea_area_name)                                  AS ea_area_name,
      LOWER(e.area_type)                                   AS ea_area_type,
      CAST(e.start_time AS DATE)                           AS issued_date,
      COUNT(*)                                             AS n_issued,
      SUM(CASE WHEN e.had_severe THEN 1 ELSE 0 END)        AS n_severe
    FROM %s e
    JOIN %s l ON l.ea_area_code = e.ea_area_code
    LEFT JOIN (
      SELECT ea_area_code, ea_area_name FROM %s
      UNION ALL
      SELECT ea_area_code, ea_area_name FROM %s
    ) n ON n.ea_area_code = e.ea_area_code
    WHERE l.intersection_pct >= %s
      AND LOWER(e.area_type) IN ('flood_alert', 'flood_warning')
    GROUP BY l.constituency_id, e.ea_area_code, LOWER(e.area_type), CAST(e.start_time AS DATE)
  ", TBL_WARNING_EPISODES, TBL_EA_CONSTITUENCY_LOOKUP, TBL_EA_FWA, TBL_EA_FAA,
     format(TARGET_AREA_MIN_PCT, scientific = FALSE)))
}


# -----------------------------------------------------------------------------
# Warnings against the FGS (ea_warning_fgs_coverage, job 26)
#
# fetch_coverage_summary(): counts of warnings by risk threshold, category (before, same day,
#   later days only, none) and lead, made in the warehouse so little crosses the wire.
# fetch_coverage_warnings(): the warnings themselves for a period, newest first, with how the
#   chosen risk threshold covered each. Capped at `limit` rows.
# fetch_coverage_detail(): every FGS statement that put a polygon over one warning's area for a
#   date it was in force (the issue day and any later day). The area code reaches the SQL as letters, digits, dashes and
#   underscores only.
# fetch_coverage_span(): the first and last issue dates held.
# -----------------------------------------------------------------------------

coverage_where <- function(from, to, type) {
  sprintf("issue_date BETWEEN '%s' AND '%s' AND %s", format(as.Date(from), "%Y-%m-%d"), format(as.Date(to), "%Y-%m-%d"),
          coverage_type_sql(type))
}

fetch_coverage_summary <- function(from, to, type = "any") {
  one <- function(r) {
    sql <- sprintf("
      SELECT %d AS threshold, category, lead_days, COUNT(*) AS n FROM (
        SELECT CASE WHEN first_cover_ge{R} < issue_date THEN 'before'
                    WHEN first_cover_ge{R} IS NOT NULL THEN 'same_day'
                    WHEN later_rank >= {R} THEN 'later'
                    ELSE 'none' END AS category,
               CASE WHEN first_cover_ge{R} < issue_date THEN datediff(issue_date, first_cover_ge{R}) END AS lead_days
        FROM %s WHERE %s
      ) GROUP BY category, lead_days", r, TBL_WARNING_FGS, coverage_where(from, to, type))
    gsub("{R}", as.character(r), sql, fixed = TRUE)
  }
  d <- as.data.table(db_query(paste(vapply(1:4, one, character(1)), collapse = " UNION ALL ")))
  if (nrow(d)) d[, `:=`(threshold = as.integer(threshold), lead_days = as.integer(lead_days), n = as.integer(n))]
  d
}

fetch_coverage_warnings <- function(from, to, type = "any", min_rank = 1L, limit = 20000L) {
  r <- max(1L, min(4L, as.integer(min_rank)))
  db_query(sprintf("
    SELECT episode_id, ea_area_code, ea_area_name, area_type, start_time, end_time, peak_level, had_severe, stale_open,
           issue_date, max_rank, issue_day_rank, later_rank, first_cover_ge%d AS first_cover, sources
    FROM %s
    WHERE %s
    ORDER BY start_time DESC
    LIMIT %d
  ", r, TBL_WARNING_FGS, coverage_where(from, to, type), as.integer(limit)))
}

fetch_coverage_detail <- function(ea_area_code, from, to) {
  code <- gsub("[^A-Za-z0-9_-]", "", as.character(ea_area_code))
  db_query(sprintf("
    SELECT s.statement_id, s.issued_at, i.forecast_date, i.day_index, i.source, i.risk_level,
           i.impact_label, i.likelihood_label, i.intersection_pct, i.link_type
    FROM %s i
    JOIN %s s ON s.statement_id = i.statement_id
    WHERE i.ea_area_code = '%s'
      AND i.geometry_source = '%s'
      AND to_date(i.forecast_date) BETWEEN '%s' AND '%s'
      AND %s >= 1
    ORDER BY s.issued_at, i.forecast_date, i.source
  ", TBL_EA_INTERSECT, TBL_STATEMENTS, code, GEOMETRY_SOURCE,
     format(as.Date(from), "%Y-%m-%d"), format(as.Date(to), "%Y-%m-%d"),
     "CASE i.risk_level WHEN 'Very Low' THEN 1 WHEN 'Low' THEN 2 WHEN 'Medium' THEN 3 WHEN 'High' THEN 4 ELSE 0 END"))
}

fetch_coverage_span <- function() {
  d <- db_query(sprintf("SELECT MIN(issue_date) AS first_date, MAX(issue_date) AS last_date FROM %s", TBL_WARNING_FGS))
  if (nrow(d) == 0L || is.na(d$first_date[1L])) NULL else list(first = as.Date(d$first_date[1L]), last = as.Date(d$last_date[1L]))
}


# -----------------------------------------------------------------------------
# fetch_warning_episodes()
#
# Every alert and warning (one row per run in force) for the areas linked to ONE
# constituency, by the same overlap rule as fetch_warning_history(). The id reaches
# the SQL as letters and digits only. end_time is NULL while an episode is open.
# -----------------------------------------------------------------------------

# fetch_area_episodes(): every alert or warning issued for one area (the same episodes as above, by area instead of by constituency), for the
#   area detail page's warning issue history. end_time is NULL while an episode is open. The id reaches the SQL as safe characters only.
fetch_area_episodes <- function(area_id) {
  id <- gsub("[^A-Za-z0-9_.-]", "", as.character(area_id))
  db_query(sprintf("
    SELECT episode_id, ea_area_code, LOWER(area_type) AS ea_area_type, start_time, end_time, duration_minutes, peak_level, had_severe, stale_open
    FROM %s
    WHERE ea_area_code = '%s' AND LOWER(area_type) IN ('flood_alert', 'flood_warning')
    ORDER BY start_time DESC
  ", TBL_WARNING_EPISODES, id))
}

fetch_warning_episodes <- function(constituency_id) {
  cid <- gsub("[^A-Za-z0-9]", "", as.character(constituency_id))
  db_query(sprintf("
    SELECT
      e.episode_id,
      e.ea_area_code,
      MAX(n.ea_area_name)  AS ea_area_name,
      LOWER(e.area_type)   AS ea_area_type,
      e.start_time,
      e.end_time,
      e.duration_minutes,
      e.peak_level,
      e.had_severe,
      e.stale_open
    FROM %s e
    JOIN %s l ON l.ea_area_code = e.ea_area_code
    LEFT JOIN (
      SELECT ea_area_code, ea_area_name FROM %s
      UNION ALL
      SELECT ea_area_code, ea_area_name FROM %s
    ) n ON n.ea_area_code = e.ea_area_code
    WHERE l.constituency_id = '%s'
      AND l.intersection_pct >= %s
      AND LOWER(e.area_type) IN ('flood_alert', 'flood_warning')
    GROUP BY e.episode_id, e.ea_area_code, LOWER(e.area_type), e.start_time, e.end_time,
             e.duration_minutes, e.peak_level, e.had_severe, e.stale_open
    ORDER BY e.start_time DESC
  ", TBL_WARNING_EPISODES, TBL_EA_CONSTITUENCY_LOOKUP, TBL_EA_FWA, TBL_EA_FAA, cid,
     format(TARGET_AREA_MIN_PCT, scientific = FALSE)))
}


# -----------------------------------------------------------------------------
# fetch_warnings_in_force()
#
# Every area with an alert, warning or severe warning in force now, nationally: one row per
# EA area (flood_warnings_current, job 11; severity 1 severe flood warning, 2 flood warning,
# 3 flood alert; 4 is "no longer in force"). The Overview counts the rows by level and lists
# them when a count is clicked.
# -----------------------------------------------------------------------------

fetch_warnings_in_force <- function() {
  db_query(sprintf("
    SELECT
      flood_area_id,
      severity_level,
      description,
      river_or_sea,
      county,
      time_raised,
      time_severity_changed
    FROM %s
    WHERE severity_level BETWEEN 1 AND 3
    ORDER BY severity_level, county, description
  ", TBL_FLOOD_WARN_CURRENT))
}


# -----------------------------------------------------------------------------
# Warning area exposure (ea_area_exposure and ea_area_overlaps, FGS_Notebooks job 35; restricted schema)
#
# fetch_area_exposure(): one row per flood warning area and flood alert area: people, households,
#   the rivers and sea bands, Flood Zones, the unique and split counts that cope with overlap, and the
#   sites inside. Rivers and sea only.
# fetch_area_overlaps(): the areas of the same kind that share people with one area.
#   The id comes from a browser input, so it is reduced to safe characters before the SQL.
# -----------------------------------------------------------------------------

fetch_area_exposure <- function() {
  # SELECT * so a table built before a column was added (the area risk score, added later) still reads; ensure_area_columns() fills the gaps.
  ensure_area_columns(db_query(sprintf("SELECT * FROM %s", TBL_AREA_EXPOSURE)))
}

# fetch_area_shape(): one warning or alert area's boundary (sf, longitude and latitude), for the area detail map. NULL when not found.
fetch_area_shape <- function(area_id) {
  id <- gsub("[^A-Za-z0-9_.-]", "", as.character(area_id))
  dt <- db_query(sprintf("
    SELECT ea_area_code, geometry FROM %s WHERE ea_area_code = '%s' AND geometry IS NOT NULL
    UNION ALL
    SELECT ea_area_code, geometry FROM %s WHERE ea_area_code = '%s' AND geometry IS NOT NULL
  ", TBL_EA_FWA, id, TBL_EA_FAA, id))
  if (nrow(dt) == 0L) return(NULL)
  geom <- geojsonsf::geojson_sfc(dt$geometry)
  dt[, geometry := NULL]
  sf::st_sf(dt, geometry = geom, crs = 4326)
}

# fetch_area_sites(): the critical sites inside one area, worst rivers and sea band first (job 35's ea_area_sites).
fetch_area_sites <- function(area_id) {
  id <- gsub("[^A-Za-z0-9_.-]", "", as.character(area_id))
  db_query(sprintf("
    SELECT site_id, name, site_group, site_type, band, rofrs_rank
    FROM %s
    WHERE area_id = '%s'
    ORDER BY rofrs_rank DESC, name
  ", TBL_AREA_SITES, id))
}

# fetch_area_geographies(): who responds to an area: the districts, counties, police forces, constituencies and regions it lies in
#   (and wards), with the people in each. One area for the detail page, or every area (area_id NULL) for the export.
fetch_area_geographies <- function(area_id = NULL) {
  where <- if (is.null(area_id)) "" else sprintf("WHERE area_id = '%s'", gsub("[^A-Za-z0-9_.-]", "", as.character(area_id)))
  db_query(sprintf("
    SELECT kind, area_id, geography, code, name, addresses, people, households, over65, people_rs_high_medium, share_of_area_people
    FROM %s
    %s
    ORDER BY kind, area_id, geography, people DESC
  ", TBL_AREA_GEOGRAPHIES, where))
}

# fetch_area_overlap_pairs(): every pair of areas of one kind that share people (a few thousand rows), for working out the people in a set of
#   areas without counting a shared person twice.
fetch_area_overlap_pairs <- function() {
  db_query(sprintf("SELECT kind, area_a AS area_id_a, area_b AS area_id_b, addresses, people FROM %s", TBL_AREA_OVERLAPS))
}

fetch_area_overlaps <- function(area_id) {
  id <- gsub("[^A-Za-z0-9_.-]", "", as.character(area_id))
  db_query(sprintf("
    SELECT o.kind, o.area_a AS area_id_a, o.area_b AS area_id_b, o.addresses, o.people,
           a.area_name AS name_a, b.area_name AS name_b
    FROM %s o
    LEFT JOIN %s a ON a.kind = o.kind AND a.area_id = o.area_a
    LEFT JOIN %s b ON b.kind = o.kind AND b.area_id = o.area_b
    WHERE o.area_a = '%s' OR o.area_b = '%s'
    ORDER BY o.people DESC
  ", TBL_AREA_OVERLAPS, TBL_AREA_EXPOSURE, TBL_AREA_EXPOSURE, id, id))
}


# fetch_area_coverage(): three rows (all, flood_warning, flood_alert): distinct people covered, and the
#   People in a High or Medium band that no area of the kind covers.
# fetch_constituency_exposure(): per constituency, people, people at risk, and how many a warning area and
#   an alert area cover. The code is the NSUL parliamentary constituency code; names come from the constituencies table.

fetch_area_coverage <- function() {
  db_query(sprintf("
    SELECT kind, addresses, people, households, people_rs_high_medium,
           people_not_in_area, people_high_medium_not_in_area
    FROM %s
  ", TBL_AREA_COVERAGE))
}

fetch_constituency_exposure <- function() {
  db_query(sprintf("
    SELECT e.constituency_code, c.name AS constituency_name,
           e.people, e.people_rs_high_medium, e.people_fz3,
           e.people_rs_high_medium_in_warning_area, e.people_rs_high_medium_in_alert_area,
           e.people_high_medium_not_in_warning_area, e.people_high_medium_not_in_alert_area
    FROM %s e
    LEFT JOIN %s c ON c.constituency_id = e.constituency_code
  ", TBL_CONST_EXPOSURE, TBL_CONSTITUENCIES))
}


# fetch_risk_grid(): the 100 m cells inside a longitude and latitude box with a score at or above min_score, highest first, at most
#   `limit` of them. Read from exposure_grid_100m by easting and northing. Only for a map in view: the table holds all of England.

# step is the cell size to read at, in metres. At more than 100 the cells are grouped into larger squares: people and critical sites add up,
# the worst band and the highest score win, and the most deprived decile is kept. fetch_risk_grid_count() says how many 100 m cells a box holds,
# so the caller can choose a step that keeps the map light.
fetch_risk_grid <- function(west, south, east, north, min_score = 10, limit = GRID_CELL_LIMIT, step = 100) {
  b <- bbox_to_bng(west, south, east, north)
  step <- as.integer(step)
  if (step <= 100L) {
    return(db_query(sprintf("
      SELECT easting, northing, people, imd_decile, haz_rs, critical_sites, pop_pct, risk_score
      FROM %s
      WHERE easting BETWEEN %.0f AND %.0f
        AND northing BETWEEN %.0f AND %.0f
        AND risk_score >= %.2f
      ORDER BY risk_score DESC
      LIMIT %d
    ", TBL_EXPOSURE_GRID, b$xmin, b$xmax, b$ymin, b$ymax, as.numeric(min_score), as.integer(limit))))
  }
  db_query(sprintf("
    SELECT CAST(FLOOR(easting / %d) * %d AS BIGINT) AS easting, CAST(FLOOR(northing / %d) * %d AS BIGINT) AS northing,
           SUM(people) AS people, MIN(NULLIF(imd_decile, 0)) AS imd_decile, MAX(haz_rs) AS haz_rs,
           SUM(critical_sites) AS critical_sites, MAX(pop_pct) AS pop_pct, MAX(risk_score) AS risk_score
    FROM %s
    WHERE easting BETWEEN %.0f AND %.0f
      AND northing BETWEEN %.0f AND %.0f
    GROUP BY 1, 2
    HAVING MAX(risk_score) >= %.2f
    ORDER BY risk_score DESC
    LIMIT %d
  ", step, step, step, step, TBL_EXPOSURE_GRID, b$xmin, b$xmax, b$ymin, b$ymax, as.numeric(min_score), as.integer(limit)))
}

fetch_risk_grid_count <- function(west, south, east, north) {
  b <- bbox_to_bng(west, south, east, north)
  as.numeric(db_query(sprintf("
    SELECT COUNT(*) AS n FROM %s
    WHERE easting BETWEEN %.0f AND %.0f AND northing BETWEEN %.0f AND %.0f
  ", TBL_EXPOSURE_GRID, b$xmin, b$xmax, b$ymin, b$ymax))$n[1L])
}


# -----------------------------------------------------------------------------
# Hydrometrics (Environment Agency real-time data)
#
# fetch_rt_register(): one row per measure (what is measured, where, on which river), with the real
#   gauge name, FFFS id and WISKI id from ea_rt_measures_mapped where job 16 has built it.
# fetch_rt_readings(): readings for chosen measures, the archive and the live tail as
#   one series (job 23's ea_rt_readings_all), for the last `days` days. Measure URIs are
#   reduced to URL characters before they reach the SQL.
# fetch_rt_boundary(): midnight UTC at the end of the last archived day; readings after
#   it are live and not yet archived (job 10).
# -----------------------------------------------------------------------------

fetch_rt_register <- function() {
  # The register with each measure's real gauge name, FFFS id and WISKI id (job 16) where that has been built.
  # A missing view is not an error: the plain register is used and the generic names stay.
  tryCatch(db_query(sprintf("SELECT * FROM %s", TBL_RT_MAPPED)), error = function(e) {
    if (is.null(missing_table_info(conditionMessage(e)))) stop(e)
    db_query(sprintf("SELECT * FROM %s", TBL_RT_REGISTER))
  })
}

# fetch_rain_gauges(): every rainfall measure (a gauge with only an easting and northing is kept, and one with no position
# is returned so it can be counted), the newest reading it has, and its rainfall over the
# past 1, 12 and 24 hours, in one query. Names, FFFS and WISKI ids come from ea_rt_measures_mapped where job 16 has
# built it, else the plain register is used. A gauge can have two measures (an instantaneous reading and a 15-minute
# total); the app chooses one per gauge, so both are returned here. A sum over no readings is NULL, so "no readings
# in the window" stays different from "0 mm".
# max_mm: a single reading above this (or below zero) is an error, not rain, and is left out of the totals and counted in
# n_bad. 100 mm in one 15-minute reading is beyond any UK record. Keep in step with RAIN_MAX_READING in helpers.R.
rain_gauges_sql <- function(src, mapped, max_mm = 100) sprintf("
  WITH g AS (
    SELECT measure_uri, station_uri, station_reference, station_label, river_name, town, ea_area_name,
           lat, long, easting, northing, grid_reference, unit_name, value_type, period, parameter, %s
    FROM %s
    WHERE parameter = 'rainfall'
  ),
  w AS (
    SELECT r.measure_uri,
           SUM(CASE WHEN r.value BETWEEN 0 AND %s AND r.date_time >= current_timestamp() - INTERVAL 1 HOURS  THEN r.value END) AS mm_1h,
           SUM(CASE WHEN r.value BETWEEN 0 AND %s AND r.date_time >= current_timestamp() - INTERVAL 12 HOURS THEN r.value END) AS mm_12h,
           SUM(CASE WHEN r.value BETWEEN 0 AND %s THEN r.value END) AS mm_24h,
           COUNT(CASE WHEN r.value BETWEEN 0 AND %s THEN 1 END) AS n_24h,
           COUNT(CASE WHEN r.value < 0 OR r.value > %s THEN 1 END) AS n_bad
    FROM %s r JOIN g ON g.measure_uri = r.measure_uri
    WHERE r.reading_date >= date_sub(current_date(), 1)
      AND r.date_time >= current_timestamp() - INTERVAL 24 HOURS
    GROUP BY r.measure_uri
  )
  SELECT g.*, w.mm_1h, w.mm_12h, w.mm_24h, w.n_24h, w.n_bad, l.date_time AS last_time, l.value AS last_value
  FROM g
  LEFT JOIN w ON w.measure_uri = g.measure_uri
  LEFT JOIN %s l ON l.measure_uri = g.measure_uri",
  if (mapped) "gauge_name, fffs_id, wiski_id" else "CAST(NULL AS STRING) AS gauge_name, CAST(NULL AS STRING) AS fffs_id, CAST(NULL AS STRING) AS wiski_id",
  src, max_mm, max_mm, max_mm, max_mm, max_mm, TBL_RT_ALL, TBL_RT_LATEST)

# max_mm comes from the browser, so it is held to a number between 1 and 1000 before it reaches the SQL.
fetch_rain_gauges <- function(max_mm = 100) {
  max_mm <- suppressWarnings(as.numeric(max_mm)[1L])
  if (is.na(max_mm) || !is.finite(max_mm)) max_mm <- 100
  max_mm <- min(max(max_mm, 1), 1000)
  tryCatch(db_query(rain_gauges_sql(TBL_RT_MAPPED, TRUE, max_mm)), error = function(e) {
    info <- missing_table_info(conditionMessage(e))
    if (is.null(info) || !grepl("ea_rt_measures_mapped", info$table, fixed = TRUE)) stop(e)
    db_query(rain_gauges_sql(TBL_RT_REGISTER, FALSE, max_mm))
  })
}

fetch_rt_readings <- function(measure_uris, days, sum_uris = character()) {
  clean <- function(x) { x <- gsub("[^A-Za-z0-9:/._#-]", "", as.character(x)); x[nzchar(x)] }
  uris <- clean(measure_uris)
  if (length(uris) == 0L) return(data.table(measure_uri = character(), date_time = as.POSIXct(character(), tz = "UTC"), value = numeric()))
  quoted <- function(x) paste0("'", x, "'", collapse = ", ")
  bucket <- rt_bucket_seconds(days)
  if (bucket <= 900L) {
    # A short window: every reading as it is.
    d <- db_query(sprintf("
      SELECT measure_uri, date_time, value
      FROM %s
      WHERE measure_uri IN (%s)
        AND date_time >= current_timestamp() - INTERVAL %d DAYS
      ORDER BY measure_uri, date_time
    ", TBL_RT_ALL, quoted(uris), as.integer(days)))
  } else {
    # A long window: one point per bucket, so the chart stays quick. Level and flow are averaged;
    # rainfall (depth per reading) is summed so a bucket holds the rain that fell in it.
    sums <- clean(sum_uris)
    value_sql <- if (length(sums)) sprintf("CASE WHEN measure_uri IN (%s) THEN SUM(value) ELSE AVG(value) END", quoted(sums)) else "AVG(value)"
    d <- db_query(sprintf("
      SELECT measure_uri,
             timestamp_seconds(CAST(FLOOR(unix_timestamp(date_time) / %d) * %d AS BIGINT)) AS date_time,
             %s AS value
      FROM %s
      WHERE measure_uri IN (%s)
        AND date_time >= current_timestamp() - INTERVAL %d DAYS
      GROUP BY measure_uri, CAST(FLOOR(unix_timestamp(date_time) / %d) * %d AS BIGINT)
      ORDER BY measure_uri, date_time
    ", bucket, bucket, value_sql, TBL_RT_ALL, quoted(uris), as.integer(days), bucket, bucket))
  }
  d <- as.data.table(d)
  if (nrow(d)) d[, `:=`(date_time = as.POSIXct(date_time, tz = "UTC"), value = as.numeric(value))]
  attr(d, "bucket_seconds") <- bucket
  d
}

# The earliest reading held for these measures, so "All available" can size its window.
fetch_rt_span <- function(measure_uris) {
  uris <- gsub("[^A-Za-z0-9:/._#-]", "", as.character(measure_uris))
  uris <- uris[nzchar(uris)]
  if (length(uris) == 0L) return(NULL)
  d <- db_query(sprintf("SELECT MIN(date_time) AS t FROM %s WHERE measure_uri IN (%s)", TBL_RT_ALL,
                        paste0("'", uris, "'", collapse = ", ")))
  if (nrow(d) == 0L || is.na(d$t[1L])) NULL else as.POSIXct(d$t[1L], tz = "UTC")
}

fetch_rt_boundary <- function() {
  d <- db_query(sprintf("SELECT MAX(reading_date) AS d FROM %s", TBL_RT_BRONZE))
  rt_boundary(if (nrow(d)) d$d[1L] else NULL)
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
# fetch_constituency_intersections()
#
# One statement's constituency intersection rows, all five days. They give the
# Constituency situation tab its flood source filter and its impact and likelihood
# columns (see situation_detail() in helpers.R), for the latest statement and for
# past ones. The look-back below also rebuilds past statements from them. This is a
# live feature, not POC-only.
# -----------------------------------------------------------------------------

fetch_constituency_intersections <- function(statement_id) {
  db_query(sprintf("
    SELECT constituency_id, constituency_name, forecast_date, day_index, source, risk_level,
           risk_x, risk_y, impact_label, likelihood_label, intersection_pct
    FROM %s
    WHERE statement_id    = %d
      AND geometry_source = '%s'
  ", TBL_CONST_INTERSECT, as.integer(statement_id), GEOMETRY_SOURCE))
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
with_cache <- function(fn, name, ttl = CACHE_TTL_SECONDS, group = NULL) {
  force(fn); force(name); force(ttl); force(group)
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
    # A group (for example the map geometry) has its own, smaller rolling cap:
    # drop the oldest of that group until there is room for this entry.
    if (!is.null(group)) {
      repeat {
        gk <- Filter(function(k) identical(.fgs_cache[[k]]$group, group) && !identical(k, key),
                     ls(.fgs_cache, all.names = TRUE))
        if (length(gk) < GEOMETRY_CACHE_ENTRIES) break
        gages <- vapply(gk, function(k) as.numeric(.fgs_cache[[k]]$at), numeric(1))
        rm(list = gk[which.min(gages)], envir = .fgs_cache)
      }
    }
    assign(key, list(at = now, value = value, group = group), envir = .fgs_cache)
    if (is.data.table(value)) copy(value) else value
  }
}

# Statement-scoped queries and reference lookups. Not cached: the statement
# list, the health table and the signature, which must always be fresh.
fetch_risk_polygons                <- with_cache(fetch_risk_polygons,                "risk_polygons", group = "geometry")
fetch_ea_areas_for_statement       <- with_cache(fetch_ea_areas_for_statement,       "ea_areas")
fetch_summary_counts               <- with_cache(fetch_summary_counts,               "summary_counts")
fetch_ea_geometry                  <- with_cache(fetch_ea_geometry,                  "ea_geometry", group = "geometry")
fetch_constituency_intersections   <- with_cache(fetch_constituency_intersections,   "const_intersections")
fetch_constituency_geometry        <- with_cache(fetch_constituency_geometry,        "const_geometry", group = "geometry")
fetch_constituencies_for_statement <- with_cache(fetch_constituencies_for_statement, "const_meta")
fetch_changes                      <- with_cache(fetch_changes,                      "changes")
fetch_rain_gauges                  <- with_cache(fetch_rain_gauges,                  "rain_gauges", ttl = min(CACHE_TTL_SECONDS, 60))
fetch_potential_areas              <- with_cache(fetch_potential_areas,              "potential_areas")
fetch_changes_log                  <- with_cache(fetch_changes_log,                  "changes_log", ttl = min(CACHE_TTL_SECONDS, 60))
fetch_constituency_shape           <- with_cache(fetch_constituency_shape,           "const_shape", group = "geometry")
fetch_all_constituencies           <- with_cache(fetch_all_constituencies,           "all_constituencies", ttl = max(CACHE_TTL_SECONDS, 0) * 6)  # POC-ONLY
fetch_mp_lookup                    <- with_cache(fetch_mp_lookup,                    "mp_lookup")                                                 # POC-ONLY
fetch_situation_for_statement      <- with_cache(fetch_situation_for_statement,      "situation_for_statement")                                   # POC-ONLY
fetch_constituency_risk_geometry   <- with_cache(fetch_constituency_risk_geometry,   "const_risk_geometry", ttl = min(CACHE_TTL_SECONDS, 120), group = "geometry")

# Fresh on every call (these are reloaded each poll), but a missing table is remembered.
fetch_pipeline_health              <- with_cache(fetch_pipeline_health,              "health",    ttl = 0)
fetch_situation                    <- with_cache(fetch_situation,                    "situation", ttl = 0)
fetch_briefing_drafts              <- with_cache(fetch_briefing_drafts,              "briefings", ttl = 0)
fetch_exposure                     <- with_cache(fetch_exposure,                     "exposure",  ttl = 0)
fetch_warning_history              <- with_cache(fetch_warning_history,              "warning_history", ttl = 0)
fetch_warning_episodes             <- with_cache(fetch_warning_episodes,             "warning_episodes", ttl = min(CACHE_TTL_SECONDS, 300))
fetch_coverage_summary             <- with_cache(fetch_coverage_summary,             "cov_summary", ttl = max(CACHE_TTL_SECONDS, 0) * 6)
fetch_coverage_warnings            <- with_cache(fetch_coverage_warnings,            "cov_warnings", ttl = max(CACHE_TTL_SECONDS, 0) * 6)
fetch_coverage_detail              <- with_cache(fetch_coverage_detail,              "cov_detail", ttl = max(CACHE_TTL_SECONDS, 0) * 6)
fetch_coverage_span                <- with_cache(fetch_coverage_span,                "cov_span", ttl = max(CACHE_TTL_SECONDS, 0) * 6)
fetch_area_exposure                <- with_cache(fetch_area_exposure,                "area_exposure", ttl = max(CACHE_TTL_SECONDS, 0) * 6)
fetch_area_shape                   <- with_cache(fetch_area_shape,                   "area_shape", group = "geometry")
fetch_area_constituency_links      <- with_cache(fetch_area_constituency_links,      "area_cons_links", ttl = max(CACHE_TTL_SECONDS, 0) * 6)
fetch_area_sites                   <- with_cache(fetch_area_sites,                   "area_sites", ttl = max(CACHE_TTL_SECONDS, 0) * 6)
fetch_area_geographies              <- with_cache(fetch_area_geographies,              "area_geographies", ttl = max(CACHE_TTL_SECONDS, 0) * 6)
fetch_area_overlap_pairs            <- with_cache(fetch_area_overlap_pairs,            "area_pairs", ttl = max(CACHE_TTL_SECONDS, 0) * 6)
fetch_area_episodes                <- with_cache(fetch_area_episodes,                "area_episodes", ttl = min(CACHE_TTL_SECONDS, 300))
fetch_area_overlaps                <- with_cache(fetch_area_overlaps,                "area_overlaps", ttl = max(CACHE_TTL_SECONDS, 0) * 6)
fetch_area_coverage                <- with_cache(fetch_area_coverage,                "area_coverage", ttl = max(CACHE_TTL_SECONDS, 0) * 6)
fetch_constituency_exposure        <- with_cache(fetch_constituency_exposure,        "const_exposure", ttl = max(CACHE_TTL_SECONDS, 0) * 6)
fetch_rt_register                  <- with_cache(fetch_rt_register,                  "rt_register", ttl = max(CACHE_TTL_SECONDS, 0) * 6)
fetch_rt_boundary                  <- with_cache(fetch_rt_boundary,                  "rt_boundary", ttl = min(CACHE_TTL_SECONDS, 300))
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
