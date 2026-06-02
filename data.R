# =============================================================================
# data.R
# All data access for the dashboard.
#
# One function per query. Each function opens a fresh ODBC connection, runs
# one statement, and closes the connection on exit. We do not hold a long-
# lived connection because Posit Connect can suspend idle apps and break held
# connections silently -- when the user comes back, the next query fails in
# an opaque way. Per-query connect costs a few hundred milliseconds and is
# worth the predictability.
#
# All returned tables are data.tables. Geometry comes back as an sf object so
# leaflet can plot it directly without conversion in server.R.
#
# Environment variables required:
#   DATABRICKS_SERVER_HOSTNAME    hostname of the workspace
#   DATABRICKS_HTTP_PATH          HTTP path of the SQL warehouse
#   DATABRICKS_TOKEN              personal access token (or service principal)
#   FGS_CATALOG, FGS_SCHEMA       Unity Catalog location of the FGS tables
# =============================================================================


# -----------------------------------------------------------------------------
# db_connection()
#
# Opens an ODBC connection to a Databricks SQL warehouse using Posit's first-
# party helper. odbc::databricks() handles the driver lookup and Spark-on-ODBC
# quirks that would otherwise be a manual dsn-less connection string.
#
# Caller is responsible for closing the connection. Helpers below use
# on.exit(dbDisconnect(...)) to guarantee that.
# -----------------------------------------------------------------------------

db_connection <- function() {
  DBI::dbConnect(
    odbc::databricks(),
    httpPath  = Sys.getenv("DATABRICKS_HTTP_PATH"),
    workspace = paste0("https://", Sys.getenv("DATABRICKS_SERVER_HOSTNAME")),
    token     = Sys.getenv("DATABRICKS_TOKEN")
  )
}


# -----------------------------------------------------------------------------
# db_query()
#
# Convenience wrapper: connect, query, disconnect, return a data.table.
# Parameters are bound via DBI's positional ? placeholders -- safer than
# pasting into the SQL string, since DBI handles quoting and escaping.
#
# setDT() converts the data.frame returned by DBI into a data.table by
# reference, with no copy. Cheap, and means every query in this file returns
# the same type.
# -----------------------------------------------------------------------------

db_query <- function(sql, params = NULL) {
  con <- db_connection()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  result <- if (is.null(params)) {
    DBI::dbGetQuery(con, sql)
  } else {
    DBI::dbGetQuery(con, sql, params = params)
  }
  
  setDT(result)
  result[]    # The [] forces print-on-return, useful when debugging at REPL.
}


# =============================================================================
# QUERIES
#
# Each function below corresponds to one block of data the UI needs. Keeping
# them as named functions (rather than building queries inline in server.R)
# means each query can be lifted out and run by hand in Databricks SQL during
# debugging, and the SQL text lives in one place.
# =============================================================================


# -----------------------------------------------------------------------------
# fetch_latest_statement()
#
# Returns metadata for the most recently issued FGS statement, as a one-row
# named list. Used by the title banner ("Forecast Day 1 - issued 22 May 14:30")
# and to anchor every other query to a specific statement_id.
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
  as.list(dt[1L])    # data.table's [1L] returns the first row as a data.table;
  # as.list() unboxes it to a named list for caller convenience.
}


# -----------------------------------------------------------------------------
# fetch_risk_polygons()
#
# Returns the FGS risk polygons for a given statement and forecast day, as
# an sf object suitable for leaflet::addPolygons().
#
# Geometry is requested as WKT (ST_ASTEXT) rather than binary because:
#   - the volume per statement-day is small (tens of polygons, not millions)
#   - WKT survives ODBC trip without driver-specific binary handling
#   - sf::st_as_sf parses WKT natively
#
# The six risk matrix columns (risk_x, risk_y, impact_label, likelihood_label,
# risk_level, risk_colour) are pulled through directly so downstream filtering
# in server.R can work on the data.table representation before plotting.
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
      ST_ASTEXT(geometry) AS geometry_wkt
    FROM %s
    WHERE statement_id = ?
      AND day_index    = ?
  ", TBL_RISK_POLYGONS),
                 params = list(statement_id, day_index))
  
  # Empty result -- return an empty sf object so callers can pattern-match on
  # nrow() without separate NULL checks.
  if (nrow(dt) == 0L) {
    return(sf::st_sf(geometry = sf::st_sfc(), crs = 4326))
  }
  
  # sf::st_as_sf parses the WKT column and returns an sf object. CRS 4326
  # (WGS84 lat/lon) is the FGS GeoJSON default and the CRS leaflet expects.
  # We rename the parsed column to "geometry" so downstream code doesn't have
  # to remember the column was called geometry_wkt at fetch time.
  shape <- sf::st_as_sf(dt, wkt = "geometry_wkt", crs = 4326)
  sf::st_geometry(shape) <- "geometry"
  shape
}


# -----------------------------------------------------------------------------
# fetch_ea_areas_for_statement()
#
# Returns the EA flood areas (FWA/FAA) that intersect any risk polygon in the
# given statement-day, ordered by risk descending then intersection size.
#
# The right-hand panel reads top-down, so the SQL does the sort rather than
# leaving it to R -- the database is already touching the rows, and ORDER BY
# in SQL is essentially free at this volume.
#
# CASE statement on risk_colour rather than ordering by risk_level numerically
# because risk_level is text in the source table; using the colour ordinal
# directly keeps the sort independent of any future relabelling.
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
    WHERE statement_id = ?
      AND day_index    = ?
    ORDER BY
      CASE risk_colour
        WHEN 'Red'    THEN 1
        WHEN 'Amber'  THEN 2
        WHEN 'Yellow' THEN 3
        WHEN 'Green'  THEN 4
        ELSE 5
      END,
      intersection_pct DESC
  ", TBL_EA_INTERSECT),
           params = list(statement_id, day_index))
}


# -----------------------------------------------------------------------------
# fetch_summary_counts()
#
# Returns three headline counts (polygons, EA areas, constituencies affected)
# for the count cards at the top of the right-hand panel.
#
# One query with three correlated sub-selects rather than three separate round
# trips. ODBC overhead per round trip is roughly 50-100ms over a corporate
# network, so combining them visibly speeds up the initial render.
# -----------------------------------------------------------------------------

fetch_summary_counts <- function(statement_id, day_index) {
  dt <- db_query(sprintf("
    SELECT
      (SELECT COUNT(*)
         FROM %s WHERE statement_id = ? AND day_index = ?) AS polygon_count,
      (SELECT COUNT(DISTINCT ea_area_code)
         FROM %s WHERE statement_id = ? AND day_index = ?) AS ea_area_count,
      (SELECT COUNT(DISTINCT constituency_id)
         FROM %s WHERE statement_id = ? AND day_index = ?) AS constituency_count
  ", TBL_RISK_POLYGONS, TBL_EA_INTERSECT, TBL_CONST_INTERSECT),
                 params = list(statement_id, day_index,
                               statement_id, day_index,
                               statement_id, day_index))
  
  if (nrow(dt) == 0L) {
    return(list(polygon_count = 0L, ea_area_count = 0L, constituency_count = 0L))
  }
  as.list(dt[1L])
}
