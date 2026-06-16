# =============================================================================
# data.R
# All data access for the dashboard, via brickster's DBI backend.
#
# brickster wraps the Databricks SQL Statement Execution API. From R's
# perspective it's a standard DBI driver: dbConnect, dbGetQuery, dbDisconnect.
# Authentication uses DATABRICKS_HOST and DATABRICKS_TOKEN from the environment.
#
# One function per query. Each:
#   - opens a connection via brickster's DBI driver
#   - runs one SQL statement
#   - closes the connection on exit
# Per-query connections rather than a shared one because the REST API has no
# per-connection setup cost worth amortising, and per-query connections
# survive Posit Connect's idle-app suspension cleanly.
#
# All returned tables are data.tables. Geometry is parsed into sf before
# returning so leaflet can plot it directly.
# =============================================================================


# -----------------------------------------------------------------------------
# db_connection()
#
# Opens a DBI connection to the configured SQL warehouse. The warehouse_id
# argument tells brickster which warehouse to run the statement on; host
# and token are picked up automatically from DATABRICKS_HOST and
# DATABRICKS_TOKEN. We could pass them explicitly but env-var auth is the
# documented pattern and keeps credentials out of any error trace.
# -----------------------------------------------------------------------------

db_connection <- function() {
  DBI::dbConnect(
    brickster::DatabricksSQL(),
    warehouse_id = Sys.getenv("DATABRICKS_WAREHOUSE_ID")
  )
}


# -----------------------------------------------------------------------------
# db_query()
#
# Convenience wrapper: connect, run query, disconnect, return a data.table.
# setDT() converts the result by reference with no copy. The trailing []
# forces print-on-return, useful when calling these functions directly at
# the REPL during development.
#
# Note on parameterisation:
#   brickster's DBI implementation does not support the params = argument to
#   dbGetQuery() that standard DBI drivers expose. Values are interpolated
#   into the SQL string via sprintf() in each fetch_* function below. This
#   is safe for our queries because every bound value is an integer we
#   control (statement_id and day_index), never user input.
# -----------------------------------------------------------------------------

db_query <- function(sql) {
  con <- db_connection()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  result <- DBI::dbGetQuery(con, sql)
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
    WHERE statement_id = %d
      AND day_index    = %d
    ORDER BY
      CASE risk_colour
        WHEN 'Red'    THEN 1
        WHEN 'Amber'  THEN 2
        WHEN 'Yellow' THEN 3
        WHEN 'Green'  THEN 4
        ELSE 5
      END,
      intersection_pct DESC
  ", TBL_EA_INTERSECT, as.integer(statement_id), as.integer(day_index)))
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
         FROM %s WHERE statement_id = %d AND day_index = %d) AS ea_area_count,
      (SELECT COUNT(DISTINCT constituency_id)
         FROM %s WHERE statement_id = %d AND day_index = %d) AS constituency_count
  ",
                         TBL_RISK_POLYGONS,   sid, did,
                         TBL_EA_INTERSECT,    sid, did,
                         TBL_CONST_INTERSECT, sid, did))
  
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
      WHERE i.statement_id = %d
        AND i.day_index    = %d
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
                         TBL_EA_INTERSECT, sid, did,
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
# risk_colour/risk_level) so server.R can apply the same source, matrix-cell,
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
      WHERE i.statement_id = %d
        AND i.day_index    = %d
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
                         TBL_CONST_INTERSECT, sid, did,
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
    WHERE statement_id = %d
      AND day_index    = %d
  ", TBL_CONST_INTERSECT, as.integer(statement_id), as.integer(day_index)))
}


# -----------------------------------------------------------------------------
# fetch_ea_constituency_mp()
#
# Returns, for every EA flood area, the constituency(ies) it overlaps and
# that constituency's MP contact details. Backs the "Export CSV" button on
# the Affected EA Warnings panel.
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
