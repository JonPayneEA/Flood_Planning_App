# =============================================================================
# global.R
# Runs once when the app starts, before any session begins.
#
# Responsibilities:
#   - Library loads
#   - Environment variable validation
#   - Shared constants (colour palette, risk matrix metadata)
#
# Anything defined here is visible to ui.R and server.R via lexical scoping.
#
# CONNECTION STRATEGY:
#   brickster provides a DBI driver that wraps the Databricks SQL Statement
#   Execution API. Each query opens a connection via dbConnect() in data.R
#   and closes it on exit -- the helper handles auth, paging, and result
#   conversion. No long-lived connection here, unlike the sparklyr version,
#   because the underlying REST API has no setup cost worth amortising.
# =============================================================================


# -----------------------------------------------------------------------------
# Library loads.
#
# data.table for in-memory work, brickster for the DBI driver against the
# Databricks SQL Statement Execution API, sf for polygon geometry, leaflet
# for the map, htmltools for the custom HTML chunks.
#
# No sparklyr, no pysparklyr, no reticulate, no Python dependency at all.
# This is the lighter route.
# -----------------------------------------------------------------------------

library(shiny)
library(bslib)        # Bootstrap 5 layouts -- needed for the CSS we use
library(data.table)
library(brickster)    # DBI driver for Databricks SQL warehouses (no ODBC needed)
library(DBI)
library(sf)
library(geojsonsf)    # Parse GeoJSON strings to sf objects (storage format
# used by the FGS pipeline -- GeoJSON in Delta all the
# way through to Leaflet)
library(leaflet)
library(htmltools)
library(dotenv)


# -----------------------------------------------------------------------------
# Local .env loading.
#
# Silent no-op when .env is missing, so the guard is belt-and-braces. On
# Posit Connect the env vars come from the deployed app's Vars panel.
# -----------------------------------------------------------------------------

if (file.exists(".env")) load_dot_env()


# -----------------------------------------------------------------------------
# Validate required environment variables.
#
# Fail loudly at startup rather than letting a half-configured app crash
# on first user interaction.
# -----------------------------------------------------------------------------

required_vars <- c(
  "DATABRICKS_HOST",          # workspace URL with https:// prefix
  "DATABRICKS_TOKEN",         # personal access token
  "DATABRICKS_WAREHOUSE_ID",  # SQL warehouse identifier (NOT a cluster id)
  "FGS_CATALOG",              # Unity Catalog name
  "FGS_SCHEMA"                # Schema within that catalog
)

missing_vars <- required_vars[!nzchar(Sys.getenv(required_vars))]
if (length(missing_vars) > 0L) {
  stop(
    "Missing required environment variables: ",
    paste(missing_vars, collapse = ", "),
    ". Copy .env.example to .env and fill in the values, or set them in the ",
    "Posit Connect Vars panel."
  )
}


# -----------------------------------------------------------------------------
# Three-part Unity Catalog table identifiers.
#
# Built once at startup so every query uses the same names. Catalog and
# schema both come from env vars; nothing about the table location lives
# in source.
# -----------------------------------------------------------------------------

CAT <- Sys.getenv("FGS_CATALOG")
SCH <- Sys.getenv("FGS_SCHEMA")

TBL_STATEMENTS      <- paste(CAT, SCH, "fgs_statements",                 sep = ".")
TBL_RISK_POLYGONS   <- paste(CAT, SCH, "fgs_risk_polygons",              sep = ".")
TBL_EA_INTERSECT    <- paste(CAT, SCH, "fgs_ea_area_intersections",      sep = ".")
TBL_CONST_INTERSECT <- paste(CAT, SCH, "fgs_constituency_intersections", sep = ".")

# Geometry source tables. The intersection tables hold the join records
# (which polygon hits which EA area or constituency) but not the polygon
# shapes themselves. To draw the EA layer or the constituency layer we
# join back to these for the geometry.
TBL_EA_FWA          <- paste(CAT, SCH, "ea_flood_warning_areas",         sep = ".")
TBL_EA_FAA          <- paste(CAT, SCH, "ea_flood_alert_areas",           sep = ".")
TBL_CONSTITUENCIES  <- paste(CAT, SCH, "parliamentary_constituencies",   sep = ".")

# Reference tables for the EA Warnings export (MP details + constituency per
# area). Joined in fetch_ea_constituency_mp() on constituency_name -- the two
# tables share no ID column.
TBL_EA_CONSTITUENCY_LOOKUP <- paste(CAT, SCH, "ea_area_constituency_lookup", sep = ".")
TBL_MP_CONTACTS            <- paste(CAT, SCH, "mp_contact_details",         sep = ".")


# -----------------------------------------------------------------------------
# FGS colour palette.
#
# Hex values taken from the FGS User Guide Table 1. Named character vector
# so colour lookups read as RISK_COLOUR_HEX[<colour_label>]. The CSS classes
# in ui.R use the same names (Red/Amber/Yellow/Green) for the swatches in
# the right-hand panel.
# -----------------------------------------------------------------------------

RISK_COLOUR_HEX <- c(
  red    = "#b02020",
  amber  = "#e08020",
  yellow = "#f0d040",
  green  = "#4f9d4f"
)


# -----------------------------------------------------------------------------
# EA flood area palette.
#
# Fixed colours, independent of risk band. The EA layer is context, not the
# primary signal -- FGS polygons carry the risk colour. FWA and FAA get
# their own fixed hues so the two area types stay visually distinct from
# each other and from the FGS layer regardless of which risk band they sit
# in. Keyed on the pipeline's ea_area_type values (flood_warning/flood_alert).
# -----------------------------------------------------------------------------

EA_AREA_TYPE_HEX <- c(
  flood_warning = "#1f4f7a",   # dark blue
  flood_alert   = "#7a4f9d"    # purple
)


# -----------------------------------------------------------------------------
# Risk matrix metadata.
#
# Sixteen cells in the 4x4 FGS matrix. x is impact (1 = Minimal, 4 = Severe),
# y is likelihood (1 = Very Low, 4 = High). Each cell resolves to one of the
# four colour bands per Table 1.
# -----------------------------------------------------------------------------

RISK_MATRIX <- data.table(
  x      = c(1, 2, 3, 4,   1, 2, 3, 4,   1, 2, 3, 4,   1, 2, 3, 4),
  y      = c(4, 4, 4, 4,   3, 3, 3, 3,   2, 2, 2, 2,   1, 1, 1, 1),
  colour = c("Green",  "Yellow", "Amber",  "Red",
             "Green",  "Yellow", "Amber",  "Amber",
             "Green",  "Green",  "Yellow", "Amber",
             "Green",  "Green",  "Yellow", "Yellow")
)
