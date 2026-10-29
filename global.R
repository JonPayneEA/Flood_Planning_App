# =============================================================================
# global.R
# Runs once when the app starts, before any session begins.
#
# Responsibilities:
#   - Library loads
#   - Environment variable validation
#   - Shared constants (colour palette, risk matrix metadata)
#
# Anything defined here is visible to app_ui.R and app_server.R via lexical scoping.
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

# -----------------------------------------------------------------------------
# Demo mode.
#
# FGS_DEMO_MODE=1 runs the whole app on generated data (demo.R). It never opens
# a warehouse connection: brickster and DBI are not even loaded, no credentials
# are needed, and db_query() refuses to run. Nothing is read from or written to
# any warehouse. It exists so the tool can be shown, and tried, by people who
# have no access to the live data.
# -----------------------------------------------------------------------------

DEMO_MODE <- tolower(Sys.getenv("FGS_DEMO_MODE", unset = "")) %in% c("1", "true", "yes", "on")

library(shiny)
library(bslib)        # Bootstrap 5 layouts -- needed for the CSS we use
library(data.table)
library(sf)
library(leaflet)
# The live data path only. Demo mode must not need (or touch) any of these.
if (!DEMO_MODE) {
  library(brickster)  # DBI driver for Databricks SQL warehouses (no ODBC needed)
  library(DBI)
  library(geojsonsf)  # Parse GeoJSON strings to sf objects (storage format
                      # used by the FGS pipeline -- GeoJSON in Delta all the
                      # way through to Leaflet)
}
library(htmltools)
library(DT)           # Sortable, searchable tables for the constituency views


# -----------------------------------------------------------------------------
# Query progress pop-ups.
#
# brickster reports every warehouse call with cli progress steps ("Submitting query",
# "Fetching metadata" and the like). While a Shiny app is running, cli draws each step as a
# Shiny progress pop-up, with its terminal colour codes showing, so every query put a stack
# of boxes on screen. Setting cli.progress_handlers_only to nothing turns them all off. Set
# FGS_SHOW_QUERY_PROGRESS=1 to see them again (for example when a query seems stuck).
# -----------------------------------------------------------------------------

if (is.null(getOption("cli.progress_handlers_only")) &&
    !tolower(Sys.getenv("FGS_SHOW_QUERY_PROGRESS", unset = "")) %in% c("1", "true", "yes", "on")) {
  options(cli.progress_handlers_only = character())
}


# -----------------------------------------------------------------------------
# Local .env loading.
#
# Silent no-op when .env is missing, so the guard is belt-and-braces. On
# Posit Connect the env vars come from the deployed app's Vars panel.
# -----------------------------------------------------------------------------

# Only when a .env file exists, and never needed in demo mode or on Connect.
if (file.exists(".env")) {
  if (!requireNamespace("dotenv", quietly = TRUE)) {
    stop("A .env file was found but the dotenv package is not installed. ",
         "Run install.packages(\"dotenv\"), or remove .env and set the variables another way.")
  }
  dotenv::load_dot_env()
}


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

missing_vars <- if (DEMO_MODE) character() else required_vars[!nzchar(Sys.getenv(required_vars))]
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

CAT <- Sys.getenv("FGS_CATALOG", unset = if (DEMO_MODE) "demo" else "")
SCH <- Sys.getenv("FGS_SCHEMA",  unset = if (DEMO_MODE) "demo" else "")
# The restricted schema holds the thresholds. Only the threshold lines on Hydrometrics read from it, and only when ticked.
RESTRICTED_SCH <- Sys.getenv("FGS_RESTRICTED_SCHEMA", unset = if (DEMO_MODE) "demo" else "flood_forecasting_restricted")

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

# Pipeline products (FGS_Notebooks jobs 18 to 22). Plain tables, no geometry.
TBL_CHANGES   <- paste(CAT, SCH, "fgs_constituency_changes",    sep = ".")  # 18
TBL_CHANGES_LOG <- paste(CAT, SCH, "fgs_changes_log",             sep = ".")  # 18 (one row per comparison made)
TBL_SITUATION <- paste(CAT, SCH, "constituency_flood_situation", sep = ".")  # 19
TBL_BRIEFINGS <- paste(CAT, SCH, "mp_briefing_drafts",          sep = ".")  # 20
TBL_FLOOD_WARN_CURRENT <- paste(CAT, SCH, "flood_warnings_current", sep = ".")  # 11 (the live feed: one row per area in force)
TBL_EXPOSURE  <- paste(CAT, SCH, "fgs_constituency_exposure",   sep = ".")  # 21
TBL_HEALTH    <- paste(CAT, SCH, "pipeline_health",             sep = ".")  # 22
TBL_RT_REGISTER <- paste(CAT, SCH, "ea_rt_measure_register",  sep = ".")  # 09 (what each gauge measures)
TBL_RT_MAPPED   <- paste(CAT, SCH, "ea_rt_measures_mapped",    sep = ".")  # 16 (the register with gauge names, FFFS and WISKI ids; optional)
TBL_RT_ALL      <- paste(CAT, SCH, "ea_rt_readings_all",      sep = ".")  # 23 (archive and live tail as one series)
TBL_RT_LATEST   <- paste(CAT, SCH, "ea_rt_latest",            sep = ".")  # 23 (the newest reading of each measure)
TBL_RT_BRONZE   <- paste(CAT, SCH, "ea_rt_readings_bronze",   sep = ".")  # 10 (the archive; its last day marks where live begins)
TBL_WARNING_FGS <- paste(CAT, SCH, "ea_warning_fgs_coverage", sep = ".")  # 26 (each warning, and whether and when an FGS polygon covered it)
TBL_FW_THRESHOLDS <- paste(CAT, RESTRICTED_SCH, "ea_fw_threshold_gauge_links", sep = ".")  # 28 (restricted: Observed flood warning thresholds linked to gauge measures)
TBL_AREA_EXPOSURE <- paste(CAT, RESTRICTED_SCH, "ea_area_exposure",         sep = ".")  # 35 (restricted: people, households and bands inside each warning or alert area)
TBL_AREA_OVERLAPS <- paste(CAT, RESTRICTED_SCH, "ea_area_overlaps",         sep = ".")  # 35 (restricted: pairs of areas of one kind that share people)
TBL_EXPOSURE_GRID <- paste(CAT, RESTRICTED_SCH, "exposure_grid_100m",       sep = ".")  # 34 (restricted: one row per 100 m cell with people, bands, critical sites and the risk score)
TBL_AREA_SITES    <- paste(CAT, RESTRICTED_SCH, "ea_area_sites",            sep = ".")  # 35 (restricted: each critical site inside each area, with its rivers and sea band)
TBL_AREA_GEOGRAPHIES <- paste(CAT, RESTRICTED_SCH, "ea_area_geographies",       sep = ".")  # 35 (restricted: each area split by district, county, ward, constituency, police force and region)
TBL_CONST_SITES   <- paste(CAT, RESTRICTED_SCH, "constituency_sites",        sep = ".")  # 35 (restricted: each critical site inside each England constituency, with its band)
TBL_AREA_COVERAGE <- paste(CAT, RESTRICTED_SCH, "ea_area_coverage",         sep = ".")  # 35 (restricted: people covered by any warning area, any alert area, and in all)
TBL_CONST_EXPOSURE <- paste(CAT, RESTRICTED_SCH, "constituency_flood_exposure", sep = ".")  # 35 (restricted: people by constituency, and how many a warning or alert area covers)
TBL_WARNING_EPISODES <- paste(CAT, SCH, "ea_warning_episodes",      sep = ".")  # 25 (each alert or warning from issue to withdrawal)


# -----------------------------------------------------------------------------
# Geometry source.
#
# The pipeline intersects either the risk-area polygons or the Areas of
# Concern (FGS_GEOMETRY_SOURCE in FGS_Notebooks/config.py) and tags every
# intersection row with `geometry_source`. The two can coexist in the same
# table, so the app must read only the source the pipeline is currently
# built on, or counts double. Set FGS_GEOMETRY_SOURCE to the same value as
# the pipeline. Optional: defaults to "risk_areas", the pipeline default.
# -----------------------------------------------------------------------------

GEOMETRY_SOURCE <- Sys.getenv("FGS_GEOMETRY_SOURCE", unset = "risk_areas")
if (!GEOMETRY_SOURCE %in% c("risk_areas", "aoc")) {
  stop("FGS_GEOMETRY_SOURCE must be 'risk_areas' or 'aoc', not '",
       GEOMETRY_SOURCE, "'.")
}


# -----------------------------------------------------------------------------
# Refresh, staleness and cache settings. All optional.
#
#   FGS_AUTO_REFRESH_SECONDS  How often an open session checks for new data.
#                             Default 120. 0 turns auto-refresh off.
#   FGS_STALE_HOURS           Hours without a pipeline health update before the
#                             stale-data banner shows. Default 3 (the pipeline
#                             polls every 30 or 60 minutes).
#   FGS_POC_FEATURES          1 (default) keeps the proof-of-concept features.
#                             0 removes them. See POC_FEATURES below.
#   FGS_CONTACT_MIN_PCT       Coverage a polygon needs to count (default 0.01).
#   FGS_QUERY_RETRIES         Retries after a network failure (default 2, max 5).
#   FGS_CACHE_SECONDS         How long query results are reused across
#                             sessions. Default 600. 0 turns the cache off.
# -----------------------------------------------------------------------------

env_number <- function(name, default, min = 0) {
  raw <- Sys.getenv(name, unset = as.character(default))
  val <- suppressWarnings(as.numeric(raw))
  if (is.na(val) || val < min) {
    stop(name, " must be a number of at least ", min, ", not '", raw, "'.")
  }
  val
}

# POC-ONLY: proof-of-concept features that should not ship in the live tool.
# FGS_POC_FEATURES=0 switches every one of them off. Today that is the statement
# look-back on the Constituency situation tab. Each piece of code behind this
# switch is tagged "POC-ONLY", so `grep -rn POC-ONLY .` lists everything to
# remove when the app goes live. Default on, because this is a proof of concept.
POC_FEATURES <- tolower(Sys.getenv("FGS_POC_FEATURES", unset = "1")) %in% c("1", "true", "yes", "on")

# The four nations, by the first three characters of a constituency's ONS code. The data holds
# every nation, because the Environment Agency's flood areas run up to the borders, and a
# "Nations" filter at the top of the app chooses which constituencies are shown.
# FGS_DEFAULT_NATIONS sets the starting choice (default England only). A comma separated list
# of codes, for example FGS_DEFAULT_NATIONS=E14,W07. An empty selection in the app shows all four.
NATIONS <- c(England = "E14", Wales = "W07", Scotland = "S14", "Northern Ireland" = "N06")

DEFAULT_NATIONS <- local({
  p <- trimws(strsplit(Sys.getenv("FGS_DEFAULT_NATIONS", unset = "E14"), ",", fixed = TRUE)[[1L]])
  p <- p[nzchar(p)]
  if (length(p) == 0L || any(!(p %in% NATIONS))) {
    stop("FGS_DEFAULT_NATIONS must be a comma separated list of ", paste(NATIONS, collapse = ", "), ".")
  }
  p
})

# Share of a constituency a polygon must cover before it counts. Match the
# pipeline's FGS_CONTACT_MIN_PCT in config.py (0.01 = 1%). Used only to rebuild
# the situation table for a past statement (POC-ONLY look-back).
CONTACT_MIN_PCT <- env_number("FGS_CONTACT_MIN_PCT", 0.01)

# A flood warning or alert area counts as "under the forecast" for a constituency
# when at least this share of the AREA lies inside the constituency. Mirrors the
# pipeline's WARNING_CONTACT_MIN_PCT in config.py (0.01 = 1%).
# The newest change merged into this app (the pull request number and date), shown in the footer with how shared links are saved, so what is running on
# Connect can be told from a glance. Update it with each merge to main.
APP_BUILD <- "PR 95, 29 Oct 2026"

TARGET_AREA_MIN_PCT <- env_number("FGS_WARNING_CONTACT_MIN_PCT", 0.01)

AUTO_REFRESH_SECONDS <- env_number("FGS_AUTO_REFRESH_SECONDS", 120)
if (AUTO_REFRESH_SECONDS > 0 && AUTO_REFRESH_SECONDS < 30) {
  stop("FGS_AUTO_REFRESH_SECONDS must be 0 (off) or at least 30.")
}
STALE_HOURS       <- env_number("FGS_STALE_HOURS", 3, min = 0.1)
CACHE_TTL_SECONDS <- env_number("FGS_CACHE_SECONDS", 600)

# Map geometry is the heavy part of the shared cache: one entry is one layer for
# one statement and day, and holds every polygon. The cache keeps only the newest
# this many geometry entries and drops the oldest, so memory stays bounded however
# much people browse. Light results are limited separately, by count (200).
GEOMETRY_CACHE_ENTRIES <- as.integer(env_number("FGS_CACHE_GEOMETRY_ENTRIES", 30, min = 1))

# How many times a query is retried after a network failure (not after a SQL
# error, which is never retried). 0 turns retrying off. Each retry waits 1.5 s,
# then 3 s, and the wait blocks the app's R process, so keep this small.
DB_RETRIES <- as.integer(env_number("FGS_QUERY_RETRIES", 2))
if (DB_RETRIES > 5L) stop("FGS_QUERY_RETRIES must be between 0 and 5.")


# -----------------------------------------------------------------------------
# FGS colour palette.
#
# Hex values taken from the FGS User Guide Table 1. Named character vector
# so colour lookups read as RISK_COLOUR_HEX[<colour_label>]. The CSS classes
# in app_ui.R use the same names (Red/Amber/Yellow/Green) for the swatches in
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
  flood_warning = "#BE0553",   # EA brand Pantone 227
  flood_alert   = "#54BCE7"    # EA brand Pantone 2985
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

# Axis labels for the matrix UI, indexed by x and y respectively (1-based).
# Aligned with the FGS Public API documentation as recorded in the pipeline
# README (risk matrix labels).
IMPACT_LABELS     <- c("Minor", "Moderate", "Significant", "Extreme")
LIKELIHOOD_LABELS <- c("Low", "Medium", "High", "Very High")
# Probability band behind each likelihood label (FGS Public API documentation).
LIKELIHOOD_BANDS  <- c("10-25%", "25-50%", "50-75%", ">75%")
# Overall risk level for each colour band, as in the pipeline's RISK_MATRIX.
RISK_LEVEL_BY_COLOUR <- c(Green = "Very Low", Yellow = "Low", Amber = "Medium", Red = "High")

# Forecast risk rank used by the constituency products (jobs 18 to 21).
# 0 means nothing flagged. Colours reuse the FGS band palette.
RANK_LABELS <- c("None", "Very Low", "Low", "Medium", "High")
RANK_HEX    <- c("#e9ecef", "#4f9d4f", "#f0d040", "#e08020", "#b02020")

