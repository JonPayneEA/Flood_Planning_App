# =============================================================================
# global.R
# Runs once when the app starts, before any session begins.
#
# Anything defined here is available to ui.R and server.R via the calling
# environment. Library loads, environment variables, shared constants, and
# the risk matrix metadata all live here so the rest of the app doesn't have
# to know where to find them.
# =============================================================================


# -----------------------------------------------------------------------------
# Library loads.
#
# The data layer is fastverse: data.table for in-memory manipulation, plus
# kit and collapse where their helpers are cleaner than base equivalents.
# The Shiny/UI layer is base R Shiny -- no bslib or htmlwidgets-wrapped UI
# helpers, since the layout we want is straightforward and the extra deps
# aren't earning their keep.
# -----------------------------------------------------------------------------

library(shiny)        # Reactive web framework
library(data.table)   # In-memory tabular data, fast joins/filters
library(odbc)         # Databricks SQL connectivity
library(DBI)          # The database-agnostic interface odbc plugs into
library(sf)           # Spatial dataframes for the polygon geometry
library(leaflet)      # The map widget
library(htmltools)    # Building custom HTML for the matrix and area list
library(dotenv)       # Loads .env into Sys.getenv during local development


# -----------------------------------------------------------------------------
# Local .env loading.
#
# When running on Posit Workbench or a developer machine, .env holds the
# Databricks credentials and the catalog/schema names. On Posit Connect the
# environment is populated from the deployed app's Vars panel and the .env
# file is absent (and deliberately excluded by .rscignore).
#
# load_dot_env() is silent if .env is missing, so guarding with file.exists
# is belt-and-braces but makes the intent explicit.
# -----------------------------------------------------------------------------

if (file.exists(".env")) load_dot_env()


# -----------------------------------------------------------------------------
# Table names assembled from environment variables.
#
# Centralising the catalog and schema here means swapping dev for prod is one
# env-var change, not a search-and-replace across every query. Sys.getenv()
# takes a default as its second argument which is used when the variable is
# not set -- handy for local development before .env is populated.
# -----------------------------------------------------------------------------

CAT <- Sys.getenv("FGS_CATALOG", "lab")
SCH <- Sys.getenv("FGS_SCHEMA",  "fgs_dev")

# paste() with sep = "." builds the three-part Unity Catalog identifier.
TBL_STATEMENTS      <- paste(CAT, SCH, "fgs_statements",                 sep = ".")
TBL_RISK_POLYGONS   <- paste(CAT, SCH, "fgs_risk_polygons",              sep = ".")
TBL_EA_INTERSECT    <- paste(CAT, SCH, "fgs_ea_area_intersections",      sep = ".")
TBL_CONST_INTERSECT <- paste(CAT, SCH, "fgs_constituency_intersections", sep = ".")


# -----------------------------------------------------------------------------
# FGS colour palette.
#
# Hex values taken from the FGS User Guide Table 1. Named character vector so
# colour lookups in server.R read as RISK_COLOUR_HEX[<colour_label>]. The
# matching CSS classes in ui.R use the same names with a capital first letter
# (Red/Amber/Yellow/Green) so the risk-bar swatches in the right panel and
# the polygon fills on the map stay consistent.
# -----------------------------------------------------------------------------

RISK_COLOUR_HEX <- c(
  Red    = "#b02020",
  Amber  = "#e08020",
  Yellow = "#f0d040",
  Green  = "#4f9d4f"
)


# -----------------------------------------------------------------------------
# Risk matrix metadata.
#
# Sixteen cells in the 4x4 FGS matrix. x is impact (1 = Minimal, 4 = Severe),
# y is likelihood (1 = Very Low, 4 = High). Each cell resolves to one of the
# four colour bands per Table 1.
#
# A data.table here rather than a tibble or list-of-lists because:
#   - the rest of the app uses data.table conventions
#   - we'll index it by row position when building the UI
#   - it has zero downstream cost
# -----------------------------------------------------------------------------

RISK_MATRIX <- data.table(
  x      = c(1, 2, 3, 4,   1, 2, 3, 4,   1, 2, 3, 4,   1, 2, 3, 4),
  y      = c(4, 4, 4, 4,   3, 3, 3, 3,   2, 2, 2, 2,   1, 1, 1, 1),
  colour = c("green",  "yellow", "amber",  "red",
             "green",  "yellow", "amber",  "amber",
             "green",  "green",  "yellow", "amber",
             "green",  "green",  "yellow", "yellow")
)
