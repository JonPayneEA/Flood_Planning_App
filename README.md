# FGS Flood Guidance Dashboard (R Shiny)

R Shiny version of the FGS dashboard. Reads from Databricks Delta tables via
**brickster's DBI driver** against a SQL warehouse. Designed for deployment
to Posit Connect. First version covers only the Live View tab; analytics
tabs are placeholders.

## Stack

- **Shiny** for the reactive web framework
- **data.table** for in-memory tabular work
- **brickster** for Databricks SQL connectivity via the SQL Statement
  Execution API (no ODBC driver required, no Python dependency)
- **DBI** as the standard database interface brickster plugs into
- **sf** for the polygon geometry
- **leaflet** for the map widget, with `leafletProxy()` for in-place updates
- **htmltools** for the custom HTML chunks

No dplyr-as-primary, no tidyverse pipes for data manipulation. All in-R
data work uses data.table's `[i, j, by]` syntax.

## Why brickster

Two existing routes were considered and discarded for this deployment:

- **ODBC** (via `odbc::databricks()`) -- needs the Databricks ODBC driver
  installed on the host. Not available on this Workbench.
- **sparklyr + Databricks Connect** -- works without ODBC but pulls in
  Python via reticulate, attaches to a general-purpose cluster (more
  expensive than a SQL warehouse), and adds first-launch lag for Python
  env setup.

Brickster takes a third path: it wraps Databricks' SQL Statement Execution
REST API and exposes it as a standard DBI driver. The wire protocol is
HTTPS to your existing SQL warehouse, with no driver to install and no
Python to manage. Performance is comparable to ODBC for the data volumes
this app handles.

## Files

| File | Purpose |
|---|---|
| `app.R` | Entry point. Sources the other files and starts the app. |
| `global.R` | Library loads, env vars, constants. |
| `ui.R` | Layout, sidebar, panels, custom CSS, risk matrix grid builder. |
| `server.R` | Reactive logic, filtering, output bindings, map updates. |
| `data.R` | Queries via brickster's DBI driver. |
| `.env.example` | Template for connection credentials. |

## Prerequisites

R 4.2 or later. Required packages:

```r
install.packages(c(
  "shiny", "data.table", "brickster", "DBI",
  "sf", "leaflet", "htmltools", "dotenv"
))
```

If `brickster` is not on CRAN where you are, install from the labs repo:

```r
# install.packages("pak")
pak::pak("databrickslabs/brickster")
```

## Setup

1. Copy `.env.example` to `.env`. Fill in:
   - `DATABRICKS_HOST` -- your workspace URL with `https://` prefix
     (e.g. `https://adb-1234567890123456.7.azuredatabricks.net`)
   - `DATABRICKS_TOKEN` -- a personal access token from Databricks
     (Settings -> Developer -> Access tokens, scope = `sql`)
   - `DATABRICKS_WAREHOUSE_ID` -- the ID of your SQL warehouse. Find it
     on the warehouse's Connection Details tab in Databricks. The ID is
     the last segment of the HTTP path, e.g. for `/sql/1.0/warehouses/abc123`
     the warehouse ID is `abc123`.
   - `FGS_CATALOG`, `FGS_SCHEMA` -- the Unity Catalog location of the
     FGS tables.

2. Open the project in RStudio (or VS Code with the R extension). Click
   *Run App*, or call:

   ```r
   shiny::runApp()
   ```

## Deployment to Posit Connect

In RStudio, click *Publish*. Or:

```r
rsconnect::deployApp(
  appName = "fgs-dashboard",
  account = "<your_connect_account>",
  server  = "<your_connect_server>"
)
```

Set the five environment variables under the deployed app's *Vars* tab.
Do not deploy `.env`; `.rscignore` already excludes it.

## Data dependencies

Reads from four Delta tables in the configured catalog/schema:

- `fgs_statements`
- `fgs_risk_polygons` with the six risk matrix columns: `risk_x`,
  `risk_y`, `impact_label`, `likelihood_label`, `risk_level`, `risk_colour`
- `fgs_ea_area_intersections` -- assumed to carry through the risk matrix
  columns from the polygon table
- `fgs_constituency_intersections`

If your intersection tables still use the older `risk_level_min` /
`risk_level_max` schema, queries in `data.R` will need adjustment.

## Known gaps for v1

- EA flood area markers listed in the right-hand panel but not yet plotted
  on the map. Requires joining the intersection table to the FWA / FAA
  geometry tables.
- Constituency layer toggle has no rendering yet.
- Public OpenStreetMap basemap. If the EA proxy blocks tile servers,
  swap the `providers$OpenStreetMap` reference in `server.R` for an
  internal tile URL.
- No query-level caching. For the analytics tabs, wrap reactives in
  `bindCache()`.
