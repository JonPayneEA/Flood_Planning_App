# FGS Flood Guidance Dashboard (R Shiny)

R Shiny version of the FGS dashboard. Reads from Databricks Delta tables via
ODBC, designed for deployment to Posit Connect. First version covers only
the Live View tab; analytics tabs are placeholders.

## Stack

- **Shiny** for the reactive web framework
- **data.table** for in-memory tabular work (joins, filters, sub-selection)
- **sf** for the polygon geometry
- **leaflet** for the map widget, with `leafletProxy()` for in-place updates
- **odbc** + **DBI** for the Databricks SQL connection
- **htmltools** for building the custom HTML chunks (risk matrix grid, EA
  area list rows)

No dplyr, no tidyverse pipes. The data manipulation in `server.R` uses
data.table's `[i, j, by]` syntax throughout.

## Files

| File | Purpose |
|---|---|
| `app.R` | Entry point. Sources the other files and starts the app. |
| `global.R` | Library loads, env vars, shared constants, risk matrix definition. |
| `ui.R` | Layout, sidebar, panels, custom CSS, risk matrix grid builder. |
| `server.R` | Reactive logic, filtering, output bindings, map updates. |
| `data.R` | All SQL queries against the Delta tables. |
| `.env.example` | Template for connection credentials. |

## Prerequisites

R 4.2 or later. Required packages:

```r
install.packages(c(
  "shiny", "data.table", "odbc", "DBI",
  "sf", "leaflet", "htmltools", "dotenv"
))
```

The Databricks ODBC driver must be installed on the machine running R. On
Posit Workbench at the EA this is usually pre-installed. To check, in an
R console:

```r
odbc::odbcListDrivers()
```

If a Databricks or Simba Spark driver appears in the list, you're set. If
not, ask whoever administers your Workbench to install Posit's managed
driver.

## Setup

1. Copy `.env.example` to `.env`. Fill in the Databricks SQL warehouse
   hostname, HTTP path, and personal access token. Get the warehouse details
   from Databricks under *SQL Warehouses -> Connection details*.

2. Open the project in RStudio (or VS Code with the R extension). Click
   *Run App*, or call:

   ```r
   shiny::runApp()
   ```

   The app opens in the Viewer pane in RStudio, or as a localhost URL in
   the terminal in VS Code.

## Deployment to Posit Connect

In RStudio, the *Publish* button next to the run icon walks through it. Or
from the console:

```r
rsconnect::deployApp(
  appName  = "fgs-dashboard",
  account  = "<your_connect_account>",
  server   = "<your_connect_server>"
)
```

After publishing, set the five environment variables under the deployed
app's *Vars* tab in Connect. Do not deploy `.env` itself; `.rscignore`
already excludes it.

## Data dependencies

Reads from four Delta tables in the configured catalog/schema:

- `fgs_statements`
- `fgs_risk_polygons` with the six risk matrix columns: `risk_x`,
  `risk_y`, `impact_label`, `likelihood_label`, `risk_level`, `risk_colour`
- `fgs_ea_area_intersections` assumed to carry through the risk matrix
  columns from the polygon table
- `fgs_constituency_intersections`

If your intersection tables still use the older `risk_level_min` /
`risk_level_max` schema, queries in `data.R` will need adjustment to
derive `risk_colour` and `risk_level` on the fly.

## Known gaps for v1

- EA flood area markers listed in the right-hand panel but not yet plotted
  on the map. Requires joining the intersection table to the FWA / FAA
  geometry tables. Straightforward extension.
- Constituency layer toggle has no rendering yet, same reason.
- Public OpenStreetMap basemap. If the EA proxy blocks the OSM tile servers
  in production, swap the `providers$OpenStreetMap` reference in `server.R`
  for an internal tile URL.
- No query-level caching. Every reactive recompute hits Databricks. Fine for
  the live view; the analytics tabs will need `bindCache()` when they land.

## Why leafletProxy()

The Python sibling rebuilt the map widget on every reactive update because
ipyleaflet's add/remove APIs are awkward. In R, `leafletProxy()` lets us
mutate the existing map in place, so the user's pan and zoom state survives
across reactive updates. A forecaster zoomed in on Cumbria doesn't get
bounced back to the national view every time they tick a checkbox. This is
the biggest functional difference between the two versions.
