# FGS Flood Guidance Dashboard (R Shiny)

R Shiny version of the FGS dashboard. Reads from Databricks Delta tables via
**brickster's DBI driver** against a SQL warehouse. Designed for deployment
to Posit Connect. Tabs: the FGS Interrogator (map and filters), Constituency
situation, and Exposure history. The remaining analytics tabs are placeholders
that wait on data the pipeline does not hold yet.

## Stack

- **Shiny** for the reactive web framework
- **data.table** for in-memory tabular work
- **brickster** for Databricks SQL connectivity via the SQL Statement
  Execution API (no ODBC driver required, no Python dependency)
- **DBI** as the standard database interface brickster plugs into
- **DT** for the sortable constituency table
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

One cost is real: brickster sends a `SELECT 1` test query every time it opens
a connection, so a connection per query doubles the requests. The app opens
one connection and reuses it for every query (rebuilding it after 30 minutes
or after a network failure), so that overhead is paid once, not per query.

## Files

| File | Purpose |
|---|---|
| `app.R` | The only entry point. Sources the files below and starts the app. They are `app_ui.R` and `app_server.R`, not `ui.R` and `server.R`, because Shiny prefers `ui.R`/`server.R` over `app.R` when both exist. |
| `global.R` | Library loads, env vars, constants. |
| `guide.R` | The Guide tab: how each product is calculated. All the wording is here and is tested against the code and the pipeline's `config.py`. |
| `demo.R` | Synthetic data for demo mode. Replaces every query. Fictional throughout. |
| `theme.R` | Environment Agency colours and Bootstrap theme, in one place. Provisional palette. |
| `helpers.R` | Pure functions (colours, popups, staleness, statement choice). Unit tested. |
| `app_ui.R` | Layout, sidebar, panels, custom CSS, risk matrix grid builder. Every section is a function of the request, which bookmarking needs. |
| `app_server.R` | Reactive logic, filtering, output bindings, map updates. |
| `data.R` | Queries via brickster's DBI driver, and the shared query cache. |
| `tests/` | Unit and view tests. No Databricks connection needed. |
| `.env.example` | Template for connection credentials and optional settings. |
| `.rscignore` | Keeps `.env`, tests and the README out of a Connect deploy. |

## Prerequisites

R 4.2 or later. Required packages:

```r
install.packages(c(
  "shiny", "data.table", "brickster", "DBI",
  "sf", "leaflet", "htmltools", "dotenv", "DT"
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
   - `FGS_GEOMETRY_SOURCE` -- optional, `risk_areas` (default) or `aoc`.
     Must match `FGS_GEOMETRY_SOURCE` in the pipeline's `config.py`. The
     intersection tables can hold both sources, and the app reads only this
     one, so a mismatch shows the wrong set or an empty map.

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

Optional settings, all in `.env.example`:

| Variable | Default | Effect |
|---|---|---|
| `FGS_GEOMETRY_SOURCE` | `risk_areas` | Must match the pipeline. See above. |
| `FGS_AUTO_REFRESH_SECONDS` | `120` | How often an open session checks for new data. `0` is off, otherwise at least 30. |
| `FGS_QUERY_RETRIES` | `2` | Extra attempts after a network failure (0 to 5), with pauses of 1.5 s then 3 s. SQL errors and missing tables are never retried. |
| `FGS_WARNING_CONTACT_MIN_PCT` | `0.01` | Share of a flood warning or alert area that must lie inside a constituency for it to count as under that constituency's forecast. Match the pipeline's `WARNING_CONTACT_MIN_PCT`. |
| `FGS_STALE_HOURS` | `3` | Hours without a pipeline health update before the stale-data banner shows. |
| `FGS_CACHE_SECONDS` | `600` | How long query results are shared across sessions. `0` is off (a missing table is still remembered for five minutes). |
| `FGS_DEMO_MODE` | off | `1` runs on generated data with no warehouse. See Demo mode. |

Set the environment variables under the deployed app's *Vars* tab.
Do not deploy `.env`; `.rscignore` already excludes it.

## Demo mode

```sh
FGS_DEMO_MODE=1 Rscript -e 'shiny::runApp()'
```

Runs the whole app on generated data. It needs no Databricks token, no warehouse
id, no `brickster`, no network beyond the map tiles, and no `.env`. A black
ribbon on every screen says it is a demonstration, so a screenshot cannot be
mistaken for live data.

What it is for: showing the tool to senior forecasters and managers, and letting
them try it, without access to live data.

- **Nothing is read from or written to a warehouse.** In demo mode `db_query()`
  and `db_connection()` refuse to run, and `tests/` checks both that no demo
  function calls them and that no file in the app contains a write statement.
- **It is interactive.** Pick a scenario (quiet, developing event, severe event),
  simulate a pipeline problem to see the stale-data banner, and mark briefing
  drafts reviewed, sent or rejected. Those actions, and the "MP audit log" tab
  that lists them, live in the browser session only. They are lost when the page
  closes, and one viewer cannot see another's.
- **Time zones.** Statement times are shown as stored, with no zone. The FGS is
  issued at 09:30 UK time and the pipeline keeps the issue time as the API gave
  it, so labelling it UTC would be wrong in summer. Pipeline health times carry
  an explicit offset and are labelled UTC.
- **Everything is fictional.** Constituencies are a hexagon map of a rough outline
  of England, not real boundaries. Constituency names, MPs, rivers and flood
  areas are invented, and email addresses end in `example.invalid`, which can
  never be delivered.
- **One model feeds every table**, so the map, the overlaps, the situation
  table, the changes and the briefings agree with each other.

To put it in front of people, deploy the same bundle to Posit Connect as a
second item and set only `FGS_DEMO_MODE=1`. It needs no credentials.

## The Guide tab

The **Guide** tab explains, in words, how each product in the app is calculated,
how to read it and where it stops being reliable. It covers the ideas behind the
figures (the risk matrix, forecast against warnings, how shapes link to places),
every product, a worked example taken from the latest statement, where each table
comes from, how this deployment is set up, and a glossary. It is searchable, and
the Overview links to it.

The text is in `guide.R`, in one place, so it can be reviewed as prose. It is
tested: every table the app reads must be documented, every threshold quoted must
match the pipeline's `config.py` (checked when `FGS_Notebooks` sits alongside this
repository), the matrix drawn is the one the map uses, and the prose follows the
house style. **When you add a product, add it to the Guide.** The tests say what
is missing.

## Proof-of-concept features (to remove when live)

Anything that exists only because this is a proof of concept is tagged
`POC-ONLY` in the code and sits behind one switch, `FGS_POC_FEATURES`
(default `1`). Set it to `0` to hide all of it, and delete what
`grep -rn POC-ONLY .` finds when the app goes live.

| Feature | Where | What it does |
|---|---|---|
| **Statement look-back** | Constituency situation tab, "Statement" picker | Shows an earlier statement's constituency table, so a quiet day today can still be compared with a busier one. |

How the look-back works, and what it cannot do:

- The live `constituency_flood_situation` table only ever holds the latest
  statement. For an earlier one the app rebuilds the table from
  `fgs_constituency_intersections`, which the pipeline keeps for every
  statement (`aggregate_situation()` in `helpers.R`, tested against the demo's
  independent model).
- **Warnings and alerts in force show n/a** for past statements: the pipeline
  keeps no history of them.
- **MP details are today's**, matched to constituencies on a normalised name.
- Briefing drafts come from `mp_briefing_drafts`, which keeps older statements'
  batches as history. Reviews can only be recorded against the live batch.
- The Overview always shows the latest statement, whatever is picked here.
- `FGS_CONTACT_MIN_PCT` (default `0.01`) should match the pipeline's
  `config.py`, so a rebuilt table counts polygons the same way.

## If a table is missing

The pipeline builds the tables behind the newer views (notebooks 18 to 22), and
a workspace that has not run them yet will answer `TABLE_OR_VIEW_NOT_FOUND`. The
app does not fail. It says what is missing, on screen:

- A banner under the header names each missing table and the notebook that
  builds it, for example `fgs_constituency_changes (pipeline notebook 18)`.
- The view that needs it says so in words ("Change tracking unavailable: ...")
  instead of showing nothing or claiming "no changes".
- The look-back still works without the changes table, with escalation shown as
  n/a (unknown, not "none").
- A missing table is remembered for five minutes so switching statement does
  not repeat the failing query. **Refresh statements** forgets it at once, so run
  the notebook, press Refresh, and the view appears.

Which notebook builds which table, and when they run:

| Table | Notebook | Master_polling step |
|---|---|---|
| `fgs_constituency_changes` | 18 | 6 |
| `constituency_flood_situation` | 19 | 7 |
| `mp_briefing_drafts` | 20 | 8 (refuses if step 7 is out of date) |
| `pipeline_health` | 22 | 9 |
| `fgs_constituency_exposure` | 21 | Master_daily |

To find the cause, check which step of the last `Master_polling` run failed (a
failure is recorded and the later steps still run), or list what exists:
`SHOW TABLES IN <catalog>.<schema>`.

**"Failed to connect to warehouse ... Failed to perform HTTP request"** is a
different problem: the warehouse could not be reached (network, VPN or the
warehouse stopping). The app shows a warning, keeps the last data, and tries
again on the next refresh. It is not remembered, so it recovers by itself.

## If the map spins

The spinner covers a map layer that is still being fetched. It now clears
whenever the layer finishes or fails, and a failure shows a red message on
screen (for example "The EA flood areas layer could not be drawn: ...") as well
as in the R console. If the spinner stays on for a long time with no message,
the layer's query is still running: the console prints `<query> took N s` for
anything over five seconds, which shows which one is slow. Send that message
or line with a bug report.

## Look and feel

Colours come from `theme.R`, and only from there. The palette is provisional:
the greens are sampled from the Environment Agency logo, the blues and magenta
were already in the app, and no official palette was available. Replace the
values in `EA_PALETTE` with the brand file's, and `tests/` will fail if a swap
breaks a contrast pair. The brand green is borderline for white text (4.43:1), so
a darker green is used wherever text sits on it.

The FGS risk colours (red, amber, yellow, green on the matrix, map, chips and
heat grid) are deliberately **not** brand colours. They come from the FGS User
Guide and keep a fixed meaning, and the brand green must never be read as the
"Very Low" green. To add the logo, put the approved file at `www/ea-logo.svg`.

## Behaviour worth knowing

- **Auto-refresh.** Every `FGS_AUTO_REFRESH_SECONDS` an open session runs one
  small query (newest statement, statement count, latest modification, latest
  health check). Only if that changed does it reload the statement list and
  the pipeline views, drop the cache and show "New pipeline data loaded". A
  user on the newest statement follows to the new one; a user who chose an
  older statement stays on it.
- **Changes panel.** It reads `fgs_changes_log` (written by notebook 18) to tell
  "compared, nothing moved" from "not compared yet". Without that table, an empty
  result is trusted as no change, as before. The table is never reported as missing.
- **Warehouse outages.** A network failure drops the shared connection and is
  retried. The console logs the first failure, at most one reminder a minute
  while it persists, and one line when the warehouse is reachable again.
- **Stale-data banner.** A banner shows above every tab when the pipeline is
  failing, needs attention, cannot be read, or has stopped updating its own
  health table (the quiet failure: nothing ran, so nothing failed). The
  decision is `stale_status()` in `helpers.R`.
- **Query cache.** Statement-scoped queries are cached in memory for
  `FGS_CACHE_SECONDS`, shared by every session in the process, so switching
  statement, day or filter does not return to the warehouse each time. The
  cache is cleared on Refresh and whenever auto-refresh sees new data.
  Memory only: a restart empties it.
- **Shareable views.** The address bar follows the tab, statement, day and
  filters. The "Copy link" button copies it. Opening the link restores that
  view. Selections (a table row, a map click) are not part of the link.
- **Constituency forecast risk layer.** A map layer that colours every
  constituency with a forecast risk by its worst rank over five days. It
  reads `constituency_flood_situation`, so it follows the pipeline's latest
  statement, not the statement or day pickers. The older "Constituencies"
  outline layer is unchanged and does follow them.

## Tests

```r
Rscript tests/run_tests.R     # from the project root
```

Needs `testthat` plus the app's own packages (`brickster` is not needed). The
view tests need `leaflet` and skip themselves without it. They cover the geometry-source filter on every
tagged query, SQL safety of browser-supplied ids, the cache, the staleness
rules, the statement-selection rules, and each new view on fake data. They do
not draw a real map and do not touch Databricks.

## Decisions that are not the app's to make

- **Recording who reviewed or sent a briefing.** The drafts are read-only
  here. The "MP audit log" tab needs the app to write status back to the
  warehouse, which needs an owner and a rule for who may do it. Until that is
  decided the tab stays a placeholder.
- **Who may open the app.** It now shows MP names, emails, phone numbers and
  draft briefing text. Make sure the Connect access list for this app matches
  the people who should see that.

## Data dependencies

Reads from these Delta tables in the configured catalog/schema:

- `fgs_statements`
- `fgs_risk_polygons` with the six risk matrix columns: `risk_x`,
  `risk_y`, `impact_label`, `likelihood_label`, `risk_level`, `risk_colour`
- `fgs_ea_area_intersections` -- assumed to carry through the risk matrix
  columns from the polygon table
- `fgs_constituency_intersections`

Both intersection tables carry `geometry_source`, and every query filters on
it (see `FGS_GEOMETRY_SOURCE` above). If your intersection tables still use the
older `risk_level_min` / `risk_level_max` schema, queries in `data.R` will need
adjustment.

Pipeline products (FGS_Notebooks jobs 18 to 22), all plain tables:

| Table | Used for |
|---|---|
| `pipeline_health` | Status badge in the header; click for each check. |
| `fgs_constituency_changes` | "Since the previous statement" strip on the Interrogator. Follows the statement picker. |
| `constituency_flood_situation` | Constituency situation tab: one row per constituency. Always the pipeline's latest complete statement. |
| `mp_briefing_drafts` | Briefing draft shown when a constituency row is selected. Read only: nothing is sent. |
| `fgs_constituency_exposure` | Exposure history tab: heat grid of constituencies by month, 12 months. |

Each product is fetched defensively. If a table is missing or unreadable the
view says so and the rest of the app carries on.

The risk matrix labels follow the pipeline README: impact Minor to Extreme,
likelihood Low to Very High.

## Known gaps for v1

- Observed gauge levels are not in the constituency view yet. The pipeline
  has not turned the readings and thresholds tables into events.
- Warning issue times, forecast verification and event playback are
  placeholders. Verification and playback depend on the Met Office licence
  decision.
- Public OpenStreetMap basemap. If the EA proxy blocks tile servers,
  swap the `providers$OpenStreetMap` reference in `app_server.R` for an
  internal tile URL.
- No query-level caching. For the analytics tabs, wrap reactives in
  `bindCache()`.
