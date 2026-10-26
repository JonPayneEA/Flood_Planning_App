# FGS Flood Guidance Dashboard (R Shiny)

R Shiny version of the FGS dashboard. Reads from Databricks Delta tables via
**brickster's DBI driver** against a SQL warehouse. Designed for deployment
to Posit Connect. It only reads: nothing is written back to the warehouse.

Tabs:

- **Overview**: the latest statement at a glance (headline, counts at Medium or
  above, escalations, the five-day outlook, constituencies to watch, what changed).
- **FGS Interrogator**: the map and filters, with the flood warning and alert
  areas under the forecast listed beneath ("Affected Target Areas").
- **Constituency situation**: one row per constituency (forecast, areas under the
  forecast, warnings in force, MP) with the MP briefing draft, and a statement
  look-back (proof of concept).
- **Exposure history**: how often each constituency has been at each risk level.
- **Warnings vs FGS**: for each alert and warning, whether an FGS polygon covered its
  area and whether that forecast was issued before, on the day or after, and the share
  of warnings covered over a period. Reads `ea_warning_fgs_coverage` (notebook 26).
- **Hydrometrics**: interactive (plotly) river level, flow and rainfall readings
  from the Environment Agency real-time data, archive and live tail together,
  from one day back to everything held (long windows are summarised to about
  1,500 points per measure).
  Needs the `plotly` R package. Where FGS_Notebooks job 16 has built `ea_rt_measures_mapped`,
  rain gauges show their real names and every label carries the WISKI and FFFS ids.
- **Rain gauges**: a map of the Environment Agency's rain gauges, each coloured by its rainfall over the
  past 1, 12 or 24 hours. Click a gauge for its latest reading, the three totals and how many of the expected
  readings arrived, and it is added to a bar chart of its past 24 hours (hourly or 15-minute bars) beside the map. Up to six gauges can be charted at once, one panel each on one scale. Every rainfall gauge in the register is drawn, with or without an FFFS ID (stations given only as an easting and northing are placed from it; any with no position are counted, not drawn). A grey gauge has had no reading for three hours. A single reading above a limit you can set (100 mm to start) or below zero is left out of the totals, and a gauge that had such readings, or a total too large to believe, is outlined as suspect and can be hidden. Find a gauge by name, station or FFFS ID.
  A gauge that reports both 15-minute totals and instantaneous readings is drawn once, on its totals. Reads
  `ea_rt_measure_register`, `ea_rt_readings_all` and `ea_rt_latest` (notebooks 9 and 23), and
  `ea_rt_measures_mapped` for names and ids where notebook 16 has built it.
- **Warning history**: for each constituency, how many flood alerts and flood
  warnings have been issued for areas inside it, with a period slider and a
  breakdown by area.
- **MP audit log** (demo mode only) and **Guide** (how each figure is calculated).
- A **Planned** menu for tabs that wait on data the pipeline does not hold yet.

A header badge shows the pipeline's health, and a banner names any pipeline table
that has not been built yet.

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
| `theme.R` | Logo palette, Bootstrap theme and header mark, in one place. |
| `design.R` | The design system and page shell: GOV.UK-style tokens, `design_css()`, the section list `APP_SECTIONS`, masthead, navigation, footer, the Planned page and the demo-only Style guide. |
| `helpers.R` | Pure functions (colours, popups, staleness, statement choice). Unit tested. |
| `app_ui.R` | Layout, sidebar, panels, custom CSS, risk matrix grid builder. Every section is a function of the request, which bookmarking needs. |
| `app_server.R` | Reactive logic, filtering, output bindings, map updates. |
| `data.R` | Queries via brickster's DBI driver (one shared connection, retried on network failure) and the shared query cache. |
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
  appName = "flood_intelligence_tool",
  account = "<your_connect_account>",
  server  = "<your_connect_server>"
)
```

Publishing needs a current `rsconnect` (1.11 or later) and a current `openssl`: an old
`openssl` fails with `'rsa_sign' is not an exported object`, and an old `rsconnect`
can get a 404 from a newer Connect. Update both, then **restart R** before publishing,
because a package already loaded in the session is not replaced.

After the first publish, on the content's page in Connect:

1. **Environment variables** (Settings, Advanced, Environment variables): add
   `DATABRICKS_HOST`, `DATABRICKS_WAREHOUSE_ID`, `FGS_CATALOG`, `FGS_SCHEMA` and
   `DATABRICKS_TOKEN`. `.env` is deliberately not deployed, so without these the app
   stops at start with "Missing required environment variables". Do not put the
   token in the code or in chat.
2. **The token.** A personal access token works to start, but it carries your own
   access, so the app stops for everyone when it expires or your access changes. For
   a deployed app use a service principal with `SELECT` on the one schema and **Can
   use** on the warehouse, and never any access to the restricted schema:

   ```sql
   GRANT USE CATALOG ON CATALOG prd_dash_lab TO `<application-id>`;
   GRANT USE SCHEMA, SELECT ON SCHEMA prd_dash_lab.flood_forecasting_unrestricted TO `<application-id>`;
   ```

   Note the token's expiry date in a calendar, and replace the variable before it
   lapses (the app then shows "Failed to connect to warehouse").
3. **Access.** Set who may open it. "All users" with login required means anyone with
   a Connect login. Check it does not say "Anyone", which needs no login. The app shows
   MP names, emails and phone numbers, and the CSV export downloads them.
4. **Runtime** (Advanced). Set **minimum processes to 1** and raise the idle timeout,
   so the first visitor after a quiet spell is not waiting for R to start. Raise the
   maximum processes if many people use it at once: one R process serves one request at
   a time, and the map layers are the slow part.

Share the **Connect content address** (`.../flood-intel-tool/`), not the address of
your Workbench session. The Workbench address sits behind a different sign-in and gives
other people a "does not exist in tenant" error.

### If Connect shows an error

| What you see | What it means |
|---|---|
| "An error has occurred. The application failed to start" | Open the content's Logs. "Missing required environment variables" means step 1 above. |
| "Failed to connect to warehouse" in the app | The host, token or warehouse id is wrong, the token has expired, or the warehouse is stopped (it takes about a minute to wake). |
| **502 Bad Gateway** from `Microsoft-Azure-Application-Gateway` | The gateway in front of Connect, not the app. If other Connect content fails too, it is the platform: tell its owners and wait. |
| "Selected user account does not exist in tenant" | The person was sent the wrong link (see above), or their account is not set up on the platform. |

Optional settings, all in `.env.example`:

| Variable | Default | Effect |
|---|---|---|
| `FGS_GEOMETRY_SOURCE` | `risk_areas` | Must match the pipeline. See above. |
| `FGS_AUTO_REFRESH_SECONDS` | `120` | How often an open session checks for new data. `0` is off, otherwise at least 30. |
| `FGS_QUERY_RETRIES` | `2` | Extra attempts after a network failure (0 to 5), with pauses of 1.5 s then 3 s. SQL errors and missing tables are never retried. |
| `FGS_WARNING_CONTACT_MIN_PCT` | `0.01` | Share of a flood warning or alert area that must lie inside a constituency for it to count as under that constituency's forecast. Match the pipeline's `WARNING_CONTACT_MIN_PCT`. |
| `FGS_STALE_HOURS` | `3` | Hours without a pipeline health update before the stale-data banner shows. |
| `FGS_CACHE_SECONDS` | `600` | How long query results are shared across sessions. `0` is off (a missing table is still remembered for five minutes). |
| `FGS_CACHE_GEOMETRY_ENTRIES` | `30` | How many map-geometry results the shared cache keeps. One entry is one layer for one statement and day. The oldest is dropped first, so memory stays bounded. Other results are capped separately at 200. |
| `FGS_CONTACT_MIN_PCT` | `0.01` | Share of a constituency a forecast polygon must cover, used only to rebuild past statements in the look-back. Match the pipeline's `FGS_CONTACT_MIN_PCT`. |
| `FGS_DEFAULT_NATIONS` | `E14` | Which nations the **Nations** filter starts on: `E14` England, `W07` Wales, `S14` Scotland, `N06` Northern Ireland. A comma separated list, for example `E14,W07`. The data always holds all four; the filter under the header changes the choice, and clearing it shows all four. |
| `FGS_POC_FEATURES` | `1` | `0` hides the proof-of-concept features (the look-back). |
| `FGS_SHOW_QUERY_PROGRESS` | off | `1` shows the driver's "Submitting query" progress pop-ups again. Off by default: they appeared for every query, with terminal colour codes in the text. |
| `FGS_DEMO_MODE` | off | `1` runs on generated data with no warehouse. See Demo mode. |

On Connect, set these under the content's Advanced tab, Environment variables.
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
  keeps no history of them. The **warning and alert areas under the forecast** are
  shown, because they are built from the intersections the pipeline keeps for every
  statement.
- The picker groups a day with several statements under one heading, for example
  "Thu 02 Oct (3 statements)".
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
  the notebook, press Refresh, and the view appears. If it does not, restart the
  app process, which empties the cache.
- `fgs_changes_log` (also written by notebook 18) is never reported as missing:
  without it the changes panel still works, with less detail.
- On a quiet day, `mp_briefing_drafts` is legitimately empty, because drafts are only
  written at Medium or above. The app says so in words.

Which notebook builds which table, and when they run:

| Table | Notebook | Master_polling step |
|---|---|---|
| `fgs_constituency_changes` | 18 | 6 |
| `constituency_flood_situation` | 19 | 7 |
| `mp_briefing_drafts` | 20 | 8 (refuses if step 7 is out of date) |
| `pipeline_health` | 22 | 9 |
| `fgs_constituency_exposure` | 21 | Master_daily |
| `ea_warning_episodes` | 25 | 10 |
| `ea_warning_fgs_coverage` | 26 | Master_daily |

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

Colours come from `theme.R`, and only from there. The palette is sampled from the
tool's logo: a navy field, white house and sky-blue waves. Change the values in
`APP_PALETTE` and `tests/` will fail if a swap breaks a contrast pair. The sky and
cyan blues are decoration only, because they are too light for text on white.

The header shows the logo mark, drawn inline from `theme.R` (`app_logo()`), so no
file has to ship. To use the artwork itself, drop a mark-only file (no lettering)
at `www/logo.svg` or `www/logo.png`; it replaces the drawn mark.

The FGS risk colours (red, amber, yellow, green on the matrix, map, chips and
heat grid) are deliberately **not** brand colours. They come from the FGS User
Guide and keep a fixed meaning. The frame is blue so that it is never read as the
"Very Low" green, and magenta stays reserved for flood warning areas on the map.

## Design system and shell

The look follows the GOV.UK Design System (Arial, its type scale, plain links, square
form controls, the yellow focus state, flat buttons, hairline tables, inset notices),
in the brand navy with the house-on-waves mark. `design.R` holds it. `design_css()` is
added after the older rules in `app_ui.R`, so it overrides them one component at a
time. Colour tokens are in `GOV_COLOURS`; `tests/testthat/test-design.R` checks their
contrast. The FGS risk colours are not touched.

Navigation is a masthead, five sections (Now, Constituencies and briefings, Observed,
History, Help) and a row of views for the section you are in. `APP_SECTIONS` is the one
list of what sits where. The views are the same `tabPanel`s as before, and Shiny's own
tab strip is hidden: the links click it, so `input$main_tabs`, bookmarked links and the
server code are unchanged. Adding a view means adding its `tabPanel` in `app_ui.R` and
one line in `APP_SECTIONS`; a test fails if the two disagree. In demonstration mode a
Style guide view under Help shows every component on one page.

## Nations filter

The Environment Agency works in England, but its flood areas run up to the borders, so the data
holds constituencies in Wales, Scotland and Northern Ireland too. A **Nations** filter under the
header chooses which are shown, on every tab: the counts, lists, map layers, changes, MP briefing
drafts, exposure history and the CSV exports follow it. It starts on England (`FGS_DEFAULT_NATIONS`),
you can add the other nations, and clearing it shows all four. A constituency's nation comes from
the first three characters of its ONS code (`E14`, `W07`, `S14`, `N06`), in `in_nations()`
(`helpers.R`). An ID that matches none of them, such as the demonstration's, is always shown. The
filter works on what is already fetched, so changing it does not query the warehouse again, and it
is saved in the shareable link like the other filters.

## Behaviour worth knowing

- **Auto-refresh.** Every `FGS_AUTO_REFRESH_SECONDS` an open session runs one
  small query (newest statement, statement count, latest modification, latest
  health check). Only if that changed does it reload the statement list and
  the pipeline views, drop the cache and show "New pipeline data loaded". A
  user on the newest statement follows to the new one; a user who chose an
  older statement stays on it.
- **Changes panel.** It lists the rises and falls since the previous statement, one
  line per constituency. When there are none it says what it compared with: "No change
  since statement #4924 (issued Sun 18 Oct 09:30, Mon 19 to Thu 22)". It reads
  `fgs_changes_log` (notebook 18) to tell "compared, nothing moved" from "not compared
  yet" ("Notebook 18 has not run for this statement"). Under the list it also shows
  how many constituencies each statement flags over all five of its own days, at any
  risk and at Medium or above, so "30 in statement #4924, none in this one" shows even
  though the day-by-day comparison leaves out days that have rolled off. Without the log
  table, an empty result is trusted as no change; without the newer notebook 18, the
  counts line is left out.
- **Source, impact and likelihood.** Constituency situation has a **Flood source** filter
  (any of river, coastal, surface water and groundwater), a **Minimum impact** filter, and
  **Impact** and **Likelihood** columns for the polygon behind the worst risk on the first
  day it occurs (the highest impact, then the highest likelihood, where several share that
  risk). They are worked out in the app from `fgs_constituency_intersections`, which keeps
  every statement, so they work for the look-back too. Choosing a source recalculates the
  risks, the five days, Sources, Impact, Likelihood and the areas under the forecast for
  those sources alone; Escalated and the in force columns stay across all sources. If the
  intersections cannot be read the new columns show n/a and the source filter says it was
  not applied. **Export CSV** has the same columns, in the same order and with the same
  names as the table, with the risk in words and the MP's email and phone added.
- **Areas under the forecast.** Constituency situation has two columns, "Warning areas
  under forecast" and "Alert areas under forecast": the flood warning areas and flood
  alert areas that sit under a risk polygon of the statement, counted per constituency
  (an area counts when at least `FGS_WARNING_CONTACT_MIN_PCT` of it lies inside, and
  once however many days or polygons cover it). They are potential warnings, places where
  one could be issued, and are separate from the "in force" columns. The minimum
  forecast risk filter also sets which polygons count. They work for any statement,
  including the look-back.
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
  Memory only: a restart empties it. Map geometry, the heavy part, rolls at
  `FGS_CACHE_GEOMETRY_ENTRIES` entries (oldest first), so memory stays bounded.
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
view tests need `leaflet` and skip themselves without it. The run takes about two
minutes. They cover the geometry-source filter on every tagged query, SQL safety of
browser-supplied ids, the cache (including the geometry cap), the shared connection,
retries and outage logging against fakes, the staleness rules, the statement-selection
rules, the changes messages, the areas under the forecast, the Guide against the code
and the pipeline's `config.py`, and each view on fake data. They do not draw a real map
and do not touch Databricks.

## Decisions that are not the app's to make

- **Recording who reviewed or sent a briefing.** The drafts are read-only
  here. The "MP audit log" tab needs the app to write status back to the
  warehouse, which needs an owner and a rule for who may do it. Until that is
  decided the tab stays a placeholder.
- **Who may open the app.** It shows MP names, emails, phone numbers and draft
  briefing text. Currently any user with a Connect login. Keep the app on the
  unrestricted schema only: if it ever needs restricted data, deploy a second app
  with its own service principal and a named access list, because data access follows
  the app's identity, not the visitor.
- **A service principal for the deployed app.** See Deployment. Until then the app
  depends on one person's token.

## Data dependencies

Reads from these Delta tables in the configured catalog/schema:

- `fgs_statements`
- `fgs_risk_polygons` with the six risk matrix columns: `risk_x`,
  `risk_y`, `impact_label`, `likelihood_label`, `risk_level`, `risk_colour`
- `fgs_ea_area_intersections` -- assumed to carry through the risk matrix
  columns from the polygon table
- `fgs_constituency_intersections`
- `ea_area_constituency_lookup` (links flood warning and alert areas to
  constituencies, for the areas under the forecast)
- `mp_contact_details` and `parliamentary_constituencies` (the look-back)

Both intersection tables carry `geometry_source`, and every query filters on
it (see `FGS_GEOMETRY_SOURCE` above). If your intersection tables still use the
older `risk_level_min` / `risk_level_max` schema, queries in `data.R` will need
adjustment.

Pipeline products (FGS_Notebooks jobs 18 to 22), all plain tables:

| Table | Used for |
|---|---|
| `pipeline_health` | Status badge in the header; click for each check. |
| `fgs_constituency_changes` | "Since the previous statement" strip on the Interrogator and the Overview. Follows the statement picker. |
| `fgs_changes_log` | Says whether notebook 18 compared a statement, and the headline counts for both statements. Optional: the app works without it. |
| `constituency_flood_situation` | Constituency situation tab: one row per constituency. Always the pipeline's latest complete statement. |
| `mp_briefing_drafts` | Briefing draft shown when a constituency row is selected. Read only: nothing is sent. |
| `fgs_constituency_exposure` | Exposure history tab: heat grid of constituencies by month, 12 months. |
| `ea_warning_episodes` | Warning history tab: alerts and warnings issued per constituency, from notebook 25. |
| `ea_warning_fgs_coverage` | Warnings vs FGS tab: each alert and warning, and whether and when an FGS polygon covered it. |
| `ea_rt_measure_register`, `ea_rt_readings_all`, `ea_rt_readings_bronze` | Hydrometrics tab: the gauges, their readings (archive and live tail), and where the archive ends. |
| `ea_rt_latest` | Rain gauges tab: the newest reading of each measure (notebook 23). |
| `ea_rt_measures_mapped` | Hydrometrics tab, optional: the register with each measure's real gauge name, FFFS id and WISKI id (FGS_Notebooks job 16). Without it the plain register is used, and the FFFS ID selector offers only All. |

Each product is fetched defensively. If a table is missing or unreadable the
view says so and the rest of the app carries on.

The risk matrix labels follow the pipeline README: impact Minor to Extreme,
likelihood Low to Very High.

## Known gaps

- Observed river and sea levels are not in the constituency view. The pipeline now
  holds live readings for every site (`ea_rt_latest`, `ea_rt_readings_all`), but this
  app does not read them. A separate hydrometric app is planned for that, with flood
  thresholds from the restricted schema in its own restricted deployment.
- Warning issue times, forecast verification and event playback are
  placeholders. Verification and playback depend on the Met Office licence
  decision.
- Public OpenStreetMap basemap. If the EA proxy blocks tile servers,
  swap the `providers$OpenStreetMap` reference in `app_server.R` for an
  internal tile URL.
- Warnings in force have no history, so the look-back shows n/a for them.
- The look-back, and anything else tagged `POC-ONLY`, is to be removed when the
  app goes live.
