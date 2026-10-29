# =============================================================================
# guide.R
# The Guide tab: how each product in this app is calculated, in words.
#
# All of the wording lives here, in one place, so it can be reviewed as prose and
# tested as data. Nothing in this file queries anything. It takes the app's live
# settings and, for the worked example, the latest situation table, and returns
# HTML.
#
# KEEPING IT TRUE. Every figure in the text comes from GUIDE_PIPELINE (the
# pipeline's defaults, checked against FGS_Notebooks/config.py by a test when that
# repository is alongside this one) or from the live settings passed in. Every
# table the app reads must appear in GUIDE_TABLES, and a test fails if one does
# not. Add a product to the app and the Guide's tests say what is missing.
#
# VOICE. UK English. No em dashes. Short sentences assert; longer ones build.
# =============================================================================


# -----------------------------------------------------------------------------
# What the pipeline does by default. These are the pipeline's settings, not the
# app's: the app cannot see config.py. Change them here if config.py changes.
# -----------------------------------------------------------------------------

GUIDE_PIPELINE <- list(
  briefing_min_rank  = 1L,      # BRIEFING_MIN_RISK_RANK
  ea_min_pct         = 0.25,    # EA_INTERSECTION_MIN_PCT
  contact_min_pct    = 0.01,    # FGS_CONTACT_MIN_PCT
  coastal_buffer_m   = 1000,    # COASTAL_LINE_BUFFER_M
  warning_min_pct    = 0.01,    # WARNING_CONTACT_MIN_PCT
  health_fgs         = c(30, 54),   # HEALTH_FGS_WARN_HOURS, HEALTH_FGS_FAIL_HOURS
  health_chain       = c(3, 12),    # HEALTH_CONTACTS_*  (situation table age)
  health_warnings    = c(3, 12),    # HEALTH_WARNINGS_*  (warnings ingest age)
  health_mp          = c(10, 21),   # HEALTH_MP_WARN_DAYS, HEALTH_MP_FAIL_DAYS
  health_live        = 1,           # HEALTH_LIVE_WARN_HOURS (live EA readings: warns, never fails)
  min_constituencies = 600L,    # MIN_CONSTITUENCIES
  min_mps            = 600L     # MIN_MPS
)


# -----------------------------------------------------------------------------
# Every table the app reads: what it holds, which notebook builds it, how often.
# -----------------------------------------------------------------------------

GUIDE_TABLES <- data.frame(
  table = c("fgs_statements", "fgs_risk_polygons", "fgs_ea_area_intersections", "fgs_constituency_intersections",
            "ea_flood_warning_areas", "ea_flood_alert_areas", "parliamentary_constituencies",
            "ea_area_constituency_lookup", "mp_contact_details",
            "fgs_constituency_changes", "fgs_changes_log", "constituency_flood_situation", "mp_briefing_drafts",
            "fgs_constituency_exposure", "pipeline_health", "ea_warning_episodes",
            "ea_rt_measure_register", "ea_rt_readings_all", "ea_rt_readings_bronze", "ea_warning_fgs_coverage", "ea_rt_measures_mapped", "ea_rt_latest", "flood_warnings_current", "ea_fw_threshold_gauge_links", "ea_area_exposure", "ea_area_overlaps", "ea_area_coverage", "constituency_flood_exposure", "exposure_grid_100m", "ea_area_sites", "ea_area_geographies", "constituency_sites"),
  notebook = c("01", "01", "07", "07", "03", "03", "05", "06", "04", "18", "18", "19", "20", "21", "22", "25", "09", "23", "10", "26", "16", "23", "11", "28", "35", "35", "35", "35", "34", "35", "35", "35"),
  refresh = c("Each FGS issue or amendment", "Each FGS issue or amendment", "Each FGS issue", "Each FGS issue",
              "Weekly", "Weekly", "Quarterly, or after an election", "Weekly and quarterly", "Weekly",
              "Every poll", "Every poll", "Every poll", "Every poll", "Daily", "Every poll", "Every poll",
              "Weekly", "Every 15 minutes", "Daily", "Daily", "When the mapping file changes", "Every 15 minutes", "Every poll", "Manual, when the mapping or thresholds change", "Manual, when the hazard maps or address points change", "Manual, with the table above", "Manual, with the table above", "Manual, with the table above", "Manual, after notebooks 30 to 33", "Manual, with the table above", "Manual, with the table above", "Manual, with the table above"),
  holds = c("One row per statement: the headline and the forecast text.",
            "One row per risk polygon, flood source and day, with impact, likelihood and risk level.",
            "Each FGS shape that overlaps an EA flood warning or alert area.",
            "Each FGS shape that overlaps a parliamentary constituency.",
            "EA Flood Warning Areas, with their boundaries.",
            "EA Flood Alert Areas, with their boundaries.",
            "ONS constituency boundaries.",
            "Which EA areas overlap which constituencies.",
            "Current MPs and their contact details.",
            "What changed in the forecast against the previous statement. History is kept.",
            "One row per statement that was compared, with the number of differences. Tells \"nothing moved\" from \"not compared yet\".",
            "One row per constituency: the forecast, the warnings in force and the MP. The latest statement only.",
            "Draft briefings for MPs. Nothing is sent. Older statements' drafts are kept.",
            "Days each constituency sat at each risk level, by month. Sparse.",
            "One row per health check of the pipeline's own outputs.",
            "Each alert and warning from the time it was switched on to the time it ended, with its duration.",
            "One row per measure at an Environment Agency gauge: what it measures, the station, river and area.",
            "Real-time readings, the daily archive and the live tail joined as one series.",
            "The daily archive of readings. Its last day marks where the live tail begins.",
            "Each alert and warning, and whether and when an FGS polygon covered it.",
            "The measure register with each measure's real gauge name, FFFS id and WISKI id, where the telemetry mapping matched it. Optional.",
            "The newest reading of each measure, from the live tail.",
            "One row per EA area with an alert, warning or severe warning in force now, from the live feed.",
            "Observed flood warning thresholds linked to gauge measures, in each measure's own unit. Held in the restricted schema; read only when the Hydrometrics tick is on.",
            "One row per flood warning area and flood alert area: the people, households and rivers and sea bands inside it, counted gross, unique and split across overlaps, and the critical sites. Restricted schema.",
            "Pairs of areas of one kind that share people, with how many. Restricted schema.",
            "People in England, and in a High or Medium band, covered by any warning area, by any alert area and by neither. Restricted schema.",
            "Per constituency: people, people at risk, and how many of each a warning or alert area covers. Restricted schema.",
            "One row per 100 m cell of England: people, deprivation, rivers and sea band shares, critical sites and a 0 to 100 risk score. Restricted schema; read only for the part of the map in view.",
            "Each critical site inside each warning or alert area, with its rivers and sea band. Restricted schema; read for the area being viewed.",
            "Each warning or alert area split by the police force, fire and rescue authority, combined authority, district, county, ward, constituency and region it lies in, with the people in each. Restricted schema; read for the area being viewed.",
            "Each critical site inside each England constituency, with its rivers and sea band. Restricted schema; read for the constituency being viewed."),
  stringsAsFactors = FALSE
)


# -----------------------------------------------------------------------------
# Small formatting helpers
# -----------------------------------------------------------------------------

guide_pct   <- function(x) sprintf("%s per cent", format(round(x * 100, 1), trim = TRUE))
guide_hours <- function(x) sprintf("%s and %s hours", x[1], x[2])


# =============================================================================
# THE CONTENT
# =============================================================================

# Foundations: ideas every product depends on.
guide_foundations <- function(s) {
  list(
    list(
      id = "matrix", title = "Risk levels and the matrix",
      answers = "How does the forecast turn an impact and a likelihood into a colour?",
      paragraphs = c(
        "The Flood Guidance Statement (FGS) is issued once a day and looks five days ahead. For each flood source (river, surface water, coastal and groundwater) it draws areas of risk, and gives each area an impact and a likelihood.",
        "Impact runs from Minor to Extreme. Likelihood runs from Low (10 to 25 per cent) to Very High (over 75 per cent). The position in the matrix below gives the risk level and its colour.",
        "Throughout the app, risk is also held as a rank: 1 Very Low, 2 Low, 3 Medium and 4 High, with 0 meaning nothing is flagged. \"Medium or above\" means rank 3 or 4."
      ),
      show = "matrix",
      limits = c(
        "The pipeline takes matrix positions from the FGS Public API documentation. The documentation gives positions only, so the risk level and colour for each cell come from the FGS User Guide. They should be checked against the published guide.",
        "The risk colours are fixed by the User Guide and always carry their name as text. The blue frame of this app (header, tabs, buttons) is branding, not a risk colour."
      )
    ),
    list(
      id = "forecast-warnings", title = "Forecast and warnings are different things",
      answers = "Why is there no single risk score for a constituency?",
      paragraphs = c(
        "A forecast says what may happen over the next five days. A warning says what the Environment Agency expects, and has issued. They measure different things, so this app keeps them apart and never combines them into one score.",
        "Warnings come in three levels in force: Severe Flood Warning, Flood Warning and Flood Alert. A fourth state, a warning that has ended, is kept in the data so that a closing warning stays visible. It is not counted as in force.",
        "The map's list titled \"Affected Target Areas\" is not a list of warnings in force. It lists the flood warning areas and flood alert areas that the forecast overlaps or is linked to: places where a warning or alert could be issued. The warnings actually in force are counted on the Overview and on Constituency situation."
      ),
      limits = c(
        "Observed river and sea levels from the gauges are not part of any figure yet. The pipeline has not turned the readings and thresholds tables into events."
      )
    ),
    list(
      id = "linking", title = "How shapes are linked to places",
      answers = "What does it mean for a forecast to \"touch\" a flood area or a constituency?",
      paragraphs = c(
        "The pipeline overlays each FGS shape on two sets of boundaries: the EA's flood areas and the parliamentary constituencies. The overlaps are stored, and every view in this app reads them.",
        sprintf("A risk polygon counts against an EA flood area when it covers at least %s of that area.",
                guide_pct(GUIDE_PIPELINE$ea_min_pct)),
        sprintf("A risk polygon counts against a constituency on any real overlap, but links covering less than %s of the constituency are dropped, so a sliver at an edge does not flag a seat.",
                guide_pct(GUIDE_PIPELINE$contact_min_pct)),
        sprintf("Coastal risk is drawn as a line along the coast, which has no area, and a constituency stops at the coast. A line on or just off that edge often crosses neither a constituency nor a coastal flood area it should reach. So each coastal line is widened by %s metres on each side before it is matched, for both flood areas and constituencies. A line has no area, so the coverage rules above do not apply to it and no share is stored. Polygons are never widened.",
                format(GUIDE_PIPELINE$coastal_buffer_m, scientific = FALSE)),
        "Each flood warning area also names its parent flood alert area. For coastal risk, when an alert area is linked to a risk shape, the coastal warning areas inside it are linked too, even where the widened line does not reach them. The pipeline marks those links as raised through the parent alert, and they have no share either.",
        "The data holds constituencies in every nation, because a flood area can run up to a border. The Nations filter under the header chooses which are shown, on every tab: the counts, lists, map layers, changes, MP briefing drafts, exposure history and the CSV exports all follow it. England is the starting choice, because the Environment Agency works in England. Add Wales, Scotland or Northern Ireland to see the constituencies a forecast reaches there. Clearing the box shows all four. A constituency's nation comes from the first three characters of its ONS code: E14 England, W07 Wales, S14 Scotland, N06 Northern Ireland.",
        "The intersection threshold slider on the map filters further, using the overlaps already stored. It changes what the map shows. It does not change the pipeline's data."
      ),
      limits = c(
        sprintf("This deployment reads the \"%s\" shapes. The pipeline can build its overlaps from the risk area polygons or from the Area of Concern map polygons, and the app must be set to match. A mismatch shows an empty or wrong map.", s$geometry_source),
        "The polygons drawn on the map are always the risk area polygons. When the geometry source is the Area of Concern, the flood-area and constituency figures come from those shapes instead."
      )
    ),
    list(
      id = "freshness", title = "Which statement, and how fresh",
      answers = "How do I know the figures are current?",
      paragraphs = c(
        "\"Latest complete statement\" means the newest statement whose overlaps are finished. If a newer statement has arrived but is not finished, the app shows the previous complete one until it is.",
        "The line in the header gives two times. The first is when the pipeline last checked itself, in UTC. The second is the time of the latest statement, shown as stored with no time zone: the FGS is issued at about 09:30 UK time, and labelling it UTC would be wrong in summer.",
        if (s$auto_refresh > 0) sprintf("An open page checks for new data every %s seconds and reloads only if something has changed. Refresh statements forces the check at once.", s$auto_refresh)
        else "Automatic refresh is switched off in this deployment. Refresh statements checks for new data.",
        if (s$cache > 0) sprintf("Query results are shared between users for %s seconds. Refresh statements, and any new pipeline data, clear them.", s$cache)
        else "Query results are not cached in this deployment."
      )
    )
  )
}


# Products: one entry for each thing the app shows.
guide_products <- function(s) {
  th <- GUIDE_PIPELINE
  products <- list(

    list(
      id = "overview", title = "Overview", where = "Overview",
      answers = "What does the latest statement say, and where should I look first?",
      tables = c("fgs_statements", "constituency_flood_situation", "fgs_constituency_changes", "mp_briefing_drafts", "pipeline_health", "flood_warnings_current", "ea_area_exposure", "ea_area_overlaps"),
      steps = c(
        "The headline is the text of the latest statement, as issued.",
        "Warnings and alerts in force counts EA areas, once each however many constituencies they cover: flood alerts, flood warnings and severe flood warnings. Select a count to list the areas.",
        "Who is in these areas (under the counts) adds up the people and households inside the warning areas and alert areas in force, from the address points, and how many of those people are in a High or Medium band for rivers and the sea. Warnings are counted against warnings and alerts against alerts. A person in two areas in force is counted once, and the figures are rounded to the nearest ten, because the counts are estimates. It is left out when the exposure tables are not built.",
        "Highest forecast risk is the highest worst risk of any constituency.",
        "Constituencies at Medium or above counts constituencies whose worst risk over the five days is rank 3 or 4. The figure for High counts rank 4.",
        "Escalated counts constituencies where the forecast rose against the previous statement, on any day or for any flood source. A new risk where there was none counts. So does a rise from Very Low to Low.",
        "Constituencies with a warning or alert in force counts constituencies with at least one Severe Flood Warning, Flood Warning or Flood Alert in force.",
        "MP briefings awaiting review counts drafts whose status is still draft.",
        "The five-day outlook gives, for each day, the highest risk in any constituency and how many constituencies are at Medium or above on that day.",
        "Constituencies to watch lists up to eight constituencies at Low risk or above. They are ordered by worst risk, then severe warnings, then warnings, then escalation, then name."
      ),
      read = c(
        "The counts in the cards are of areas and the constituencies are counts of constituencies. The people and households under the areas in force are in the paragraph beneath the cards, not in the cards.",
        "Click a row in a list to see that constituency on the map."
      ),
      limits = c(
        "The Overview always shows the latest complete statement, whichever statement is picked on the map.",
        "Escalated is a broad measure. On a building event it will exceed the count at Medium or above. Read it as activity, not severity."
      )
    ),

    list(
      id = "interrogator", title = "FGS Interrogator", where = "FGS Interrogator",
      answers = "Where is the risk for one statement and one day, and which flood areas and constituencies does it touch?",
      tables = c("fgs_statements", "fgs_risk_polygons", "fgs_ea_area_intersections", "fgs_constituency_intersections",
                 "ea_flood_warning_areas", "ea_flood_alert_areas", "parliamentary_constituencies",
                 "ea_area_constituency_lookup", "mp_contact_details"),
      steps = c(
        "Pick a statement and a forecast day. The map draws that day's risk polygons, one for each flood source and risk, coloured by risk level.",
        "The source ticks and the risk matrix decide which polygons are shown. The matrix starts with the Minor impact column unticked.",
        sprintf("The flood areas are the stored overlaps for that day. An area is shown if its flood source is ticked, its type (Flood Warning Area or Flood Alert Area) is ticked, the risk it overlaps sits in a ticked cell of the matrix, and its overlap meets the intersection threshold. The slider starts at %s, the pipeline's own rule.",
                guide_pct(th$ea_min_pct)),
        "Flood areas are coloured by type, not by risk: magenta for warning areas and blue for alert areas. They read as context beneath the risk polygons. The list is ordered from Red to Green, then by overlap.",
        "The constituencies are those touched on that day, after the same source, matrix and threshold filters.",
        "The three counts are the polygons drawn, the distinct flood areas and the distinct constituencies, all after the filters.",
        "Export CSV joins each affected flood area to the constituencies it overlaps and their sitting MPs. An area that spans several constituencies gets one row for each."
      ),
      read = c(
        "The list titled \"Affected Target Areas\" is flood areas the forecast overlaps. It is not a list of warnings in force.",
        "Click a row in that list to fly to the area on the map."
      ),
      limits = c(
        "Counts move with the filters. Compare like with like.",
        "The polygons drawn are always the risk area polygons, whichever geometry source the overlaps were built from.",
        "For coastal risk a flood area can be listed without the drawn line touching it. It is within the widened line, or it is a warning area whose parent alert area is linked. Its overlap shows as crossed by a coastal line, because a line has no area to measure. The intersection threshold slider never removes these."
      )
    ),

    list(
      id = "changes", title = "Changed since the previous statement", where = "FGS Interrogator",
      answers = "What is different from the last statement?",
      tables = c("fgs_constituency_changes", "fgs_changes_log"),
      steps = c(
        "The pipeline takes the two newest complete statements.",
        "It compares only the dates both cover. Day 1 of the newer statement is day 2 of the older one, so a day that rolls off the end, or is newly added, is never reported as a change.",
        sprintf("For each constituency, date and flood source it takes the worst risk in each statement. Polygons covering less than %s of a constituency are ignored.", guide_pct(th$contact_min_pct)),
        "It stores every difference and nothing else. A change is new (nothing before, a risk now), escalated (the rank rose), de-escalated (the rank fell but a risk remains) or cleared (a risk before, none now).",
        "It also writes one row to a log for each comparison, including when nothing changed, so an empty result can be told from a comparison that has not run yet.",
        "The same log row counts the constituencies each statement flags over all five of its own days, at any risk and at Medium or above. That is how a forecast that has gone from 30 constituencies at Very Low to none still shows, even when the days it sat on have rolled off the newer statement.",
        "This app reduces the rows to one per constituency: its highest new risk, with a count of other cells that changed. Rises come first. Up means new or escalated; down means de-escalated or cleared."
      ),
      read = c(
        "The strip on the map opens as an overlay. The same list sits on the Overview.",
        "Click a row to see the constituency on the map."
      ),
      limits = c(
        "Only one statement back is compared. The first complete statement has nothing to compare with.",
        "The day-by-day changes leave out days the newer statement no longer covers, on purpose. A risk on a day that has rolled off is not a change in the forecast, so it is not listed there. The headline counts line above them is where it shows.",
        "The history is kept, so any statement's changes can be read. The look-back does not need this table to work, but escalation then shows as not available."
      )
    ),

    list(
      id = "situation", title = "Constituency situation", where = "Constituency situation",
      answers = "For every constituency: what is the forecast, what could be issued, what is in force, and who is the MP?",
      tables = c("constituency_flood_situation", "fgs_constituency_changes", "mp_contact_details", "parliamentary_constituencies",
                 "fgs_ea_area_intersections", "ea_area_constituency_lookup"),
      steps = c(
        "Every constituency has a row, so a constituency with nothing flagged is shown as None.",
        sprintf("For each constituency the pipeline takes the worst risk on each day, across all flood sources. Polygons covering less than %s of the constituency are ignored. A coastal line has no area, so it is matched through the line widened by %s metres and is never dropped by that floor.", guide_pct(th$contact_min_pct), format(GUIDE_PIPELINE$coastal_buffer_m, scientific = FALSE)),
        "Worst is the highest of those five. On is the first date it occurs. Sources are the flood sources at that worst risk, taken across all days.",
        "The five day columns show the worst risk on each day. Escalated is yes if the forecast rose against the previous statement on any day or source.",
        "Impact and Likelihood are those of the forecast polygon behind the worst risk, on the first day it occurs. Where several polygons share that risk on that day, the one with the highest impact is shown, then the highest likelihood. They are read from the stored intersections, so they are available for any statement.",
        "The in force columns count the Severe Flood Warnings, Flood Warnings and Flood Alerts in force that overlap the constituency. An area that spans several constituencies is counted once in each.",
        sprintf("Warning areas and alert areas under the forecast are the potential ones: flood warning areas and flood alert areas that sit under a risk polygon of the statement, on any of the five days. An area counts when at least %s of it lies inside the constituency, and counts once however many days or polygons cover it. The minimum forecast risk filter also sets which polygons count: at Medium, only areas under Medium or High polygons are counted. These are places where a warning or alert could be issued. None of them need be in force.", guide_pct(th$warning_min_pct)),
        "The MP is matched to the constituency on a normalised name (lower case, accents folded, \"&\" as \"and\"). A seat with no match is shown as vacant or unmatched."
      ),
      read = c(
        "The filters narrow the table. Setting the minimum risk to Any includes constituencies with only a warning or alert in force.",
        "Flood source keeps only constituencies with risk from the sources you pick, and recalculates the risk, the five days, Sources, Impact, Likelihood and the areas under the forecast for those sources alone. Escalated and the in force columns stay across all sources. Minimum impact keeps constituencies whose worst-day impact is at least the level chosen.",
        "Select a row to see its MP and briefing draft. Show on map jumps to the constituency."
      ),
      limits = c(
        "The table holds the latest complete statement, whatever is picked on the map.",
        "Forecast and warnings are separate columns on purpose. There is no combined score.",
        "With the minimum risk on Any, an area under a Very Low polygon counts the same as one under a High polygon, so broad low-risk polygons raise the counts. Raise the minimum risk to count only areas under higher-risk polygons. Severe does not apply to a potential warning.",
        "A vacancy and a name that fails to match look the same. The pipeline's health check watches for a jump in unmatched seats."
      )
    ),

    list(
      id = "briefings", title = "MP briefing drafts", where = "Constituency situation",
      answers = "What would we tell an MP whose constituency is at risk?",
      tables = c("mp_briefing_drafts", "constituency_flood_situation"),
      steps = c(
        sprintf("A draft is written for an MP when the forecast for their constituency reaches rank %s (%s) or above on any of the five days. Vacant seats get no draft.", th$briefing_min_rank, RANK_LABELS[th$briefing_min_rank + 1L]),
        "The wording is factual: what the statement says, which days, which flood sources, and whether the risk has risen. It gives no advice and makes no promise. The sender and sign-off are left as placeholders.",
        "Each draft starts with the status draft. A person reviews it and sends it by their own route.",
        "A re-run replaces drafts still marked draft. It leaves any draft a person has moved on exactly as it is. Older statements' drafts are kept."
      ),
      read = c(
        "Nothing is sent from this app or from the pipeline.",
        if (s$demo) "In this demonstration, Mark reviewed, Mark sent and Reject record the action for this session only, and the MP audit log tab lists them. Nothing is saved."
        else "Briefings are read only in the live app. Recording who reviewed or sent one needs a decision on who may write that record."
      ),
      limits = c(
        "A message to an MP is a policy decision and cannot be recalled. The drafts exist to be edited."
      )
    ),

    list(
      id = "risk-layer", title = "Constituency forecast risk (map layer)", where = "FGS Interrogator",
      answers = "Which constituencies carry the highest forecast risk over the whole five days?",
      tables = c("constituency_flood_situation", "parliamentary_constituencies"),
      steps = c(
        "Each constituency with a forecast risk is filled with the colour of its worst risk over the five days, taken from the situation table.",
        "Only constituencies flagged at Very Low or above are drawn, which keeps the layer small."
      ),
      read = c(
        "Click a constituency for its worst risk, the date and sources, escalation, warnings in force and MP."
      ),
      limits = c(
        "The layer follows the pipeline's latest statement. It does not follow the statement or day pickers.",
        "It is separate from the Constituencies outline layer, which does follow the pickers and the matrix filter."
      )
    ),

    list(
      id = "risk-grid", title = "Risk grid (map layer)", where = "FGS Interrogator",
      answers = "Where, street by street, are people, deprived communities and critical sites exposed to flooding from rivers and the sea?",
      tables = c("exposure_grid_100m"),
      steps = c(
        "Why there is a grid. The counts for each warning area come from address points, which is the accurate route: each point is placed inside a boundary and carries its own flood band. The grid is there for what the points cannot do.",
        "It shows where risk concentrates inside an area. A warning area is one row in a table; the grid shows which streets carry the risk, which is what someone prioritising a response needs.",
        "It gives one score. Hazard, population, deprivation and critical sites sit in different tables. In each 100 m cell they combine into a single 0 to 100 score, so cells can be ranked and mapped without reading four tables.",
        "It is a safer thing to show. Address-level data stays in the restricted schema. A 100 m cell is an aggregate, so the app can map it without displaying any individual address.",
        "It is a common base for other geographies. Warning areas, constituencies and fire authorities have awkward, overlapping boundaries. A fixed fine grid is a neutral layer each can be rolled up from, so a new geography, such as local resilience forums later, is a lookup and not a rebuild.",
        "100 m is a compromise: fine enough to see street-scale patterns, coarse enough to cover England in a table the app can query by easting and northing. Large areas are read in coarser squares (200 m to 2 km) so the page stays quick.",
        "Tick Risk grid and zoom to street level. Each 100 m cell in view is filled by its risk score, from 0 to 100.",
        "The score runs from 0 to 100: the cell's rivers and sea band (0 for none to 4 for High) as a share of the top band, multiplied by a weighted mix of three things. No hazard, no score.",
        "The mix is 40% population, 30% vulnerability and 30% critical sites. Population is the cell's people as a national percentile among every cell in England, so a cell near 1 is among the most populous. Vulnerability turns the deprivation decile round: decile 1, the most deprived tenth, scores 1 and decile 10 scores 0.1. Critical sites are the sites in the cell, capped at 3, as a share of 3.",
        "In short: score = 100 x (band / 4) x (0.4 x population percentile + 0.3 x vulnerability + 0.3 x critical sites share). The weights are set in the pipeline's configuration (notebook 34) and can be changed there; the score is never the only output, because the layers behind it are stored too.",
        "The cell's band is an area-weighted average: each NaFRA2 band's rank (Very Low 1 to High 4) times the share of the cell in it, added up, so a cell half High and half no band scores 2. Where bands overlap at a point on the 10 m map the worst band wins. Rivers and the sea only.",
        "Only the cells in view with a score at or above the slider are read, the highest first, up to a cap. The sidebar says when the cap is reached."
      ),
      read = c(
        "Click a cell for its score, people, band, deprivation decile and critical sites.",
        "A pale cell is exposed but has few people or low deprivation. A dark cell has all three."
      ),
      limits = c(
        "Rivers and the sea only. Surface water is not in the score.",
        "People are the Census 2021 output area total shared among the addresses, so a single cell's count is an estimate. Read the pattern, not the digit.",
        "Until AddressBase Premium is available the addresses come from the National Statistics UPRN Lookup, which limits how good any cell-level figure can be, whatever the grid resolution.",
        "Not tied to the forecast: the statement and day pickers do not apply. The figures are restricted."
      )
    ),

    list(
      id = "exposure", title = "Exposure history", where = "Exposure history",
      answers = "Which constituencies are repeatedly forecast at risk, and is this winter unusual?",
      tables = c("fgs_constituency_exposure"),
      steps = c(
        "Statements overlap: each looks five days ahead, so one date is forecast up to five times. To count each date once, the pipeline uses day 1 of the last statement issued on that date. An amended statement replaces the morning issue. Days 2 to 5 are not counted.",
        "For each constituency, calendar month and flood source it counts the days at each risk rank. The source called any is the worst across all sources.",
        "The table is sparse. A month with no flagged day has no row. The number of days in that month with a statement is stored as the denominator.",
        "The grid shows the constituencies with the most days at the chosen measure over the last 12 months, month by month. The share measure divides days at Medium or above by the days covered.",
        "The current month includes today's day 1 forecast, which a later amendment can still change."
      ),
      read = c(
        "A blank cell means no flagged day that month. It does not mean missing data.",
        "Days with no statement are not counted as quiet. The tooltip on each month gives the days covered.",
        "Click a row for that constituency's month-by-month history."
      ),
      limits = c(
        "This measures what was forecast, not what happened. It is a history of forecasts, and it is not a record of observed flooding.",
        "A gap in the statement history shortens a month's denominator. Compare shares, not raw days, across months with gaps."
      )
    ),

    list(
      id = "warning_history", title = "Warning history", where = "Warning history",
      answers = "How many flood alerts and flood warnings have been issued for each constituency, and for which areas?",
      tables = c("ea_warning_episodes", "ea_area_constituency_lookup", "ea_flood_warning_areas", "ea_flood_alert_areas"),
      steps = c(
        "Notebook 25 turns the Environment Agency's message history, and then the live warnings log, into episodes. An episode runs from the message that switched an area on to the message that ended it, at any level. A warning that rises to severe and falls back is one episode.",
        "An issue is one episode. Alerts and warnings are counted separately. Severe is the number of warning episodes that reached severe at any point, so it is part of Warnings and not added to the total. The total is alerts plus warnings.",
        "An area belongs to a constituency when at least the same share of it lies inside as is used for the warning contacts. An area that spans several constituencies counts once in each.",
        "Each issue is dated by the day it was switched on. The slider keeps the issues dated inside the chosen period. Select a row to see each area, how often it was issued and when last.",
        "Tick Detailed to list every alert and warning for the selected constituency instead: the area, when it was issued, when it ended (or In force), how long it lasted, the highest level it reached and whether it went severe. The same period applies.",
        "The graph under the table counts the alerts and warnings in force at the same time. Each issue steps the line up when it starts and down when it ends. It counts areas, not people or properties."
      ),
      read = c(
        "The total counts issues, not days in force and not people affected. Duration is held in the episodes table but is not shown here.",
        "Constituencies with no issue in the period are left out of the table.",
        "In the graph and the Detailed table, an alert or warning with no end message for over 30 days is treated as one that never received it. The table marks it as still open; the graph leaves it out.",
        "The Nations filter at the top applies."
      ),
      limits = c(
        "History before the live log began comes from the message file, which can be incomplete. Notebook 25 reports where the two sources meet.",
        "A coastal warning area is a line, linked to constituencies on a 1000 m buffer, so a coastal area can count towards a constituency it only touches."
      )
    ),

    list(
      id = "coverage", title = "Warnings vs FGS", where = "Warnings vs FGS",
      answers = "Was this warning under an FGS polygon on the day it was issued, and was the forecast made before the day or on it? Across a period, how often did the polygons cover the warnings?",
      tables = c("ea_warning_fgs_coverage", "fgs_ea_area_intersections", "fgs_statements"),
      steps = c(
        "Each alert and warning is an episode from the warning history: one area, from the day it was issued to the day it ended.",
        "The pipeline finds every complete FGS statement whose polygon was linked to that area for a date the warning was in force. A link is an overlap, or, for a coastal warning area, the alert area above it being under the polygon.",
        "The reference is the day the warning was issued. Where there is an impact the FGS keeps a coloured polygon over the area for the whole warning, so polygons on later days say nothing about timing. The day each statement covering the issue day was issued is compared with the issue day, and the earliest decides: an earlier day is Before (the FGS gave notice, and the table says how many days) and the same day is Same day.",
        "If no polygon covered the issue day but one covered a later day the warning was in force, it is Later days only: the FGS caught up, or the impact came after the warning. If none covered any day it is Not covered.",
        "A Very Low polygon covers a great deal of England, so you choose how serious a polygon must be to count. The table and chart show all four levels side by side for the period and kind of warning you chose.",
        "Select a warning to list the statements behind it: when each was issued compared with the warning, whether it covers the issue day or a later one, the source, the risk, and whether it overlaps the area or reaches it through the parent alert."
      ),
      read = c(
        "Covered on the issue day is Before plus Same day. The share covered Before is the closest to saying the polygons gave notice.",
        "Expect the share covered Before to fall as the polygon must be more serious: High polygons are rare.",
        "Days are compared, not hours, because statement and message times are not held in one zone. The Timing column in the statement list gives the hours between them."
      ),
      limits = c(
        "Not covered is a disagreement between two forecasts of a different kind, not a missed forecast: the FGS forecasts risk across regions and the Environment Agency warns for particular rivers and coasts.",
        "Polygons that had no warning under them are not counted. This shows how often warnings were covered, not how often polygons were followed by warnings.",
        "A warning open for over 30 days is treated as having lost its end message and counts only for its first day. A span is capped at 60 days."
      )
    ),

    list(
      id = "warning-areas", title = "Warning areas", where = "Warning areas",
      answers = "How many people and households live inside each flood warning area and flood alert area, and how many of them are in a High or Medium band for flooding from rivers and the sea?",
      tables = c("ea_area_exposure", "ea_area_overlaps", "ea_area_coverage", "constituency_flood_exposure", "ea_area_sites", "ea_area_geographies"),
      steps = c(
        "Notebook 33 places every address in England as a point and gives it its share of the Census 2021 output area's people and households. Notebook 35 then finds which warning and alert area boundaries contain each point.",
        "Each point carries the rivers and sea band (NaFRA2) and Flood Zone at its position, so an area's people can be split by band. At risk means High or Medium.",
        "Areas of one kind overlap. Gross counts everyone inside the area. Unique counts only people in no other area of that kind. Split shares each person equally between the areas they are in, so split figures add up to the true total.",
        "Download a one-page brief on the Target Area page for a printable page of the figures, the band split, the critical sites in a High or Medium band, who responds and the constituencies. It is a PDF, ready to print or send, and is marked restricted.",

        "The risk score in the table is the people-weighted average of the 100 m cell scores inside the area, so an area where many people live in flooded, deprived or critical cells scores high and an area of mostly dry land scores low. The detail page also gives the highest single cell. It is blank until job 35 has been run with the grid built.",
        "Linked gauges on the Area detail page lists every Observed flood warning threshold that notebook 28 linked to a gauge for the area, with the gauge's newest level, how far below or above each threshold it is, the trend and a status. Select a gauge name to open River levels and flows at that gauge's station with the thresholds drawn. A reading older than three hours is shown as no recent reading.",
        "Warning issue history on the Area detail page lists every alert or warning issued for the area, from the warning episodes table (notebook 25): how many in the last 12 months and in all, how many reached a severe warning, the median time in force, a month by month chart and each episode with when it was issued, when it ended and its highest level. An episode open for over 30 days is marked, because it most likely never received its end message.",

        "Warning issue history on the Target Area page lists every alert or warning issued for the area, from the warning episodes table (notebook 25): how many in the last 12 months and in all, how many reached a severe warning, the median time in force, a month by month chart and each episode with when it was issued, when it ended and its highest level. An episode open for over 30 days is marked, because it most likely never received its end message.",
        "Select an area for its figures, the 100 m grid inside it, its people by band, the named critical sites in a High or Medium band, and the areas that share people with it.",
        "On the Forecast map, tick Risk grid and zoom in to street level: each 100 m cell is coloured by its risk score, the rivers and sea band weighted by people, deprivation and critical sites. Only the cells in view with a score at or above the slider are read, the highest first, up to a cap.",
        "Who responds lists the police forces, fire and rescue authorities, combined authorities, districts, counties, constituencies and regions the area lies in. Every address point carries each of these codes from the National Statistics UPRN Lookup, so the area's people are split among them. Each table adds up to the area's people. Fire and rescue authorities and combined authorities are not in that lookup, so they are reached another way: the fire authority from the boundary file, through the output area each address is in, and the combined authority from the district it is in. Combined authorities cover only part of England, so that table can add up to less than the area's people. Wards are in the export only.",
        "The coverage panel counts every person once and shows how many of those in a High or Medium band sit in no warning area, and in no alert area. The constituency view gives the same split for each constituency."
      ),
      read = c(
        "Warning areas and alert areas are counted against their own kind. Do not add a warning area figure to an alert area figure.",
        "Share at risk is the share of the area's people in a High or Medium band, not the chance of flooding."
      ),
      limits = c(
        "Rivers and the sea only. Surface water is not counted: the Environment Agency does not warn for it.",
        "Until AddressBase Premium is available the points come from the National Statistics UPRN Lookup, so counts are of addresses, not of buildings or businesses, and England only.",
        "The figures are restricted. Small counts are shown as they are held; treat them with care before sharing."
      )
    ),

    list(
      id = "hydrometrics", title = "Hydrometrics", where = "Hydrometrics",
      answers = "What have river levels, flows and rainfall been doing at the Environment Agency's gauges?",
      tables = c("ea_rt_measure_register", "ea_rt_readings_all", "ea_rt_readings_bronze", "ea_rt_measures_mapped", "ea_fw_threshold_gauge_links"),
      steps = c(
        "The measure register lists every measure: a level, flow or rainfall series at a station. Where the telemetry mapping has matched a measure, a rain gauge the API calls only \"Rainfall station\" takes its real name, and the label carries its WISKI and FFFS ids, so you can search by either. The five drop-downs (EA area, river, parameter, station and FFFS ID) narrow each other, so a choice never leads to an empty list. Pick an FFFS ID, the key the thresholds use, to go straight to that gauge's measures; a gauge with two measures of one kind (a rain gauge's instantaneous and total readings, or a tide gauge in two datums) shows both.",
        "Choose up to eight measures and how far back to look, from 1 day to everything held. Readings are fetched only for those measures and that window, from one series that joins the daily archive to the live tail.",
        "Up to about 15 days every reading is drawn. Beyond that the warehouse summarises each measure to about 1,500 points, so a long window stays quick: level and flow are the mean of the readings in each point, and rainfall is the total. A short window shows the detail.",
        "Each unit gets its own panel, because metres and cubic metres a second do not share an axis.",
        "The dashed line is midnight at the end of the last archived day. The shaded band to its right is the live tail, which the daily archive has not yet picked up.",
        "Tick \"Show flood warning thresholds\" to draw a red dashed line across the panel for each Observed flood warning threshold linked to a chosen measure (notebook 28). A line is in the measure's own unit: where the thresholds file gave it on another datum, notebook 28 has moved it to the gauge's datum, and the label says so. The label is the file's label description prefix and the target area id, for example RES FW 065FWF3007. Tick \"Only the RES thresholds\" to hide the others (the RES level is where impacts are expected). Thresholds are restricted, so the app needs access to that schema; without it, or before notebook 28 has run, the tick says they are not available."
      ),
      read = c(
        "Times are UTC. Hover for the value of every line at that time.",
        "The text under the chart gives the number of readings, how many are archived and live, and how old the newest is.",
        "Not tied to the forecast: nothing here depends on the statement or day pickers. The Nations filter does not apply."
      ),
      limits = c(
        "These are readings as the real-time API supplies them, unchecked. They are not quality-controlled and are not a record of flooding.",
        "Only the first 500 measures matching the filters are offered. Narrow by river or station to find the rest.",
        "Uses Environment Agency flood and river level data from the real-time data API (Beta), under the Open Government Licence."
      )
    ),
    list(
      id = "threshold-board", title = "Gauges near thresholds", where = "Gauges near thresholds",
      answers = "Which flood warning areas have a linked gauge close to its warning threshold, and how many people are at risk below them?",
      tables = c("ea_fw_threshold_gauge_links", "ea_rt_latest", "ea_rt_readings_all", "ea_rt_measure_register", "ea_area_exposure"),
      steps = c(
        "Notebook 28 links each Observed flood warning threshold to the gauge measure that records it. The page reads those links with the newest level of each linked gauge, from the live feed.",
        "For each warning area and gauge, the lowest Response threshold is used, because that is the level at which impacts are expected. Where an area has no Response threshold, its lowest threshold of any kind is used.",
        "Headroom is the threshold minus the newest level, in metres. Because it is a difference on one gauge, it needs no datum. Choose how close counts with Show gauges within.",
        "The trend compares the newest level with the average two to four hours earlier. A level rising by more than 0.01 metres an hour is rising, and for those the page gives a straight-line time to the threshold.",
        "Rows are ordered: at or above the threshold first, then close and rising by the soonest time to threshold, then close. Ties go to the area with more people at risk.",
        "The people at risk are the people in the warning area who are in a High or Medium band for rivers and the sea. Select an area name to open its detail page.",
        "Two flags sit beside each row. In force now is the severest alert or warning the Environment Agency feed has in force for that area. Potential is set where nothing is in force but the area lies under a risk polygon of Low or above in the latest Flood Guidance Statement, with the highest risk and the first forecast day. Potential means a warning could be issued, not that one will be."
      ),
      read = c(
        "At or above means the newest level is at or over the lowest threshold used. It does not mean a warning has been issued.",
        "Time to threshold is shown only for a rising level. It is not a forecast and ignores rain still to come."
      ),
      limits = c(
        "Only thresholds that notebook 28 could link to a gauge appear. A threshold whose unit disagrees with the gauge's is never linked, so some areas are missing.",
        "A reading older than three hours is left out, because a stale level would give false comfort.",
        "The thresholds are in the restricted schema, so the app needs access to it."
      )
    ),

    list(
      id = "constituency-exposure", title = "Constituency exposure", where = "Constituency exposure",
      answers = "Which constituencies have the most people at risk from rivers and the sea that no warning area covers, and what is in force or possible there now?",
      tables = c("constituency_flood_exposure", "constituency_sites", "constituency_flood_situation", "flood_warnings_current", "fgs_ea_area_intersections", "ea_area_constituency_lookup", "ea_warning_episodes"),
      steps = c(
        "Each address point carries its parliamentary constituency, so the people in a High or Medium rivers and sea band are counted per constituency, and so are those who are in a flood warning area or a flood alert area. The gap is the people at risk who are in none.",
        "In force now counts the severe flood warnings, flood warnings and flood alerts the Environment Agency feed has in force in areas linked to the constituency.",
        "Potential counts the flood warning and flood alert areas that lie under a risk polygon in the latest Flood Guidance Statement, at or above the risk you choose. It means a warning could be issued there. It is not a prediction that one will be. An area already in force is listed as in force and not again as potential.",
        "An area is linked to a constituency when at least the app's minimum share of it lies inside (the same rule as the MP warning contacts), so an area that spans two constituencies counts in both.",
        "Select a constituency for its detail, and for a one-page PDF brief: the MP, the figures, what is in force, the potential areas, the five day forecast, the critical sites in a High or Medium band and the warning history."
      ),
      read = c(
        "Tick the filter to show only constituencies with something in force or potential.",
        "The gap is measured against flood warning areas or flood alert areas, as you choose. Warnings are not added to alerts."
      ),
      limits = c(
        "Rivers and the sea only: surface water is not counted.",
        "The critical sites come from the interim sources until AddressBase Premium is available, so the counts are of the sites those sources hold.",
        "England only. The figures are restricted."
      )
    ),

    list(
      id = "rain-gauges", title = "Rain gauges", where = "Rain gauges",
      answers = "How much rain has each gauge caught in the last hour, 12 hours and 24 hours, and where is it falling?",
      tables = c("ea_rt_measure_register", "ea_rt_readings_all", "ea_rt_latest", "ea_rt_measures_mapped"),
      steps = c(
        "One query returns every rainfall measure that has a position, its newest reading, and its rainfall over the past 1, 12 and 24 hours. The totals add up the readings in each window.",
        "A gauge can report both 15-minute totals and instantaneous readings. It is drawn once, on its totals.",
        "Each gauge is a circle coloured by its total for the window you choose, in six bands from the palest (almost nothing) to the darkest (the heaviest). The legend gives the bands in millimetres, which differ by window because an hour's rain is smaller than a day's.",
        "Click a gauge for its name, its station reference and FFFS and WISKI ids where the telemetry mapping holds them, its latest reading and when it was taken, the three totals, and how many of the expected readings it reported in 24 hours. Use Find a gauge to fly to one by name, station or FFFS ID.",
        "Clicking a gauge adds it to a bar chart of its past 24 hours beside the map: the rain in each hour, or in each 15 minutes if you switch the bars. You can chart up to six gauges, one panel each on the same scale so they compare; click more to add, remove one in the Charted gauges box, or press Clear. Adding a seventh drops the oldest. Finding a gauge charts it too. One query fetches the charted gauges' readings when the set changes, and switching the bar width does not fetch again."
      ),
      read = c(
        "A grey gauge has had no reading for three hours. Its last reading is in the popup, but it is not counted as dry.",
        "\"No readings\" in a total means the gauge sent nothing in that window. It is not the same as 0.0 mm, which means it reported and caught nothing.",
        "A total over fewer readings than expected is probably low. The line at the foot of the popup says how many arrived.",
        "Times are UTC. Not tied to the forecast: the statement and day pickers and the Nations filter do not apply."
      ),
      limits = c(
        "These are readings as the real-time API supplies them, unchecked. They are not quality-controlled and are not a record of rainfall.",
        "Every rainfall gauge in the register is shown, whether or not the telemetry mapping gives it an FFFS ID; the summary line says how many have none. Some stations are given only as an Ordnance Survey easting and northing, and these are placed from it. A gauge with no position at all cannot be drawn, and the summary line says how many there are.",
        "A single reading below zero or over the limit cannot be rain. The limit starts at 100 mm and you can change it in the box above the map. Readings over it are left out of every total, and the popup says how many were ignored.",
        "A gauge is suspect when it sent such readings, or when its total is more than any UK gauge has recorded (over 80 mm in an hour, 200 mm in 12 hours or 300 mm in 24 hours). A suspect gauge has a dashed outline, its popup says why, it is not counted as the wettest or as having rain, and a tick box hides it. This is a guard against faulty data, not a quality control of the gauge.",
        "Uses Environment Agency flood and river level data from the real-time data API (Beta), under the Open Government Licence."
      )
    ),

    list(
      id = "health", title = "Pipeline status and the banners", where = "Header",
      answers = "Can I trust what I am looking at?",
      tables = c("pipeline_health"),
      steps = c(
        "The pipeline writes one row per check of its own outputs, and an overall row. Each check reads ok, info, warn or fail. Info is worth knowing and is not a problem.",
        sprintf("The checks are: the age of the newest statement (warn after %s hours, fail after %s); the age of the situation table, which proves the polling chain ran (%s hours); the age of the warnings ingest (%s hours); MP details (warn after %s days, fail after %s); and whether the statements are all intersected, whether warnings match a constituency, and whether MPs match.",
                th$health_fgs[1], th$health_fgs[2], paste(th$health_chain, collapse = " and "),
                paste(th$health_warnings, collapse = " and "), th$health_mp[1], th$health_mp[2]),
        sprintf("A further check watches the live river and sea readings the pipeline keeps for every site. It warns when the newest reading is over %s hour old, and it never fails, because a stale feed is usually a bad spell at the Environment Agency and must not fail the run that carries the forecast and warnings alerts.", th$health_live),
        sprintf("Two completeness checks fail if there are fewer than %s constituencies or fewer than %s MPs.", th$min_constituencies, th$min_mps),
        "The badge in the header shows the overall result. Click it for every check."
      ),
      read = c(
        sprintf("The red or amber banner appears when a check is failing or needs attention. It also appears when the health table has not updated for %s hours, which means the pipeline may have stopped: nothing ran, so nothing failed.", s$stale_hours),
        "A second banner names any pipeline table that does not exist yet, and the notebook that builds it. A view that depends on a missing table says so in words.",
        "A brief warning appears if the Databricks warehouse cannot be reached. The app retries a network failure twice before giving up, keeps the last data, and tries again on the next refresh."
      ),
      limits = c(
        "The thresholds above are the pipeline's defaults. They are set in its config file, which this app cannot read."
      )
    )
  )

  if (s$poc) {   # POC-ONLY: remove with the look-back
    products <- c(products, list(list(
      id = "lookback", title = "Statement look-back", where = "Constituency situation",
      answers = "What did an earlier statement say, constituency by constituency?",
      poc = TRUE,
      tables = c("fgs_constituency_intersections", "fgs_constituency_changes", "mp_contact_details",
                 "parliamentary_constituencies", "mp_briefing_drafts"),
      steps = c(
        "The situation table holds the latest statement only. For an earlier statement the app rebuilds it from the overlaps the pipeline keeps for every statement.",
        sprintf("It applies the same rules as the pipeline: polygons covering less than %s of a constituency are ignored, then the worst risk per day, then the worst of the five, with its first date and sources.", guide_pct(s$contact_min_pct)),
        "Escalation comes from the changes table for that statement. Briefing drafts come from the drafts the pipeline kept for it. The warning and alert areas under the forecast are rebuilt from the area overlaps kept for every statement, so they work for any past date.",
        "Choosing the newest statement gives the live table, with its warnings."
      ),
      read = c(
        "A box above the table says when a look-back is on."
      ),
      limits = c(
        "Warnings and alerts in force are not kept for past statements, so they show not available. The areas under the forecast are kept, and are shown.",
        "MP details are today's, matched on a normalised name.",
        "If the changes table does not exist, escalation shows not available. It does not show none.",
        "Reviews cannot be recorded against a past statement.",
        "This is a proof-of-concept feature. It is marked in the code and switched off by one setting before go-live."
      )
    )))
  }

  if (s$demo) {
    products <- c(products, list(list(
      id = "demo", title = "This demonstration", where = "Header",
      answers = "What is real here, and what is not?",
      demo = TRUE,
      tables = character(),
      steps = c(
        "Every figure, name and shape is generated. Nothing is read from a warehouse, and nothing is written to one.",
        "The map is a hexagon layout of a rough outline of England. It is not constituency boundaries. Constituency names, MPs, rivers and flood areas are invented. Email addresses end in example.invalid and cannot be delivered.",
        "One model feeds every table. A storm moves north-east across the country over five days. For each statement, day and flood source it gives every hexagon a likelihood and an impact. Those map to a risk level through the same matrix as the live app, and everything else is derived from that one field, so the figures agree with each other.",
        "The three scenarios change how strong the storm is. The pipeline problem switch makes one health check fail so that the banners can be seen."
      ),
      read = c(
        "The calculations described elsewhere in this guide are the live app's. The demonstration runs the same code on generated data."
      ),
      limits = c(
        "Reviews and the audit log live in this browser session and are lost when the page closes."
      )
    )))
  }
  products
}


guide_glossary <- function() {
  list(
    c("AOC", "Area of Concern. The detailed map section of a Flood Guidance Statement."),
    c("EA", "Environment Agency."),
    c("FFC", "Flood Forecasting Centre, which issues the Flood Guidance Statement."),
    c("FGS", "Flood Guidance Statement: the daily five-day forecast of flood risk for England and Wales."),
    c("Flood Alert Area", "An area in which the EA may issue a Flood Alert, the lowest of its three warning levels."),
    c("Flood Warning Area", "An area in which the EA may issue a Flood Warning or a Severe Flood Warning."),
    c("Flood source", "River, surface water, coastal or groundwater. Each has its own risk areas."),
    c("Impact", "How serious the consequences would be: Minor, Moderate, Significant or Extreme."),
    c("Likelihood", "How likely the impact is: Low (10 to 25 per cent), Medium (25 to 50), High (50 to 75) or Very High (over 75)."),
    c("Rank", "The risk level as a number: 1 Very Low, 2 Low, 3 Medium, 4 High. Zero means nothing is flagged."),
    c("Poll", "One run of the pipeline's polling job, every 30 or 60 minutes."),
    c("Intersection", "The link between an FGS shape and a flood area or a constituency, and its size. It is usually an overlap. Coastal lines are widened first, and coastal warning areas can be linked through their parent alert area, so a link is not always a plain overlap, and then it has no size.")
  )
}


# =============================================================================
# THE WORKED EXAMPLE
# =============================================================================

# guide_example_text(): the explanation for one real row of the situation table.
# Pure, so a test can check it. `row` is one row of the situation table, `n_medium`
# the number of constituencies at Medium or above, `base` the first forecast date.
guide_example_text <- function(row, n_medium, base = NA) {
  plural <- function(n, noun) sprintf("%s %s%s", n, noun, if (isTRUE(as.integer(n) == 1L)) "" else "s")
  lvl <- function(r) RANK_LABELS[pmax(0L, pmin(4L, as.integer(r))) + 1L]
  days <- vapply(1:5, function(d) {
    when <- if (is.na(base)) paste0("Day ", d) else format(base + d - 1L, "%a %d %b")
    sprintf("%s %s", when, lvl(row[[paste0("fgs_rank_day", d)]]))
  }, character(1))
  worst <- as.integer(row$fgs_worst_rank)
  c(
    sprintf("Take %s. The worst risk on each of the five days is: %s.", row$constituency_name, paste(days, collapse = "; ")),
    sprintf("The highest of those is %s. It is first reached on %s, from %s. That is the Worst column%s.",
            lvl(worst), if (is.na(row$fgs_worst_date)) "no date" else format(as.Date(row$fgs_worst_date), "%A %d %B"),
            if (is.na(row$fgs_worst_sources)) "no source" else row$fgs_worst_sources,
            if (worst >= 3L) sprintf(", and because it is rank 3 or 4 this constituency is one of the %s counted as Medium or above", n_medium) else ""),
    if (isTRUE(row$fgs_escalated)) "It is marked escalated: the forecast rose against the previous statement on at least one day or for at least one flood source."
    else if (isFALSE(row$fgs_escalated)) "It is not marked escalated: no day or flood source rose against the previous statement."
    else "Escalation is not available for this statement.",
    if (isTRUE(row$has_active_warning)) sprintf("It has %s, %s and %s in force. These are counted separately from the forecast, and are not part of the risk above.",
                                                  plural(row$severe_warnings, "severe flood warning"), plural(row$warnings, "flood warning"),
                                                  plural(row$alerts, "flood alert"))
    else if (isFALSE(row$has_active_warning)) "It has no warning or alert in force. The forecast and the warnings are separate measures, so a constituency can sit at High risk with none issued."
    else "Warnings in force are not available for this statement.",
    if (isTRUE(row$mp_matched)) sprintf("Its MP is %s, matched on the constituency name.", row$mp_name)
    else "No MP is matched to this constituency: a vacancy, or a name that does not match."
  )
}


# =============================================================================
# RENDERING
# =============================================================================

# guide_matrix_ui(): the 4 x 4 matrix, drawn from the same RISK_MATRIX the map uses.
guide_matrix_ui <- function() {
  head <- tags$tr(tags$th(""), lapply(seq_along(IMPACT_LABELS), function(x) tags$th(scope = "col", IMPACT_LABELS[x])))
  rows <- lapply(4:1, function(lik) {
    tags$tr(
      tags$th(scope = "row", tags$div(LIKELIHOOD_LABELS[lik]), tags$small(class = "text-muted", LIKELIHOOD_BANDS[lik])),
      lapply(1:4, function(imp) {
        # (named lik and imp, not x and y: inside a data.table call those are the columns)
        colour <- RISK_MATRIX$colour[RISK_MATRIX$x == imp & RISK_MATRIX$y == lik]
        level  <- RISK_LEVEL_BY_COLOUR[[colour]]
        tags$td(class = paste("guide-cell", tolower(colour)), `data-level` = level, level)
      })
    )
  })
  tags$div(class = "guide-matrix-wrap",
    tags$table(class = "guide-matrix",
      tags$caption("Risk level by likelihood (rows) and impact (columns)"),
      tags$thead(head), tags$tbody(rows)))
}

guide_list <- function(x, ordered = FALSE) {
  x <- x[!vapply(x, is.null, logical(1))]
  if (length(x) == 0L) return(NULL)
  (if (ordered) tags$ol else tags$ul)(lapply(x, tags$li))
}

guide_card <- function(p, gaps = list()) {
  missing <- intersect(p$tables, names(gaps))
  tags$section(
    id = paste0("guide-", p$id), class = paste("guide-card", if (isTRUE(p$poc)) "guide-poc"),
    tags$div(class = "guide-head",
      tags$h3(p$title),
      if (isTRUE(p$poc)) tags$span(class = "poc-tag", "POC only"),
      if (length(missing) > 0L) tags$span(class = "guide-gap", "Not built yet in this warehouse")),
    if (!is.null(p$answers)) tags$p(class = "guide-q", tags$strong("Answers: "), p$answers),
    if (!is.null(p$where)) tags$p(class = "guide-where", tags$span("Where: "), p$where),
    if (length(p$paragraphs)) lapply(p$paragraphs, tags$p),
    if (identical(p$show, "matrix")) guide_matrix_ui(),
    if (length(p$steps)) tagList(tags$h4("How it is calculated"), guide_list(as.list(p$steps), ordered = TRUE)),
    if (length(p$read)) tagList(tags$h4("How to read it"), guide_list(as.list(p$read))),
    if (length(p$limits)) tagList(tags$h4("What to keep in mind"), guide_list(as.list(p$limits))),
    if (length(p$tables)) tags$p(class = "guide-tables", tags$span("Built from: "),
      paste(vapply(p$tables, function(t) {
        j <- GUIDE_TABLES$notebook[GUIDE_TABLES$table == t]
        if (length(j) == 1L) sprintf("%s (notebook %s)", t, j) else t
      }, character(1)), collapse = "; "))
  )
}

guide_settings_ui <- function(s) {
  rows <- list(
    c("Mode", if (s$demo) "Demonstration, on generated data" else "Live, reading the warehouse"),
    if (!s$demo) c("Tables read from", paste0(s$catalog, ".", s$schema)),
    c("Geometry source", s$geometry_source),
    c("Nations shown at the start", paste(names(s$nations)[match(s$default_nations, s$nations)], collapse = ", ")),
    c("Constituency overlap floor (this app, look-back only)", guide_pct(s$contact_min_pct)),
    c("Automatic refresh", if (s$auto_refresh > 0) sprintf("Every %s seconds", s$auto_refresh) else "Off"),
    c("Stale-data banner after", sprintf("%s hours without a health update", s$stale_hours)),
    c("Query cache", if (s$cache > 0) sprintf("%s seconds", s$cache) else "Off"),
    c("Proof-of-concept features", if (s$poc) "On" else "Off")
  )
  rows <- rows[!vapply(rows, is.null, logical(1))]
  tags$table(class = "guide-table",
    tags$tbody(lapply(rows, function(r) tags$tr(tags$th(scope = "row", r[1]), tags$td(r[2])))))
}

guide_lineage_ui <- function(products, gaps = list()) {
  used <- unique(unlist(lapply(products, function(p) p$tables)))
  t <- GUIDE_TABLES[GUIDE_TABLES$table %in% used, ]
  tags$table(class = "guide-table guide-lineage",
    tags$thead(tags$tr(tags$th("Table"), tags$th("Notebook"), tags$th("Refreshed"), tags$th("What it holds"), tags$th("Used by"))),
    tags$tbody(lapply(seq_len(nrow(t)), function(i) {
      users <- vapply(Filter(function(p) t$table[i] %in% p$tables, products), function(p) p$title, character(1))
      tags$tr(
        tags$td(tags$code(t$table[i]), if (t$table[i] %in% names(gaps)) tags$span(class = "guide-gap", "not found")),
        tags$td(t$notebook[i]), tags$td(t$refresh[i]), tags$td(t$holds[i]), tags$td(paste(users, collapse = "; ")))
    })))
}

guide_toc_items <- function(s) {
  c(lapply(guide_foundations(s), function(p) c(p$id, p$title)),
    list(c("example", "A worked example")),
    lapply(guide_products(s), function(p) c(p$id, p$title)),
    list(c("lineage", "Where each table comes from"), c("settings", "How this deployment is set up"), c("glossary", "Glossary")))
}

guide_toc_ui <- function(s) {
  tags$nav(class = "guide-nav", `aria-label` = "On this page",
    tags$div(class = "guide-nav-h", "On this page"),
    tags$ul(lapply(guide_toc_items(s), function(it) {
      tags$li(tags$a(href = paste0("#guide-", it[1]),
                     onclick = sprintf("var e=document.getElementById('guide-%s');if(e){e.scrollIntoView({behavior:'smooth',block:'start'});}return false;", it[1]),
                     it[2]))
    })))
}

# guide_body_ui(): the whole body. `gaps` is the named list of missing tables,
# `sit` the latest situation table (or NULL), used for the worked example.
guide_body_ui <- function(s, gaps = list(), sit = NULL) {
  foundations <- guide_foundations(s)
  products <- guide_products(s)

  example <- NULL
  if (!is.null(sit) && nrow(sit) > 0L) {
    top <- sit[order(-fgs_worst_rank, -severe_warnings, -warnings, constituency_name)][1L]
    base <- tryCatch(as.Date(substr(top$fgs_issued_at, 1L, 10L)), error = function(e) NA)
    example <- tags$section(id = "guide-example", class = "guide-card",
      tags$div(class = "guide-head", tags$h3("A worked example")),
      tags$p(class = "guide-q", tags$strong("Answers: "), "How does one constituency's row on the Overview and Constituency situation come about?"),
      tags$p(class = "text-muted", "This uses the constituency at the top of the latest statement, so it changes with the data."),
      lapply(guide_example_text(top, sum(sit$fgs_worst_rank >= 3L, na.rm = TRUE), base), tags$p))
  }

  tags$div(class = "guide-content",
    tags$div(class = "guide-intro",
      tags$h2("Guide"),
      tags$p("Each figure in this app comes from a table the pipeline builds. This guide says how every one is calculated, how to read it, and where it stops being reliable. It describes the live calculations; the settings of this deployment are at the foot."),
      tags$input(type = "search", class = "form-control guide-search", placeholder = "Search the guide", `aria-label` = "Search the guide",
                 oninput = "var q=this.value.toLowerCase();document.querySelectorAll('.guide-card').forEach(function(c){c.style.display=(q===''||c.innerText.toLowerCase().indexOf(q)>-1)?'':'none';});")),
    tags$h3(class = "guide-part", "The ideas behind the figures"),
    lapply(foundations, guide_card, gaps = gaps),
    example,
    tags$h3(class = "guide-part", "Each product"),
    lapply(products, guide_card, gaps = gaps),
    tags$section(id = "guide-lineage", class = "guide-card",
      tags$div(class = "guide-head", tags$h3("Where each table comes from")),
      guide_lineage_ui(products, gaps)),
    tags$section(id = "guide-settings", class = "guide-card",
      tags$div(class = "guide-head", tags$h3("How this deployment is set up")),
      guide_settings_ui(s)),
    tags$section(id = "guide-glossary", class = "guide-card",
      tags$div(class = "guide-head", tags$h3("Glossary")),
      tags$dl(class = "guide-glossary", lapply(guide_glossary(), function(g) tagList(tags$dt(g[1]), tags$dd(g[2])))))
  )
}

make_guide_tab <- function() {
  s <- guide_settings()
  tags$div(class = "guide p-3", guide_toc_ui(s), uiOutput("guide_body"))
}

# guide_settings(): the live settings the text depends on, from the globals.
guide_settings <- function() {
  list(
    geometry_source = GEOMETRY_SOURCE, contact_min_pct = CONTACT_MIN_PCT,
    default_nations = DEFAULT_NATIONS, nations = NATIONS,
    auto_refresh = AUTO_REFRESH_SECONDS, stale_hours = STALE_HOURS, cache = CACHE_TTL_SECONDS,
    poc = POC_FEATURES, demo = DEMO_MODE, catalog = CAT, schema = SCH)
}
