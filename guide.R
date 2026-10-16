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
  briefing_min_rank  = 3L,      # BRIEFING_MIN_RISK_RANK
  ea_min_pct         = 0.25,    # EA_INTERSECTION_MIN_PCT
  contact_min_pct    = 0.01,    # FGS_CONTACT_MIN_PCT
  warning_min_pct    = 0.01,    # WARNING_CONTACT_MIN_PCT
  health_fgs         = c(30, 54),   # HEALTH_FGS_WARN_HOURS, HEALTH_FGS_FAIL_HOURS
  health_chain       = c(3, 12),    # HEALTH_CONTACTS_*  (situation table age)
  health_warnings    = c(3, 12),    # HEALTH_WARNINGS_*  (warnings ingest age)
  health_mp          = c(10, 21),   # HEALTH_MP_WARN_DAYS, HEALTH_MP_FAIL_DAYS
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
            "fgs_constituency_changes", "constituency_flood_situation", "mp_briefing_drafts",
            "fgs_constituency_exposure", "pipeline_health"),
  notebook = c("01", "01", "07", "07", "03", "03", "05", "06", "04", "18", "19", "20", "21", "22"),
  refresh = c("Each FGS issue or amendment", "Each FGS issue or amendment", "Each FGS issue", "Each FGS issue",
              "Weekly", "Weekly", "Quarterly, or after an election", "Weekly and quarterly", "Weekly",
              "Every poll", "Every poll", "Every poll", "Daily", "Every poll"),
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
            "One row per constituency: the forecast, the warnings in force and the MP. The latest statement only.",
            "Draft briefings for MPs. Nothing is sent. Older statements' drafts are kept.",
            "Days each constituency sat at each risk level, by month. Sparse.",
            "One row per health check of the pipeline's own outputs."),
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
        "The risk colours are fixed by the User Guide and always carry their name as text. The green of this app's header is the Environment Agency's, not a risk colour."
      )
    ),
    list(
      id = "forecast-warnings", title = "Forecast and warnings are different things",
      answers = "Why is there no single risk score for a constituency?",
      paragraphs = c(
        "A forecast says what may happen over the next five days. A warning says what the Environment Agency expects, and has issued. They measure different things, so this app keeps them apart and never combines them into one score.",
        "Warnings come in three levels in force: Severe Flood Warning, Flood Warning and Flood Alert. A fourth state, a warning that has ended, is kept in the data so that a closing warning stays visible. It is not counted as in force.",
        "The map's list titled \"Affected EA Warnings\" is not a list of warnings in force. It lists the flood warning areas and flood alert areas that the forecast overlaps: places where a warning or alert could be issued. The warnings actually in force are counted on the Overview and on Constituency situation."
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
        sprintf("A risk polygon counts against an EA flood area when it covers at least %s of that area. Coastal risk is drawn as a line, which has no area, so coastal lines are always kept.",
                guide_pct(GUIDE_PIPELINE$ea_min_pct)),
        sprintf("A risk polygon counts against a constituency on any real overlap, but links covering less than %s of the constituency are dropped, so a sliver at an edge does not flag a seat. Coastal lines are kept.",
                guide_pct(GUIDE_PIPELINE$contact_min_pct)),
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
      tables = c("fgs_statements", "constituency_flood_situation", "fgs_constituency_changes", "mp_briefing_drafts", "pipeline_health"),
      steps = c(
        "The headline is the text of the latest statement, as issued.",
        "Highest forecast risk is the highest worst risk of any constituency.",
        "Constituencies at Medium or above counts constituencies whose worst risk over the five days is rank 3 or 4. The figure for High counts rank 4.",
        "Escalated counts constituencies where the forecast rose against the previous statement, on any day or for any flood source. A new risk where there was none counts. So does a rise from Very Low to Low.",
        "Constituencies with a warning or alert in force counts constituencies with at least one Severe Flood Warning, Flood Warning or Flood Alert in force.",
        "MP briefings awaiting review counts drafts whose status is still draft.",
        "The five-day outlook gives, for each day, the highest risk in any constituency and how many constituencies are at Medium or above on that day.",
        "Constituencies to watch lists up to eight constituencies at Low risk or above. They are ordered by worst risk, then severe warnings, then warnings, then escalation, then name."
      ),
      read = c(
        "Counts are of constituencies. They are not counts of people, homes or businesses.",
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
        sprintf("The flood areas are the stored overlaps for that day. An area is shown if its flood source is ticked, its type (Flood Warning Area or Flood Alert Area) is ticked, and its overlap meets the intersection threshold. The slider starts at %s, the pipeline's own rule.",
                guide_pct(th$ea_min_pct)),
        "Flood areas are coloured by type, not by risk: magenta for warning areas and blue for alert areas. They read as context beneath the risk polygons. The list is ordered from Red to Green, then by overlap.",
        "The constituencies are those touched on that day, after the same source, matrix and threshold filters.",
        "The three counts are the polygons drawn, the distinct flood areas and the distinct constituencies, all after the filters.",
        "Export CSV joins each affected flood area to the constituencies it overlaps and their sitting MPs. An area that spans several constituencies gets one row for each."
      ),
      read = c(
        "The list titled \"Affected EA Warnings\" is flood areas the forecast overlaps. It is not a list of warnings in force.",
        "Click a row in that list to fly to the area on the map."
      ),
      limits = c(
        "Counts move with the filters. Compare like with like.",
        "The polygons drawn are always the risk area polygons, whichever geometry source the overlaps were built from."
      )
    ),

    list(
      id = "changes", title = "Changed since the previous statement", where = "FGS Interrogator",
      answers = "What is different from the last statement?",
      tables = c("fgs_constituency_changes"),
      steps = c(
        "The pipeline takes the two newest complete statements.",
        "It compares only the dates both cover. Day 1 of the newer statement is day 2 of the older one, so a day that rolls off the end, or is newly added, is never reported as a change.",
        sprintf("For each constituency, date and flood source it takes the worst risk in each statement. Polygons covering less than %s of a constituency are ignored.", guide_pct(th$contact_min_pct)),
        "It stores every difference and nothing else. A change is new (nothing before, a risk now), escalated (the rank rose), de-escalated (the rank fell but a risk remains) or cleared (a risk before, none now).",
        "This app reduces the rows to one per constituency: its highest new risk, with a count of other cells that changed. Rises come first. Up means new or escalated; down means de-escalated or cleared."
      ),
      read = c(
        "The strip on the map opens as an overlay. The same list sits on the Overview.",
        "Click a row to see the constituency on the map."
      ),
      limits = c(
        "Only one statement back is compared. The first complete statement has nothing to compare with.",
        "The history is kept, so any statement's changes can be read. The look-back does not need this table to work, but escalation then shows as not available."
      )
    ),

    list(
      id = "situation", title = "Constituency situation", where = "Constituency situation",
      answers = "For every constituency: what is the forecast, what is in force, and who is the MP?",
      tables = c("constituency_flood_situation", "fgs_constituency_changes", "mp_contact_details", "parliamentary_constituencies"),
      steps = c(
        "Every constituency has a row, so a constituency with nothing flagged is shown as None.",
        sprintf("For each constituency the pipeline takes the worst risk on each day, across all flood sources. Polygons covering less than %s of the constituency are ignored.", guide_pct(th$contact_min_pct)),
        "Worst is the highest of those five. On is the first date it occurs. Sources are the flood sources at that worst risk, taken across all days.",
        "The five day columns show the worst risk on each day. Escalated is yes if the forecast rose against the previous statement on any day or source.",
        "Severe, Warnings and Alerts count the Severe Flood Warnings, Flood Warnings and Flood Alerts in force that overlap the constituency. An area that spans several constituencies is counted once in each.",
        "The MP is matched to the constituency on a normalised name (lower case, accents folded, \"&\" as \"and\"). A seat with no match is shown as vacant or unmatched."
      ),
      read = c(
        "The filters narrow the table. Setting the minimum risk to Any includes constituencies with only a warning or alert in force.",
        "Select a row to see its MP and briefing draft. Show on map jumps to the constituency."
      ),
      limits = c(
        "The table holds the latest complete statement, whatever is picked on the map.",
        "Forecast and warnings are separate columns on purpose. There is no combined score.",
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
      id = "health", title = "Pipeline status and the banners", where = "Header",
      answers = "Can I trust what I am looking at?",
      tables = c("pipeline_health"),
      steps = c(
        "The pipeline writes one row per check of its own outputs, and an overall row. Each check reads ok, info, warn or fail. Info is worth knowing and is not a problem.",
        sprintf("The checks are: the age of the newest statement (warn after %s hours, fail after %s); the age of the situation table, which proves the polling chain ran (%s hours); the age of the warnings ingest (%s hours); MP details (warn after %s days, fail after %s); and whether the statements are all intersected, whether warnings match a constituency, and whether MPs match.",
                th$health_fgs[1], th$health_fgs[2], paste(th$health_chain, collapse = " and "),
                paste(th$health_warnings, collapse = " and "), th$health_mp[1], th$health_mp[2]),
        sprintf("Two completeness checks fail if there are fewer than %s constituencies or fewer than %s MPs.", th$min_constituencies, th$min_mps),
        "The badge in the header shows the overall result. Click it for every check."
      ),
      read = c(
        sprintf("The red or amber banner appears when a check is failing or needs attention. It also appears when the health table has not updated for %s hours, which means the pipeline may have stopped: nothing ran, so nothing failed.", s$stale_hours),
        "A second banner names any pipeline table that does not exist yet, and the notebook that builds it. A view that depends on a missing table says so in words.",
        "A brief warning appears if the Databricks warehouse cannot be reached. The app keeps the last data and tries again."
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
        "Escalation comes from the changes table for that statement. Briefing drafts come from the drafts the pipeline kept for it.",
        "Choosing the newest statement gives the live table, with its warnings."
      ),
      read = c(
        "A box above the table says when a look-back is on."
      ),
      limits = c(
        "Warnings and alerts in force are not kept for past statements, so they show not available.",
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
    c("Intersection", "The overlap of an FGS shape with a flood area or a constituency, and its size.")
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
    auto_refresh = AUTO_REFRESH_SECONDS, stale_hours = STALE_HOURS, cache = CACHE_TTL_SECONDS,
    poc = POC_FEATURES, demo = DEMO_MODE, catalog = CAT, schema = SCH)
}
