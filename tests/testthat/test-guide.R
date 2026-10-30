# The Guide tab: its structure, its truth and its prose.

consts <- load_app_constants()
guide_env <- local({
  e <- new.env(parent = globalenv())
  for (n in ls(consts, all.names = TRUE)) assign(n, get(n, consts), envir = e)
  source(file.path(app_root, "guide.R"), local = e)
  e
})
settings <- function(...) modifyList(list(geometry_source = "risk_areas", contact_min_pct = 0.01, auto_refresh = 120,
                                          default_nations = "E14", nations = c(England = "E14", Wales = "W07", Scotland = "S14", "Northern Ireland" = "N06"),
                                          stale_hours = 3, cache = 600, poc = TRUE, demo = FALSE,
                                          catalog = "prd_dash_lab", schema = "flood_forecasting_unrestricted"), list(...))
html <- function(x) gsub("\\s+", " ", as.character(x))

# ---- structure ------------------------------------------------------------------

test_that("every section has the fields its card needs, and ids are unique", {
  s <- settings(poc = TRUE, demo = TRUE)
  all_sections <- c(guide_env$guide_foundations(s), guide_env$guide_products(s))
  ids <- vapply(all_sections, function(p) p$id, character(1))
  expect_equal(anyDuplicated(ids), 0L)
  for (p in all_sections) {
    expect_true(nzchar(p$title), info = p$id)
    expect_true(nzchar(p$answers), info = paste(p$id, "answers"))
  }
  for (p in guide_env$guide_products(s)) {
    expect_gt(length(p$steps), 0L); expect_gt(length(p$read), 0L); expect_gt(length(p$limits), 0L)
    expect_true(nzchar(p$where), info = p$id)
  }
})

test_that("every app tab that shows a product has an entry in the Guide", {
  where <- vapply(guide_env$guide_products(settings()), function(p) p$where, character(1))
  expect_true(all(c("Overview", "FGS Interrogator", "Constituency situation", "Exposure history") %in% where))
})

# ---- truth ------------------------------------------------------------------------

test_that("every table the app reads is documented, and every documented table is used", {
  tbl_consts <- grep("^TBL_", ls(consts), value = TRUE)
  read_by_app <- sub("^.*\\.", "", vapply(tbl_consts, function(n) get(n, consts), character(1)))
  documented <- guide_env$GUIDE_TABLES$table
  expect_setequal(setdiff(read_by_app, documented), character())             # nothing undocumented
  used <- unique(unlist(lapply(guide_env$guide_products(settings(poc = TRUE)), function(p) p$tables)))
  expect_setequal(setdiff(documented, used), character())                    # nothing orphaned
  expect_setequal(setdiff(used, documented), character())
})

test_that("the notebook numbers in the Guide agree with the gap messages", {
  g <- guide_env$GUIDE_TABLES
  for (tb in names(PIPELINE_TABLE_JOBS)) {
    if (tb %in% g$table) expect_equal(as.integer(g$notebook[g$table == tb]), PIPELINE_TABLE_JOBS[[tb]], info = tb)
  }
})

test_that("the figures in the text match the pipeline's config.py when it is alongside", {
  cfg <- file.path(dirname(app_root), "FGS_Notebooks", "config.py")
  skip_if_not(file.exists(cfg), "FGS_Notebooks is not alongside this repository")
  txt <- readLines(cfg, warn = FALSE)
  val <- function(name) {
    l <- grep(paste0("^", name, "\\s*="), txt, value = TRUE)
    expect_length(l, 1L)
    as.numeric(sub("^[^=]*=\\s*([0-9.]+).*$", "\\1", l))
  }
  d <- guide_env$GUIDE_PIPELINE
  expect_equal(d$briefing_min_rank, val("BRIEFING_MIN_RISK_RANK"))
  expect_equal(d$ea_min_pct, val("EA_INTERSECTION_MIN_PCT"))
  expect_equal(d$contact_min_pct, val("FGS_CONTACT_MIN_PCT"))
  expect_equal(d$coastal_buffer_m, val("COASTAL_LINE_BUFFER_M"))
  expect_true(any(grepl("^COASTAL_LINK_WARNINGS_TO_ALERTS\\s*=\\s*True", txt)),       # the Guide says warnings are linked through the parent alert
              info = "COASTAL_LINK_WARNINGS_TO_ALERTS is no longer True in config.py: update the Guide's linking section")
  expect_equal(d$warning_min_pct, val("WARNING_CONTACT_MIN_PCT"))
  expect_equal(d$health_fgs, c(val("HEALTH_FGS_WARN_HOURS"), val("HEALTH_FGS_FAIL_HOURS")))
  expect_equal(d$health_chain, c(val("HEALTH_CONTACTS_WARN_HOURS"), val("HEALTH_CONTACTS_FAIL_HOURS")))
  expect_equal(d$health_warnings, c(val("HEALTH_WARNINGS_WARN_HOURS"), val("HEALTH_WARNINGS_FAIL_HOURS")))
  expect_equal(d$health_mp, c(val("HEALTH_MP_WARN_DAYS"), val("HEALTH_MP_FAIL_DAYS")))
  expect_equal(d$health_live, val("HEALTH_LIVE_WARN_HOURS"))
  expect_equal(d$min_constituencies, val("MIN_CONSTITUENCIES"))
  expect_equal(d$min_mps, val("MIN_MPS"))
})

test_that("the risk matrix in the Guide is the matrix the map uses", {
  m <- html(guide_env$guide_matrix_ui())
  cells <- regmatches(m, gregexpr('<td class="guide-cell [a-z]+" data-level="[A-Za-z ]+"', m))[[1]]
  expect_length(cells, 16L)
  for (imp in 1:4) for (lik in 1:4) {
    colour <- consts$RISK_MATRIX$colour[consts$RISK_MATRIX$x == imp & consts$RISK_MATRIX$y == lik]
    expect_true(any(grepl(sprintf('guide-cell %s" data-level="%s"', tolower(colour), consts$RISK_LEVEL_BY_COLOUR[[colour]]), cells, fixed = TRUE)))
  }
  expect_equal(sum(grepl("Very Low", cells)), sum(consts$RISK_MATRIX$colour == "Green"))
  for (l in consts$IMPACT_LABELS) expect_match(m, l, fixed = TRUE)
  for (b in consts$LIKELIHOOD_BANDS) expect_match(m, gsub(">", "&gt;", b, fixed = TRUE), fixed = TRUE)
})

test_that("the Guide says plainly that the map's flood-area list is not warnings in force", {
  txt <- html(guide_env$guide_body_ui(settings()))
  expect_match(txt, "is not a list of warnings in force")
  expect_match(txt, "Observed river and sea levels")                  # the known gap is stated, not hidden
  expect_match(txt, "Forecast and warnings are separate columns on purpose")
  expect_match(txt, "This measures what was forecast, not what happened")
})

# ---- rendering ---------------------------------------------------------------------

test_that("the body renders every section, and every contents link has a target", {
  s <- settings(poc = TRUE, demo = TRUE)
  sit <- NULL
  body <- html(guide_env$guide_body_ui(s, sit = sit))
  toc <- html(guide_env$guide_toc_ui(s))
  for (it in guide_env$guide_toc_items(s)) {
    expect_match(toc, paste0("#guide-", it[1]), fixed = TRUE)
    if (it[1] != "example") expect_match(body, paste0('id="guide-', it[1], '"'), fixed = TRUE, info = it[1])
  }
  for (p in c(guide_env$guide_foundations(s), guide_env$guide_products(s))) expect_match(body, p$title, fixed = TRUE)
})

test_that("the look-back and demonstration sections appear only when those modes are on", {
  live <- html(guide_env$guide_body_ui(settings(poc = FALSE, demo = FALSE)))
  expect_false(grepl("Statement look-back", live)); expect_false(grepl("This demonstration", live))
  poc <- html(guide_env$guide_body_ui(settings(poc = TRUE, demo = FALSE)))
  expect_match(poc, "Statement look-back"); expect_match(poc, "POC only"); expect_false(grepl("This demonstration", poc))
  demo <- html(guide_env$guide_body_ui(settings(poc = FALSE, demo = TRUE)))
  expect_match(demo, "This demonstration"); expect_match(demo, "example.invalid")
})

test_that("the text follows the live settings", {
  a <- html(guide_env$guide_body_ui(settings(geometry_source = "aoc", auto_refresh = 0, cache = 0, stale_hours = 6, contact_min_pct = 0.05)))
  expect_match(a, 'reads the \\\\?"aoc\\\\?" shapes|reads the "aoc" shapes')
  expect_match(a, "Automatic refresh is switched off")
  expect_match(a, "Query results are not cached")
  expect_match(a, "not updated for 6 hours")
  expect_match(a, "5 per cent")
  b <- html(guide_env$guide_body_ui(settings(auto_refresh = 90, cache = 300)))
  expect_match(b, "every 90 seconds"); expect_match(b, "shared between users for 300 seconds")
})

test_that("a table the warehouse does not have is flagged on the products that need it", {
  body <- html(guide_env$guide_body_ui(settings(), gaps = list(fgs_constituency_changes = 18L)))
  expect_match(body, "Not built yet in this warehouse")
  expect_match(body, "not found")                                             # in the lineage table
  clean <- html(guide_env$guide_body_ui(settings()))
  expect_false(grepl("Not built yet in this warehouse", clean))
})

test_that("the lineage table lists every table used, with its notebook and how often it is refreshed", {
  lin <- html(guide_env$guide_lineage_ui(guide_env$guide_products(settings())))
  for (i in seq_len(nrow(guide_env$GUIDE_TABLES))) {
    expect_match(lin, guide_env$GUIDE_TABLES$table[i], fixed = TRUE)
    expect_match(lin, guide_env$GUIDE_TABLES$refresh[i], fixed = TRUE)
  }
})

# ---- the worked example ---------------------------------------------------------------

example_row <- function(...) {
  base <- data.table(constituency_name = "Alpha", fgs_worst_rank = 4L, fgs_worst_date = "2026-10-17", fgs_worst_sources = "river, surface",
    fgs_rank_day1 = 1L, fgs_rank_day2 = 4L, fgs_rank_day3 = 3L, fgs_rank_day4 = 0L, fgs_rank_day5 = 0L,
    fgs_escalated = TRUE, has_active_warning = TRUE, severe_warnings = 1L, warnings = 2L, alerts = 0L, mp_matched = TRUE, mp_name = "A MP")
  for (n in names(list(...))) set(base, j = n, value = list(...)[[n]])
  base
}

test_that("the worked example explains one row in order, with real numbers", {
  t <- paste(guide_env$guide_example_text(example_row(), 25L, base = as.Date("2026-10-16")), collapse = " ")
  expect_match(t, "Take Alpha")
  expect_match(t, "Fri 16 Oct Very Low; Sat 17 Oct High; Sun 18 Oct Medium; Mon 19 Oct None; Tue 20 Oct None")
  expect_match(t, "highest of those is High")
  expect_match(t, "Saturday 17 October, from river, surface")
  expect_match(t, "one of the 25 counted as Medium or above")
  expect_match(t, "marked escalated")
  expect_match(t, "1 severe flood warning, 2 flood warnings and 0 flood alerts in force")
  expect_match(t, "separate from the forecast|counted separately from the forecast")
  expect_match(t, "Its MP is A MP")
})

test_that("the worked example does not overclaim for a low, quiet or unknown row", {
  low <- paste(guide_env$guide_example_text(example_row(fgs_worst_rank = 2L, fgs_escalated = FALSE, has_active_warning = FALSE, mp_matched = FALSE), 25L), collapse = " ")
  expect_false(grepl("counted as Medium or above", low))
  expect_match(low, "not marked escalated"); expect_match(low, "no warning or alert in force"); expect_match(low, "No MP is matched")
  unk <- paste(guide_env$guide_example_text(example_row(fgs_escalated = NA, has_active_warning = NA), 25L), collapse = " ")
  expect_match(unk, "Escalation is not available"); expect_match(unk, "Warnings in force are not available")
  none <- paste(guide_env$guide_example_text(example_row(fgs_worst_rank = 0L, fgs_worst_date = NA_character_, fgs_worst_sources = NA_character_), 0L), collapse = " ")
  expect_match(none, "no date"); expect_match(none, "no source")
})

test_that("the example uses the top constituency of a situation table", {
  sit <- data.table(constituency_name = c("Low one", "Top one"), fgs_worst_rank = c(2L, 4L), fgs_worst_date = "2026-10-17",
    fgs_worst_sources = "river", fgs_issued_at = "2026-10-16T09:30:00", fgs_rank_day1 = c(2L, 4L), fgs_rank_day2 = 0L, fgs_rank_day3 = 0L,
    fgs_rank_day4 = 0L, fgs_rank_day5 = 0L, fgs_escalated = c(FALSE, TRUE), has_active_warning = c(FALSE, TRUE),
    severe_warnings = c(0L, 1L), warnings = 0L, alerts = 0L, mp_matched = TRUE, mp_name = c("L", "T"))
  b <- html(guide_env$guide_body_ui(settings(), sit = sit))
  expect_match(b, "Take Top one"); expect_false(grepl("Take Low one", b))
  expect_match(b, "Fri 16 Oct High")
})

# ---- prose ----------------------------------------------------------------------------

all_text <- function() {
  s <- settings(poc = TRUE, demo = TRUE)
  parts <- c(unlist(guide_env$guide_foundations(s)), unlist(guide_env$guide_products(s)),
             unlist(guide_env$guide_glossary()), unlist(guide_env$GUIDE_TABLES),
             unlist(guide_env$guide_example_text(example_row(), 25L, base = as.Date("2026-10-16"))))
  unname(parts[nzchar(parts)])
}

test_that("the Guide follows the house style", {
  txt <- all_text()
  expect_gt(length(txt), 100L)
  bad <- function(pattern) grep(pattern, txt, value = TRUE, ignore.case = TRUE, perl = TRUE)
  expect_equal(bad("—"), character())                 # no em dashes
  expect_equal(bad("–"), character())                 # no en dashes
  expect_equal(bad(" -- |--"), character())
  expect_equal(bad("!"), character())                      # no exclamation marks
  for (p in c("in order to", "leverage", "utilis", "going forward", "it is worth noting", "at this point in time",
              "ensure that stakeholders"))
    expect_equal(bad(p), character(), info = p)
  # UK spelling
  for (p in c("\\bcolor", "\\banalyz", "\\bbehavior", "\\borganiz", "\\bcenter\\b", "\\bgray\\b", "\\bmodeling"))
    expect_equal(bad(p), character(), info = p)
})

test_that("no sentence in the Guide is left unfinished or contains a placeholder", {
  txt <- all_text()
  expect_equal(grep("NA\\b|NULL|TODO|\\{|\\}|%s|%d", txt, value = TRUE), character())
  prose <- txt[nchar(txt) > 60 & !grepl("^[A-Z0-9_a-z]+$", txt)]
  expect_true(all(grepl("[.?:)\"]$", prose)), info = paste(head(prose[!grepl("[.?:)\"]$", prose)], 2), collapse = " | "))
})


test_that("the Guide explains the Nations filter, and the settings table names the starting nations", {
  body <- html(guide_env$guide_body_ui(settings(), gaps = list(), sit = NULL))
  expect_match(body, "The Nations filter under the header chooses which are shown")
  expect_match(body, "England is the starting choice")
  expect_match(html(guide_env$guide_settings_ui(settings())), "Nations shown at the start.{0,40}England")
  expect_match(html(guide_env$guide_settings_ui(settings(default_nations = c("E14", "W07")))), "England, Wales")
})


test_that("the Guide carries the prompt for finding the damage figures", {
  txt <- html(guide_env$guide_body_ui(settings(poc = TRUE)))
  expect_match(txt, "Economics: finding the damage figures", fixed = TRUE)
  expect_match(txt, "taken only from the files you can access", fixed = TRUE)
  expect_match(txt, "Do not convert one into the other", fixed = TRUE)
})
