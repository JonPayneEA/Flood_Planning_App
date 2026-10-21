# =============================================================================
# demo.R
# Synthetic data for demo mode (FGS_DEMO_MODE=1). Sourced at the end of data.R,
# where it replaces every fetch_* function.
#
# NOTHING HERE TOUCHES A WAREHOUSE. There is no connection, no query and no
# write. All data is generated in memory from a fixed seed, so a given scenario
# looks the same every time.
#
# EVERYTHING IS FICTIONAL. The map is a hexagon cartogram of a rough outline of
# England, not real constituency boundaries. Constituency names, MPs, flood
# areas and rivers are invented, and email addresses use example.invalid, which
# can never be delivered. No real person is represented.
#
# ONE MODEL FEEDS EVERY TABLE. A storm track moves north-east across the
# country over five days. For each statement, day and flood source it gives
# every hexagon a likelihood and an impact, which map to a risk level through
# the same RISK_MATRIX the live app uses. Risk polygons, constituency and
# flood-area overlaps, the situation table, the changes since the previous
# statement and the briefing drafts are all derived from that one field, so
# they agree with each other.
#
# SCENARIOS (chosen per session in the demo controls):
#   quiet       almost nothing flagged
#   developing  Medium risk building over the north west (the default)
#   severe      High risk over a wide area
# =============================================================================

DEMO_SCENARIOS <- c("Quiet day" = "quiet", "Developing event" = "developing", "Severe event" = "severe")

.demo_env <- new.env(parent = emptyenv())

demo_memo <- function(key, expr) {
  if (!is.null(.demo_env[[key]])) return(.demo_env[[key]])
  value <- force(expr)
  assign(key, value, envir = .demo_env)
  value
}

# Run code with a fixed RNG seed without disturbing the process's own RNG.
demo_with_seed <- function(seed, expr) {
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had) old <- get(".Random.seed", envir = globalenv())
  on.exit({
    if (had) assign(".Random.seed", old, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  })
  set.seed(seed)
  force(expr)
}

# Scenario and "simulate a pipeline problem" for THIS session. Held in the
# session, so two people using one deployed demo never affect each other.
demo_current <- function() {
  ses <- shiny::getDefaultReactiveDomain()
  ud  <- if (!is.null(ses)) ses$userData else NULL
  scn <- if (!is.null(ud) && !is.null(ud$demo_scenario)) ud$demo_scenario else "developing"
  list(scenario = scn, problem = !is.null(ud) && isTRUE(ud$demo_problem))
}


# =============================================================================
# GEOMETRY: a hexagon cartogram of England
# =============================================================================

demo_geometry <- function() demo_memo("geometry", demo_with_seed(20261016, {
  outline <- rbind(
    c(-5.70, 50.05), c(-4.90, 50.35), c(-4.20, 50.35), c(-3.40, 50.60), c(-2.40, 50.60),
    c(-1.20, 50.75), c(-0.10, 50.80), c(0.90, 50.85), c(1.40, 51.15), c(1.40, 51.40),
    c(0.60, 51.50), c(1.20, 51.80), c(1.70, 52.50), c(1.75, 52.90), c(0.40, 52.95),
    c(0.10, 53.20), c(0.10, 53.55), c(-0.40, 53.70), c(-0.10, 54.10), c(-0.90, 54.50),
    c(-1.40, 54.70), c(-1.60, 55.30), c(-1.90, 55.65), c(-2.10, 55.70), c(-2.40, 55.30),
    c(-3.00, 55.00), c(-3.40, 54.90), c(-3.60, 54.50), c(-3.30, 54.10), c(-2.90, 54.10),
    c(-3.00, 53.50), c(-3.20, 53.30), c(-3.00, 53.20), c(-2.90, 52.90), c(-3.00, 52.50),
    c(-3.10, 52.00), c(-2.70, 51.60), c(-3.00, 51.20), c(-3.80, 51.20), c(-4.50, 51.00),
    c(-4.60, 50.75), c(-5.00, 50.60), c(-5.50, 50.20), c(-5.70, 50.05))
  eng <- sf::st_sfc(sf::st_polygon(list(outline)), crs = 4326)
  eng_m <- sf::st_make_valid(sf::st_transform(eng, 3857))

  # Hexagons are laid out in Web Mercator, so they look regular on a web map.
  grid <- sf::st_make_grid(eng_m, cellsize = 52000, square = FALSE)
  keep <- lengths(sf::st_intersects(sf::st_centroid(grid), eng_m)) > 0
  hex  <- grid[keep]
  # Order west to east, then north to south, so ids are stable and readable.
  cen_m <- sf::st_coordinates(sf::st_centroid(hex))
  o <- order(round(cen_m[, 1] / 60000), -cen_m[, 2])
  hex <- hex[o]
  n <- length(hex)

  cen4326 <- sf::st_coordinates(sf::st_transform(sf::st_centroid(hex), 4326))
  coast_dist <- as.numeric(sf::st_distance(sf::st_centroid(hex), sf::st_boundary(eng_m)))

  stems <- c("Aldwyn","Brenmoor","Cardell","Dunmere","Eskwick","Fennick","Garrowby","Halvern",
             "Istwell","Jorvale","Kendrith","Lowmere","Marrick","Netherby","Orsley","Pennock",
             "Quillby","Ravensdale","Skellmoor","Tarnwick","Ulverby","Varnley","Wendmoor","Yarrowby",
             "Axholme","Bretby","Corvale","Drumlin","Elderby","Fallowmere","Grisedale","Hexley",
             "Inglemoor","Kirkvale","Langwith","Moorcote","Northwick","Oxmere","Pellham","Redmarsh")
  dirs <- c("North","South","East","West","Central","Vale","Heath","Moor","Coast","Hills")
  nm <- character(n); used <- character()
  for (i in seq_len(n)) repeat {
    cand <- if (runif(1) < 0.5) paste(sample(stems, 1), sample(dirs, 1))
            else sprintf("%s and %s", sample(stems, 1), sample(stems, 1))
    if (!(cand %in% used)) { nm[i] <- cand; used <- c(used, cand); break }
  }

  first <- c("Alex","Sam","Jo","Chris","Priya","Tom","Amara","Rhys","Ella","Imran","Hannah","Kofi","Maeve",
             "Dev","Lucy","Owen","Sofia","Callum","Nia","Ravi","Esme","Tariq","Freya","Jonas","Leila","Marcus")
  last  <- c("Ashdown","Penrose","Halloran","Okafor","Varley","Lindqvist","Marchetti","Thackeray",
             "Bellweather","Corrigan","Duquesne","Fairweather","Gantry","Holloway","Iverson","Jessop",
             "Kettleby","Lovelace","Merrow","Nightingale","Ormerod","Pargeter","Quenby","Radcliffe",
             "Sandiford","Trevelyan","Underhill","Vickery","Wadsworth","Yardley")
  mp_name <- character(n); seen <- character()
  for (i in seq_len(n)) repeat {
    cand <- paste(sample(first, 1), sample(last, 1))
    if (!(cand %in% seen)) { mp_name[i] <- cand; seen <- c(seen, cand); break }
  }
  vacant <- sample(n, 2L)
  mp_name[vacant] <- NA_character_
  parties <- sample(c("Civic Party", "Union Party", "Independent"), n, replace = TRUE, prob = c(.5, .35, .15))

  list(
    n = n,
    hex_m = hex,
    hex_area = as.numeric(sf::st_area(hex)),
    hex4326 = sf::st_transform(hex, 4326),
    eng_m = eng_m,
    table = data.table(
      constituency_id   = sprintf("DEMO-C%03d", seq_len(n)),
      constituency_name = nm,
      lon = cen4326[, 1], lat = cen4326[, 2],
      vuln = sample(1:4, n, replace = TRUE, prob = c(.30, .35, .25, .10)),
      coastal = coast_dist < 40000,
      mp_name = mp_name, mp_party = parties,
      mp_email = fifelse(is.na(mp_name), NA_character_,
                         paste0(tolower(gsub("[^A-Za-z]+", ".", mp_name)), ".mp@example.invalid")),
      mp_phone = fifelse(is.na(mp_name), NA_character_, "0000 000 0000")
    )
  )
}))

# Fictional flood warning and flood alert areas: small discs placed over the map.
demo_ea_areas <- function() demo_memo("ea_areas", demo_with_seed(7711, {
  g <- demo_geometry()
  n <- 96L
  idx <- sample(g$n, n)
  rivers <- c("Aldwyn","Brenn","Cardell","Dunmere","Esk","Fenn","Garrow","Halvern","Ister","Jorrow","Kend","Lowe")
  disc <- sf::st_buffer(sf::st_centroid(g$hex_m[idx]), dist = runif(n, 12000, 22000))
  type <- sample(c("flood_warning", "flood_alert"), n, replace = TRUE, prob = c(.35, .65))
  list(
    geom_m = disc, geom4326 = sf::st_transform(disc, 4326), area_m2 = as.numeric(sf::st_area(disc)),
    table = data.table(
      ea_area_code = sprintf("DEMO-%s-%03d", ifelse(type == "flood_warning", "FWA", "FAA"), seq_len(n)),
      ea_area_name = sprintf("River %s at %s", sample(rivers, n, replace = TRUE),
                             sub(" .*", "", g$table$constituency_name[idx])),
      ea_area_type = type
    )
  )
}))


# =============================================================================
# THE RISK FIELD
# =============================================================================

DEMO_LEVELS <- c("Very Low", "Low", "Medium", "High")

# The storm is anchored to the calendar, not to a forecast day number, so two
# statements that cover the same date describe the same storm and differ only
# because the later one is more (or less) confident. k is whole days from
# today: k = 0 is today, k = 4 is four days ahead, k < 0 is the build-up.
DEMO_K_RANGE <- -7:4
demo_track  <- function(k) cbind(lon = -3.4 + 0.75 * k, lat = 54.7 - 0.40 * k)
demo_day_f  <- function(k) {
  f <- c(`-7` = .10, `-6` = .10, `-5` = .15, `-4` = .25, `-3` = .35, `-2` = .50, `-1` = .70,
         `0` = .95, `1` = 1.00, `2` = .90, `3` = .65, `4` = .45)
  unname(f[as.character(pmin(4L, pmax(-7L, k)))])
}

demo_statements_table <- function(scn) demo_memo(paste0("stmts_", scn), {
  today  <- Sys.Date()
  ids    <- 4805:4812
  scales <- switch(scn,
    quiet      = rep(0.18, 8),
    developing = c(.15, .22, .32, .45, .60, .75, .88, .95),
    severe     = c(.20, .30, .50, .70, .90, 1.10, 1.30, 1.55))
  top <- scales[8]
  headline <- function(s) {
    if (s < 0.30) return("Little or no flood risk across England over the next five days.")
    if (s < 0.75) return("Showers and longer spells of rain bring a Low flood risk from rivers and surface water in the north west of England, with a chance of Medium in places.")
    if (s < 1.20) return("Heavy rain moving north-east brings a Medium flood risk from rivers and surface water, peaking over the north west and northern England on days 2 and 3.")
    "Prolonged heavy rain on saturated ground brings a High flood risk from rivers and surface water across northern England, with flooding of homes and businesses likely and disruption to travel."
  }
  data.table(
    statement_id = ids,
    issued_at    = format(as.POSIXct(paste(today - 7:0, "09:30:00"), tz = "UTC"), "%Y-%m-%dT%H:%M:%S"),
    scale        = scales,
    headline     = vapply(scales, headline, character(1)),
    source_coastal = ifelse(scales > 0.6, "Large waves along western coasts may bring spray and overtopping at high tide.", "No significant coastal flood risk."),
    source_surface = ifelse(scales > 0.3, "Intense downpours may cause surface water flooding, especially in urban areas.", "Surface water flooding is unlikely."),
    source_ground  = ifelse(scales > 0.9, "Groundwater levels are rising in vulnerable areas.", "No groundwater flood risk."),
    source_river   = ifelse(scales > 0.3, "River levels will rise and stay high; responsive catchments will react quickly.", "River levels are normal."),
    england_forecast = ifelse(scales > 0.3, "An active frontal system crosses the country from the west, slowing over the north west.", "Settled for most of England."),
    pdf_url = NA_character_
  )
})

# Noise per cell, source and calendar day, fixed for the whole process.
demo_noise <- function() demo_memo("noise", demo_with_seed(4242, {
  n <- demo_geometry()$n
  array(runif(n * 4L * length(DEMO_K_RANGE), -0.07, 0.07), dim = c(n, 4L, length(DEMO_K_RANGE)))
}))

DEMO_SOURCES <- c("river", "surface", "coastal", "ground")

# Per-hexagon intensity (0 to about 1.5) for one scale, calendar day k and source.
demo_intensity <- function(scale, k, source) {
  g <- demo_geometry()$table
  aspect <- 1.66   # 1 / cos(53 degrees): keeps the storm round on the map
  field <- function(kk) {
    kk <- max(min(DEMO_K_RANGE), kk)
    cen <- demo_track(kk)
    dx <- g$lon - cen[1L, "lon"]; dy <- (g$lat - cen[1L, "lat"]) * aspect
    demo_day_f(kk) * exp(-(dx^2 + dy^2) / (2 * 1.5^2))
  }
  base <- switch(source,
    surface = field(k),
    river   = 0.75 * field(k) + 0.55 * field(k - 1L),
    coastal = ifelse(g$coastal & g$lon < -2.5, 0.85 * field(k), 0),
    ground  = 0.45 * (0.75 * field(k - 1L) + 0.55 * field(k - 2L)))
  noise <- demo_noise()[, match(source, DEMO_SOURCES), match(k, DEMO_K_RANGE)]
  pmax(0, scale * base + ifelse(base > 0.05, noise, 0))
}

# Flagged hexagons for one statement scale, day and source.
demo_cells <- function(scale, k) {
  g <- demo_geometry()$table
  out <- lapply(DEMO_SOURCES, function(src) {
    int <- demo_intensity(scale, k, src)
    y <- ifelse(int < 0.24, 0L, pmin(4L, 1L + as.integer(floor((int - 0.24) / 0.22))))
    x <- pmin(4L, pmax(1L, as.integer(round(g$vuln * (0.55 + 0.7 * int)))))
    data.table(constituency_id = g$constituency_id, hex = seq_len(nrow(g)), source = src, x = x, y = y)[y >= 1L]
  })
  cells <- rbindlist(out)
  if (nrow(cells) == 0L) return(cells[, `:=`(risk_colour = character(), risk_level = character(), rank = integer())])
  cells <- merge(cells, RISK_MATRIX[, .(x, y, risk_colour = colour)], by = c("x", "y"), all.x = TRUE, sort = FALSE)
  cells[, risk_level := RISK_LEVEL_BY_COLOUR[risk_colour]]
  cells[, rank := match(risk_level, DEMO_LEVELS)]
  cells[]
}

# Risk polygons for one statement and day (sf, EPSG:4326), in the live schema.
demo_polygons <- function(scn, stmt_id, day) demo_memo(sprintf("poly_%s_%d_%d", scn, stmt_id, day), {
  st <- demo_statements_table(scn)[statement_id == stmt_id]
  g  <- demo_geometry()
  empty <- function() sf::st_sf(
    poly_id = integer(), source = character(), day_index = integer(), forecast_date = as.Date(character()),
    risk_x = integer(), risk_y = integer(), impact_label = character(), likelihood_label = character(),
    risk_level = character(), risk_colour = character(), geometry = sf::st_sfc(), crs = 4326)
  if (nrow(st) == 0L) return(empty())
  offset <- match(as.integer(stmt_id), demo_statements_table(scn)$statement_id) - 8L   # 0 for the newest
  cells <- demo_cells(st$scale, offset + day - 1L)
  if (nrow(cells) == 0L) return(empty())
  groups <- unique(cells[, .(source, x, y, risk_colour, risk_level)])
  groups <- groups[order(source, x, y)]
  geoms <- lapply(seq_len(nrow(groups)), function(i) {
    members <- cells[source == groups$source[i] & x == groups$x[i] & y == groups$y[i], hex]
    sf::st_buffer(sf::st_union(g$hex_m[members]), 6000)
  })
  fdate <- as.Date(substr(st$issued_at, 1, 10)) + day - 1L
  out <- data.table(
    poly_id = seq_len(nrow(groups)) + 1000L * day, source = groups$source, day_index = as.integer(day),
    forecast_date = fdate, risk_x = groups$x, risk_y = groups$y,
    impact_label = IMPACT_LABELS[groups$x], likelihood_label = LIKELIHOOD_LABELS[groups$y],
    risk_level = groups$risk_level, risk_colour = groups$risk_colour)
  sf::st_sf(out, geometry = sf::st_transform(sf::st_sfc(do.call(c, geoms), crs = 3857), 4326))
})

# Overlap of each polygon with each target, as a share of the target's area.
# One spatial call for the lot (not one per pair).
demo_overlap <- function(polys_m, targets_m, target_area) {
  none <- data.table(poly = integer(), target = integer(), pct = numeric())
  if (length(polys_m) == 0L) return(none)
  a <- sf::st_sf(poly = seq_along(polys_m), geometry = polys_m)
  b <- sf::st_sf(target = seq_along(targets_m), geometry = targets_m)
  x <- suppressWarnings(sf::st_intersection(a, b))
  if (nrow(x) == 0L) return(none)
  x <- x[sf::st_is(x, c("POLYGON", "MULTIPOLYGON")), ]
  if (nrow(x) == 0L) return(none)
  d <- data.table(poly = x$poly, target = x$target, area = as.numeric(sf::st_area(x)))
  d[, .(pct = pmin(1, sum(area) / target_area[target[1L]])), by = .(poly, target)]
}

# Constituency overlaps for one statement and day (live schema, no geometry).
demo_const_inter <- function(scn, stmt_id, day) demo_memo(sprintf("cinter_%s_%d_%d", scn, stmt_id, day), {
  g <- demo_geometry(); polys <- demo_polygons(scn, stmt_id, day)
  none <- data.table(constituency_id = character(), constituency_name = character(), source = character(),
    risk_x = integer(), risk_y = integer(), risk_colour = character(), risk_level = character(),
    intersection_pct = numeric(), forecast_date = as.Date(character()))
  if (nrow(polys) == 0L) return(none)
  m <- demo_overlap(sf::st_transform(sf::st_geometry(polys), 3857), g$hex_m, g$hex_area)[pct >= 0.02]
  if (nrow(m) == 0L) return(none)
  p <- sf::st_drop_geometry(polys)
  data.table(
    constituency_id = g$table$constituency_id[m$target], constituency_name = g$table$constituency_name[m$target],
    source = p$source[m$poly], risk_x = p$risk_x[m$poly], risk_y = p$risk_y[m$poly],
    risk_colour = p$risk_colour[m$poly], risk_level = p$risk_level[m$poly],
    intersection_pct = m$pct, forecast_date = p$forecast_date[m$poly])
})

# Flood warning / alert area overlaps for one statement and day.
demo_ea_inter <- function(scn, stmt_id, day) demo_memo(sprintf("eainter_%s_%d_%d", scn, stmt_id, day), {
  ea <- demo_ea_areas(); polys <- demo_polygons(scn, stmt_id, day)
  none <- data.table(ea_area_code = character(), ea_area_name = character(), ea_area_type = character(),
    source = character(), risk_level = character(), risk_colour = character(), risk_x = integer(),
    risk_y = integer(), intersection_pct = numeric())
  if (nrow(polys) == 0L) return(none)
  m <- demo_overlap(sf::st_transform(sf::st_geometry(polys), 3857), ea$geom_m, ea$area_m2)[pct >= 0.25]  # the live rule for polygons
  if (nrow(m) == 0L) return(none)
  p <- sf::st_drop_geometry(polys)
  out <- data.table(
    ea_area_code = ea$table$ea_area_code[m$target], ea_area_name = ea$table$ea_area_name[m$target],
    ea_area_type = ea$table$ea_area_type[m$target], source = p$source[m$poly],
    risk_level = p$risk_level[m$poly], risk_colour = p$risk_colour[m$poly],
    risk_x = p$risk_x[m$poly], risk_y = p$risk_y[m$poly], intersection_pct = m$pct)
  out[order(match(risk_colour, c("Red", "Amber", "Yellow", "Green")), -intersection_pct)]
})

demo_stmt_row <- function(scn, stmt_id) {
  st <- demo_statements_table(scn)[statement_id == as.integer(stmt_id)]
  if (nrow(st) == 0L) NULL else st
}
demo_latest_id <- function(scn) max(demo_statements_table(scn)$statement_id)


# =============================================================================
# DERIVED PRODUCTS (the same shapes as the pipeline's tables)
# =============================================================================

# Worst rank per constituency and day (any source), from overlaps of 25% or more.
demo_rank_by_day <- function(scn, stmt_id) demo_memo(sprintf("rankday_%s_%d", scn, stmt_id), {
  g <- demo_geometry()$table
  per_day <- lapply(1:5, function(d) {
    ci <- demo_const_inter(scn, stmt_id, d)
    if (nrow(ci) == 0L) return(NULL)
    ci <- ci[intersection_pct >= 0.25]
    if (nrow(ci) == 0L) return(NULL)
    ci[, .(rank = max(match(risk_level, DEMO_LEVELS)),
           sources = paste(sort(unique(source[match(risk_level, DEMO_LEVELS) == max(match(risk_level, DEMO_LEVELS))])), collapse = ", ")),
       by = constituency_id][, day := d]
  })
  rbindlist(per_day)
})

demo_changes_for <- function(scn, stmt_id) demo_memo(sprintf("changes_%s_%d", scn, stmt_id), {
  st <- demo_statements_table(scn)
  idx <- match(as.integer(stmt_id), st$statement_id)
  empty <- data.table(constituency_id = character(), constituency_name = character(), forecast_date = character(),
    day_index = integer(), source = character(), previous_rank = integer(), new_rank = integer(),
    rank_change = integer(), change_type = character())
  if (is.na(idx) || idx == 1L) return(empty)
  prev_id <- st$statement_id[idx - 1L]
  by_cell <- function(sid, day) {
    ci <- demo_const_inter(scn, sid, day)
    if (nrow(ci) == 0L) return(NULL)
    ci <- ci[intersection_pct >= 0.25]
    if (nrow(ci) == 0L) return(NULL)
    ci[, .(rank = max(match(risk_level, DEMO_LEVELS))), by = .(constituency_id, constituency_name, source, forecast_date)]
  }
  # Only dates both statements cover: day d of this one is day d + 1 of the previous.
  rows <- lapply(1:4, function(d) {
    a <- by_cell(stmt_id, d); b <- by_cell(prev_id, d + 1L)
    keys <- unique(rbindlist(list(
      if (!is.null(a)) a[, .(constituency_id, constituency_name, source, forecast_date)],
      if (!is.null(b)) b[, .(constituency_id, constituency_name, source, forecast_date)]), fill = TRUE))
    if (nrow(keys) == 0L) return(NULL)
    keys[, day_index := d]
    keys[, new_rank := 0L]; keys[, previous_rank := 0L]
    if (!is.null(a)) keys[a, new_rank := i.rank, on = c("constituency_id", "source", "forecast_date")]
    if (!is.null(b)) keys[b, previous_rank := i.rank, on = c("constituency_id", "source", "forecast_date")]
    keys
  })
  ch <- rbindlist(rows)
  if (nrow(ch) == 0L) return(empty)
  ch <- ch[new_rank != previous_rank]
  if (nrow(ch) == 0L) return(empty)
  ch[, rank_change := new_rank - previous_rank]
  ch[, change_type := fcase(previous_rank == 0L, "new", new_rank == 0L, "cleared",
                            rank_change > 0L, "escalated", default = "de_escalated")]
  ch[, forecast_date := as.character(forecast_date)]
  ch[order(-new_rank, -rank_change, constituency_name),
     .(constituency_id, constituency_name, forecast_date, day_index, source, previous_rank, new_rank, rank_change, change_type)]
})

# One row per constituency for any statement. Only the newest statement has
# warnings and alerts in force: the pipeline keeps no history of them, so for
# an older statement they are NA, exactly as the real look-back returns them.
demo_situation_for <- function(scn, sid) demo_memo(sprintf("situation_%s_%d", scn, sid), {
  g <- demo_geometry()$table
  st  <- demo_stmt_row(scn, sid)
  is_latest <- sid == demo_latest_id(scn)
  days <- demo_rank_by_day(scn, sid)
  ch   <- demo_changes_for(scn, sid)
  esc  <- unique(ch[change_type %in% c("new", "escalated"), constituency_id])

  wide <- data.table(constituency_id = g$constituency_id)
  for (d in 1:5) {
    r <- if (is.null(days) || nrow(days) == 0L) NULL else days[day == d]
    wide[, (paste0("fgs_rank_day", d)) := 0L]
    if (!is.null(r) && nrow(r) > 0L) wide[r, (paste0("fgs_rank_day", d)) := i.rank, on = "constituency_id"]
  }
  rk <- as.matrix(wide[, paste0("fgs_rank_day", 1:5), with = FALSE])
  wide[, fgs_worst_rank := apply(rk, 1L, max)]
  first_day <- apply(rk, 1L, function(r) if (max(r) == 0L) NA_integer_ else which(r == max(r))[1L])
  fdate <- as.Date(substr(st$issued_at, 1, 10))
  wide[, fgs_worst_date := as.character(fdate + first_day - 1L)]
  # Sources behind the worst risk across ALL days, as the pipeline's job 19 does
  # (collect_set over every cell at the worst rank), not just the first day.
  cell_ranks <- rbindlist(lapply(1:5, function(d) {
    ci <- demo_const_inter(scn, sid, d)[intersection_pct >= 0.25]
    if (nrow(ci) == 0L) return(NULL)
    ci[, .(rank = max(match(risk_level, DEMO_LEVELS))), by = .(constituency_id, source)]
  }))
  src <- rep(NA_character_, nrow(wide))
  if (!is.null(cell_ranks) && nrow(cell_ranks) > 0L) {
    w <- cell_ranks[, .(worst = max(rank)), by = constituency_id]
    top <- merge(cell_ranks, w, by = "constituency_id")[rank == worst, .(s = paste(sort(unique(source)), collapse = ", ")), by = constituency_id]
    src <- top$s[match(wide$constituency_id, top$constituency_id)]
  }
  wide[, fgs_worst_sources := src]
  wide[, fgs_worst_level := fifelse(fgs_worst_rank == 0L, "None", DEMO_LEVELS[pmax(1L, fgs_worst_rank)])]
  wide[, `:=`(fgs_escalated = constituency_id %in% esc, fgs_changed = constituency_id %in% ch$constituency_id)]

  # Warnings in force: flood areas flagged Medium or above on days 1 to 3.
  geo <- demo_geometry(); ea <- demo_ea_areas()
  warn <- if (!is_latest) data.table(ea_area_code = character(), ea_area_type = character(), rank = integer()) else rbindlist(lapply(1:3, function(d) {
    ei <- demo_ea_inter(scn, sid, d)
    ei[match(risk_level, DEMO_LEVELS) >= 3L, .(ea_area_code, ea_area_type, rank = match(risk_level, DEMO_LEVELS))]
  }))
  wide[, `:=`(severe_warnings = 0L, warnings = 0L, alerts = 0L)]
  if (nrow(warn) > 0L) {
    warn <- warn[, .(ea_area_type = ea_area_type[1L], rank = max(rank)), by = ea_area_code]
    hit <- sf::st_intersects(ea$geom_m[match(warn$ea_area_code, ea$table$ea_area_code)], geo$hex_m)
    for (i in seq_len(nrow(warn))) for (h in hit[[i]]) {
      cid <- g$constituency_id[h]
      if (warn$ea_area_type[i] == "flood_warning") {
        if (warn$rank[i] >= 4L) wide[constituency_id == cid, severe_warnings := severe_warnings + 1L]
        else                    wide[constituency_id == cid, warnings := warnings + 1L]
      } else wide[constituency_id == cid, alerts := alerts + 1L]
    }
  }
  wide[, has_active_warning := severe_warnings + warnings + alerts > 0L]
  if (!is_latest) wide[, `:=`(severe_warnings = NA_integer_, warnings = NA_integer_, alerts = NA_integer_, has_active_warning = NA)]

  out <- merge(g[, .(constituency_id, constituency_name, mp_name, mp_party, mp_email, mp_phone)], wide, by = "constituency_id", sort = FALSE)
  out[, `:=`(mp_matched = !is.na(mp_name), fgs_statement_id = sid, fgs_issued_at = st$issued_at)]
  setcolorder(out, c("constituency_id", "constituency_name", "mp_matched", "mp_name", "mp_party", "mp_email", "mp_phone",
                     "fgs_statement_id", "fgs_issued_at", "fgs_worst_rank", "fgs_worst_level", "fgs_worst_date",
                     "fgs_worst_sources", paste0("fgs_rank_day", 1:5), "fgs_changed", "fgs_escalated",
                     "severe_warnings", "warnings", "alerts", "has_active_warning"))
  out[]
})

demo_situation <- function(scn) demo_situation_for(scn, demo_latest_id(scn))

demo_briefings_for <- function(scn, sid) demo_memo(sprintf("briefings_%s_%d", scn, sid), {
  s <- demo_situation_for(scn, sid)[fgs_worst_rank >= 3L & !is.na(mp_name)]
  if (nrow(s) == 0L) return(data.table(statement_id = integer(), constituency_id = character(), constituency_name = character(),
    mp_name = character(), mp_email = character(), worst_rank = integer(), worst_level = character(), worst_date = character(),
    is_escalation = logical(), has_active_warning = logical(), subject = character(), body = character(),
    status = character(), reviewed_by = character()))
  st <- demo_stmt_row(scn, sid)
  issued <- parse_utc(st$issued_at)
  s[, `:=`(
    statement_id = sid, worst_rank = fgs_worst_rank, worst_level = fgs_worst_level,
    worst_date = fgs_worst_date, is_escalation = fgs_escalated,
    subject = sprintf("Flood Guidance Statement: %s flood risk forecast for %s on %s", fgs_worst_level,
                      constituency_name, format(as.Date(fgs_worst_date), "%A %d %B")))]
  s[, body := vapply(seq_len(.N), function(i) {
    r <- s[i]
    days <- paste(sprintf("Day %d %s", 1:5, RANK_LABELS[unlist(r[, paste0("fgs_rank_day", 1:5), with = FALSE]) + 1L]), collapse = ", ")
    force <- if (isTRUE(r$has_active_warning)) sprintf("%d severe flood warning(s), %d flood warning(s) and %d flood alert(s) are currently in force in the constituency.",
                                                       r$severe_warnings, r$warnings, r$alerts)
             else if (is.na(r$has_active_warning)) "Warnings in force at the time are not held for this statement."
             else "No flood warnings or alerts are currently in force in the constituency."
    paste0("Dear ", r$mp_name, ",\n\n",
           "The Flood Guidance Statement issued at ", format(issued, "%H:%M"), " on ", format(issued, "%d %B %Y"),
           " forecasts a ", r$fgs_worst_level, " risk of ", r$fgs_worst_sources, " flooding in ", r$constituency_name,
           " on ", format(as.Date(r$fgs_worst_date), "%A %d %B"), ".",
           if (r$fgs_escalated) " The risk has risen since the previous statement." else "", "\n\n",
           force, "\n\nForecast by day: ", days, ".\n\n[Sender name and role]\n[Contact details]")
  }, character(1))]
  s[, `:=`(status = "draft", reviewed_by = NA_character_)]
  s[order(-worst_rank, constituency_name),
    .(statement_id, constituency_id, constituency_name, mp_name, mp_email, worst_rank, worst_level, worst_date,
      is_escalation, has_active_warning = fifelse(is.na(has_active_warning), FALSE, has_active_warning), subject, body, status, reviewed_by)]
})

demo_briefings <- function(scn) demo_briefings_for(scn, demo_latest_id(scn))

demo_exposure <- function(scn) demo_memo(paste0("exposure_", scn), demo_with_seed(99, {
  g <- demo_geometry()$table
  mons <- rev(format(seq(as.Date(format(Sys.Date(), "%Y-%m-01")), by = "-1 month", length.out = 12L), "%Y-%m"))
  season <- c(1.0, 0.9, 0.7, 0.35, 0.2, 0.15, 0.15, 0.25, 0.4, 0.7, 0.9, 1.0)   # Jan..Dec
  aspect <- 1.66
  prone <- exp(-(((g$lon + 2.3)^2 + ((g$lat - 54.0) * aspect)^2) / (2 * 2.2^2))) * (0.5 + 0.25 * g$vuln)
  covered <- vapply(mons, function(m) {
    if (m == mons[length(mons)]) as.integer(format(Sys.Date(), "%d"))
    else as.integer(format(seq(as.Date(paste0(m, "-01")), by = "month", length.out = 2L)[2L] - 1L, "%d"))
  }, integer(1))
  d <- CJ(i = seq_len(nrow(g)), m = seq_along(mons))
  d[, `:=`(month = mons[m], days_covered = covered[m],
           lambda = 9 * prone[i] * season[as.integer(substr(mons[m], 6, 7))] *
                    (if (scn == "quiet") 0.4 else if (scn == "severe") 1.3 else 1))]
  d[, days_medium_or_above := pmin(days_covered, rpois(.N, lambda * 0.45))]
  d[, days_flagged := pmin(days_covered, days_medium_or_above + rpois(.N, lambda * 0.9))]
  d <- d[days_flagged > 0L]
  d[, `:=`(constituency_id = g$constituency_id[i], constituency_name = g$constituency_name[i],
           source = "any", worst_rank = ifelse(days_medium_or_above > 0L, 3L, 2L))]
  d[, .(constituency_id, constituency_name, month, source, days_medium_or_above, days_flagged, worst_rank, days_covered)]
}))

demo_health <- function(problem) {
  stamp <- format(Sys.time() - 180, "%Y-%m-%dT%H:%M:%S+00:00", tz = "UTC")
  checks <- data.table(
    check_name = c("fgs_latest_statement", "fgs_intersections_pending", "situation_table_age", "warnings_current_age",
                   "warnings_unlinked", "mp_details_age", "mp_count", "constituency_count", "mps_unmatched"),
    status = c("ok", "ok", "ok", "ok", "ok", "ok", "ok", "ok", "info"),
    value  = c("1.2 h", "0", "0.1 h", "0.1 h", "0", "2.0 days", "148", "150", "2"),
    detail = c("newest statement issued 09:30", "all statements intersected", "last built a few minutes ago",
               "last ingest a few minutes ago", "", "MP details refreshed this week", "", "", "2 vacant seats"))
  if (problem) {
    checks[check_name == "mp_details_age", `:=`(status = "fail", value = "21 days", detail = "MP details 21 days old (fail at 14)")]
    checks[check_name == "warnings_unlinked", `:=`(status = "warn", value = "3", detail = "3 warnings match no constituency")]
  }
  worst <- if (any(checks$status == "fail")) "fail" else if (any(checks$status == "warn")) "warn" else "ok"
  failing <- checks$check_name[checks$status == "fail"]
  rbind(checks, data.table(check_name = "overall", status = worst, value = "",
                           detail = paste0("worst of the checks above", if (length(failing)) paste0("; failing: ", paste(failing, collapse = ", ")) else "")))[
    , checked_at := stamp][]
}


# =============================================================================
# THE fetch_* FUNCTIONS (replacing the warehouse versions, same signatures)
# =============================================================================

fetch_recent_statements <- function(n = 20L) {
  st <- demo_statements_table(demo_current()$scenario)
  out <- head(st[order(-issued_at), .(statement_id, issued_at, headline, source_coastal, source_surface,
                                      source_ground, source_river, england_forecast, pdf_url)], n)
  # The warehouse returns the time as text ("2026-10-15T09:30:00"), which R's
  # as.POSIXct() reads as midnight. Keep it as text so the demo exercises the
  # same path (as_utc() in helpers.R) as the live app.
  out[]
}

fetch_latest_statement <- function() {
  scn <- demo_current()$scenario
  st <- demo_stmt_row(scn, demo_latest_id(scn))
  sit <- demo_situation(scn)
  out <- list(statement_id = st$statement_id, issued_at = st$issued_at, headline = st$headline)
  for (d in 1:5) out[[paste0("flood_risk_trend_day", d)]] <- RANK_LABELS[max(sit[[paste0("fgs_rank_day", d)]]) + 1L]
  out
}

fetch_risk_polygons <- function(statement_id, day_index) {
  demo_polygons(demo_current()$scenario, as.integer(statement_id), as.integer(day_index))
}

fetch_ea_areas_for_statement <- function(statement_id, day_index) {
  demo_ea_inter(demo_current()$scenario, as.integer(statement_id), as.integer(day_index))[, .(
    ea_area_code, ea_area_name, ea_area_type, source, risk_level, risk_colour, risk_x, risk_y, intersection_pct)]
}

fetch_ea_geometry <- function(statement_id, day_index) {
  ei <- fetch_ea_areas_for_statement(statement_id, day_index)
  ea <- demo_ea_areas()
  if (nrow(ei) == 0L) {
    return(sf::st_sf(ea_area_code = character(), ea_area_name = character(), ea_area_type = character(),
                     risk_colour = character(), risk_level = character(), intersection_pct = numeric(),
                     source = character(), geometry = sf::st_sfc(), crs = 4326))
  }
  sf::st_sf(ei[, .(ea_area_code, ea_area_name, ea_area_type, risk_colour, risk_level, intersection_pct, source)],
            geometry = ea$geom4326[match(ei$ea_area_code, ea$table$ea_area_code)])
}

fetch_constituencies_for_statement <- function(statement_id, day_index) {
  demo_const_inter(demo_current()$scenario, as.integer(statement_id), as.integer(day_index))[, .(
    constituency_id, constituency_name, source, risk_x, risk_y, risk_colour, risk_level, intersection_pct)]
}

fetch_constituency_geometry <- function(statement_id, day_index) {
  ci <- unique(fetch_constituencies_for_statement(statement_id, day_index))
  g <- demo_geometry()
  if (nrow(ci) == 0L) {
    return(sf::st_sf(constituency_id = character(), constituency_name = character(), source = character(),
                     risk_x = integer(), risk_y = integer(), risk_colour = character(), risk_level = character(),
                     intersection_pct = numeric(), geometry = sf::st_sfc(), crs = 4326))
  }
  sf::st_sf(ci, geometry = g$hex4326[match(ci$constituency_id, g$table$constituency_id)])
}

fetch_summary_counts <- function(statement_id, day_index) {
  scn <- demo_current()$scenario; sid <- as.integer(statement_id); d <- as.integer(day_index)
  list(polygon_count = nrow(demo_polygons(scn, sid, d)),
       ea_area_count = uniqueN(demo_ea_inter(scn, sid, d)$ea_area_code),
       constituency_count = uniqueN(demo_const_inter(scn, sid, d)$constituency_id))
}

fetch_ea_constituency_mp <- function() demo_memo("ea_constituency_mp", {
  g <- demo_geometry(); ea <- demo_ea_areas()
  m <- demo_overlap(ea$geom_m, g$hex_m, g$hex_area)
  data.table(ea_area_code = ea$table$ea_area_code[m$poly], constituency_id = g$table$constituency_id[m$target],
             constituency_name = g$table$constituency_name[m$target],
             constituency_overlap_pct = m$pct, mp_name = g$table$mp_name[m$target],
             mp_party = g$table$mp_party[m$target], mp_email = g$table$mp_email[m$target],
             mp_phone = g$table$mp_phone[m$target])
})

fetch_pipeline_health <- function() demo_health(demo_current()$problem)

fetch_changes <- function(statement_id) demo_changes_for(demo_current()$scenario, as.integer(statement_id))

fetch_changes_log <- function(statement_id) {
  st <- demo_statements_table(demo_current()$scenario)
  idx <- match(as.integer(statement_id), st$statement_id)
  if (is.na(idx) || idx == 1L) return(data.table())      # the oldest has nothing before it, so is never compared
  ch <- demo_changes_for(demo_current()$scenario, as.integer(statement_id))
  scn <- demo_current()$scenario
  now_rk  <- demo_situation_for(scn, as.integer(statement_id))$fgs_worst_rank
  prev_rk <- demo_situation_for(scn, st$statement_id[idx - 1L])$fgs_worst_rank
  data.table(statement_id = as.integer(statement_id), previous_statement_id = st$statement_id[idx - 1L],
             previous_issued_at = st$issued_at[idx - 1L], days_compared_from = NA_character_,
             days_compared_to = NA_character_, n_changes = nrow(ch), status = "compared",
             flagged_previous = sum(prev_rk >= 1L, na.rm = TRUE), flagged_new = sum(now_rk >= 1L, na.rm = TRUE),
             medium_plus_previous = sum(prev_rk >= 3L, na.rm = TRUE), medium_plus_new = sum(now_rk >= 3L, na.rm = TRUE))
}

fetch_potential_areas <- function(statement_id, min_rank = 0L, sources = NULL) {
  scn <- demo_current()$scenario; sid <- as.integer(statement_id)
  ea <- rbindlist(lapply(1:5, function(d) {
    x <- demo_ea_inter(scn, sid, d)
    if (length(sources) > 0L) x <- x[tolower(source) %in% tolower(sources)]
    x[match(risk_level, RANK_LABELS) - 1L >= min_rank, .(ea_area_code, ea_area_type)]
  }))
  g <- demo_geometry()
  lk <- fetch_ea_constituency_mp()[constituency_overlap_pct >= TARGET_AREA_MIN_PCT, .(ea_area_code, constituency_name)]
  m <- merge(unique(ea), lk, by = "ea_area_code")
  m[, constituency_id := g$table$constituency_id[match(constituency_name, g$table$constituency_name)]]
  m[, .(warning_areas = uniqueN(ea_area_code[ea_area_type == "flood_warning"]),
        alert_areas   = uniqueN(ea_area_code[ea_area_type == "flood_alert"])), by = constituency_id]
}

# Constituency overlaps for all five days of one statement, with the impact and likelihood, for
# the source filter and the impact and likelihood columns. The 25% floor matches the demo's own
# situation table, so the two agree.
fetch_constituency_intersections <- function(statement_id) {
  scn <- demo_current()$scenario; sid <- as.integer(statement_id)
  if (is.null(demo_stmt_row(scn, sid))) return(NULL)
  x <- rbindlist(lapply(1:5, function(d) {
    y <- demo_const_inter(scn, sid, d)[intersection_pct >= 0.25]
    if (nrow(y) == 0L) return(NULL)
    y <- copy(y); y[, day_index := d]; y
  }))
  if (is.null(x) || nrow(x) == 0L) return(data.table())
  x[, `:=`(impact_label = IMPACT_LABELS[risk_x], likelihood_label = LIKELIHOOD_LABELS[risk_y])]
  x[]
}

fetch_situation <- function() copy(demo_situation(demo_current()$scenario))

fetch_briefing_drafts <- function(statement_id = NULL) {
  scn <- demo_current()$scenario
  copy(demo_briefings_for(scn, if (is.null(statement_id)) demo_latest_id(scn) else as.integer(statement_id)))
}

# POC-ONLY: statement look-back (see helpers.R aggregate_situation for the real path).
fetch_situation_for_statement <- function(statement_id, issued_at = NA) {
  scn <- demo_current()$scenario
  if (is.null(demo_stmt_row(scn, statement_id))) return(NULL)
  copy(demo_situation_for(scn, as.integer(statement_id)))
}

fetch_exposure <- function(n_months = 12L) copy(demo_exposure(demo_current()$scenario))

# A small invented gauge network: 4 rivers in 3 areas, with level, flow and rainfall measures.
demo_rt_register <- function() demo_memo("rt_register", {
  st <- data.table(
    station_label = c("Aldwyn Bridge", "Brenn Weir", "Cardell Mill", "Dunmere Ford", "Rainfall station", "Rainfall station"),
    station_reference = c("DM1001", "DM1002", "DM2001", "DM3001", "DM4001", "DM4002"),
    river_name = c("River Aldwyn", "River Brenn", "River Cardell", "River Dunmere", NA, NA),
    ea_area_name = c("Demo North", "Demo North", "Demo Central", "Demo South", "Demo North", "Demo South"),
    town = c("Aldwyn", "Brenn", "Cardell", "Dunmere", "Hexton", "Ashby"))
  m <- rbindlist(lapply(seq_len(nrow(st)), function(i) {
    if (is.na(st$river_name[i])) data.table(st[i], parameter = "rainfall", qualifier = "Tipping Bucket Raingauge", unit_name = "mm", period = 900L)
    else rbind(data.table(st[i], parameter = "level", qualifier = "Stage", unit_name = "mASD", period = 900L),
               if (i %% 2L == 1L) data.table(st[i], parameter = "flow", qualifier = "Flow", unit_name = "m3/s", period = 900L))
  }))
  m[, `:=`(station_uri = paste0("http://demo.invalid/id/stations/", station_reference),
           measure_uri = paste0("http://demo.invalid/id/measures/", station_reference, "-", parameter, "-", period))]
  m[]
})

fetch_rt_register <- function() copy(demo_rt_register())

# The archive ends at midnight last night; everything after it is the live tail.
fetch_rt_boundary <- function() rt_boundary(Sys.Date() - 1L)

fetch_rt_readings <- function(measure_uris, days) {
  reg <- demo_rt_register()[measure_uri %in% measure_uris]
  if (nrow(reg) == 0L) return(data.table(measure_uri = character(), date_time = as.POSIXct(character(), tz = "UTC"), value = numeric()))
  now <- as.POSIXct(trunc(Sys.time(), "mins"), tz = "UTC")
  ts <- seq(now - as.integer(days) * 86400, now, by = 900)
  rbindlist(lapply(seq_len(nrow(reg)), function(i) {
    h <- as.numeric(difftime(ts, now, units = "hours"))
    set.seed(sum(utf8ToInt(reg$measure_uri[i])))
    storm <- exp(-((h + 6) / 9)^2)
    v <- switch(reg$parameter[i],
      level    = 0.6 + 0.25 * sin(h / 11) + 1.4 * storm + rnorm(length(h), 0, 0.01),
      flow     = 3 + 1.5 * sin(h / 13) + 22 * storm + rnorm(length(h), 0, 0.15),
      rainfall = pmax(0, 2.5 * exp(-((h + 9) / 3)^2) + rnorm(length(h), 0, 0.03)))
    data.table(measure_uri = reg$measure_uri[i], date_time = ts, value = round(v, 3))
  }))
}

# Two years of invented warnings and alerts, heavier where the demo's storms fall.
# Invented episodes for one constituency, so the Detailed table and the graph work in the demo.
fetch_warning_episodes <- function(constituency_id) {
  h <- { cid_ <- constituency_id; fetch_warning_history()[constituency_id == cid_] }
  h <- h[, .(ea_area_code, ea_area_name, ea_area_type, issued_date, n_issued, n_severe)]
  if (nrow(h) == 0L) return(data.table())
  set.seed(sum(utf8ToInt(as.character(constituency_id))))
  e <- h[rep(seq_len(.N), n_issued)]
  e[, `:=`(episode_id = sprintf("%s_%d", ea_area_code, seq_len(.N)),
           start_time = as.POSIXct(paste(issued_date, "00:00:00"), tz = "UTC") + sample.int(86000L, .N, replace = TRUE),
           duration_minutes = round(rexp(.N, 1 / ifelse(ea_area_type == "flood_alert", 2400, 900))) + 30)]
  e[, end_time := start_time + duration_minutes * 60]
  e[, `:=`(had_severe = ea_area_type == "flood_warning" & runif(.N) < 0.12, stale_open = FALSE)]
  e[, peak_level := fifelse(had_severe, "severe_flood_warning", ea_area_type)]
  e[end_time > Sys.time(), `:=`(end_time = as.POSIXct(NA, tz = "UTC"), duration_minutes = NA_real_)]
  e[, .(episode_id, ea_area_code, ea_area_name, ea_area_type, start_time, end_time, duration_minutes, peak_level, had_severe, stale_open)]
}

fetch_warning_history <- function() demo_memo("warning_history", demo_with_seed(515, {
  lk <- fetch_ea_constituency_mp()[constituency_overlap_pct >= TARGET_AREA_MIN_PCT]
  ea <- demo_ea_areas()$table
  lk <- merge(lk[, .(ea_area_code, constituency_id)], ea, by = "ea_area_code")
  rate <- ifelse(lk$ea_area_type == "flood_warning", 3, 7)
  lk[, n_days := pmin(60L, rpois(.N, rate * runif(.N, 0.3, 2)))]
  d <- lk[n_days > 0L, .(issued_date = as.Date(Sys.Date() - sample.int(730L, n_days, replace = TRUE))), by = .(constituency_id, ea_area_code, ea_area_name, ea_area_type)]
  d <- d[, .(n_issued = .N), by = .(constituency_id, ea_area_code, ea_area_name, ea_area_type, issued_date)]
  d[, n_severe := fifelse(ea_area_type == "flood_warning", as.integer(rbinom(.N, n_issued, 0.12)), 0L)]
  d[]
}))

fetch_constituency_shape <- function(constituency_id) {
  g <- demo_geometry(); i <- match(constituency_id, g$table$constituency_id)
  if (is.na(i)) return(NULL)
  sf::st_sf(constituency_id = g$table$constituency_id[i], constituency_name = g$table$constituency_name[i],
            geometry = g$hex4326[i])
}

fetch_constituency_risk_geometry <- function() {
  s <- demo_situation(demo_current()$scenario)[fgs_worst_rank >= 1L]
  g <- demo_geometry()
  if (nrow(s) == 0L) {
    return(sf::st_sf(constituency_id = character(), constituency_name = character(), fgs_worst_rank = integer(),
                     fgs_worst_level = character(), fgs_worst_date = character(), fgs_worst_sources = character(),
                     fgs_escalated = logical(), severe_warnings = integer(), warnings = integer(), alerts = integer(),
                     mp_name = character(), geometry = sf::st_sfc(), crs = 4326))
  }
  sf::st_sf(s[, .(constituency_id, constituency_name, fgs_worst_rank, fgs_worst_level, fgs_worst_date,
                  fgs_worst_sources, fgs_escalated, severe_warnings, warnings, alerts, mp_name)],
            geometry = g$hex4326[match(s$constituency_id, g$table$constituency_id)])
}

fetch_data_signature <- function() {
  cur <- demo_current()
  paste("demo", cur$scenario, cur$problem, as.character(Sys.Date()), sep = "|")
}

# Warm the default scenario at start-up, so the first page load is quick.
invisible(demo_situation("developing"))
