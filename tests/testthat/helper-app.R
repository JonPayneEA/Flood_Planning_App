# Shared setup. Tests run with the working directory at tests/testthat.

app_root <- normalizePath(file.path("..", ".."))

suppressMessages({
  library(shiny); library(data.table); library(htmltools)
})

# Load global.R's constants without its library() calls or .env loading, and
# with dummy credentials. Evaluated into the global environment because
# data.R and app_server.R look these up by name.
load_app_constants <- function(extra_env = list()) {
  old <- Sys.getenv(c("DATABRICKS_HOST", "DATABRICKS_TOKEN", "DATABRICKS_WAREHOUSE_ID",
                      "FGS_CATALOG", "FGS_SCHEMA", names(extra_env)), names = TRUE, unset = NA)
  do.call(Sys.setenv, c(list(DATABRICKS_HOST = "h", DATABRICKS_TOKEN = "t",
                             DATABRICKS_WAREHOUSE_ID = "w", FGS_CATALOG = "cat", FGS_SCHEMA = "sch"),
                        extra_env))
  on.exit({
    for (n in names(old)) if (is.na(old[[n]])) Sys.unsetenv(n) else do.call(Sys.setenv, setNames(list(old[[n]]), n))
  })
  g <- readLines(file.path(app_root, "global.R"), warn = FALSE)
  g <- g[!grepl("^\\s*library\\(", g) & !grepl("load_dot_env", g)]
  env <- new.env(parent = globalenv())
  eval(parse(text = g), envir = env)
  env
}

# data.R in its own environment with a stubbed db_query that records the SQL.
# The cache is off unless ttl is given, so tests do not see each other's rows.
load_data_layer <- function(source_value = "risk_areas", ttl = 0, constants = load_app_constants()) {
  env <- new.env(parent = globalenv())
  for (n in ls(constants, all.names = TRUE)) assign(n, get(n, constants), envir = env)
  assign("GEOMETRY_SOURCE", source_value, envir = env)
  assign("CACHE_TTL_SECONDS", ttl, envir = env)
  env$captured <- character()
  source(file.path(app_root, "data.R"), local = env)
  env$db_query <- function(sql) { env$captured <- c(env$captured, sql); data.table() }
  env
}

# helpers.R reads RANK_HEX and RANK_LABELS from the global environment.
.consts <- load_app_constants()
assign("RANK_HEX",    .consts$RANK_HEX,    envir = globalenv())
assign("RANK_LABELS", .consts$RANK_LABELS, envir = globalenv())
source(file.path(app_root, "helpers.R"), local = FALSE)
