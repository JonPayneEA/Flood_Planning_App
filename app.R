# =============================================================================
# app.R
# Entry point for the FGS Flood Guidance Shiny dashboard.
#
# Sources global.R (library loads, env vars, constants), app_ui.R (layout), and
# app_server.R (reactive logic and outputs). Then hands the ui/server pair to
# shinyApp() to start the application.
#
# Posit Connect detects this file automatically as the app entry point. To
# run locally, open the project in RStudio (or VS Code with the R extension)
# and either click the Run App button or call shiny::runApp() at the console.
# =============================================================================

# WHY app_ui.R AND app_server.R, NOT ui.R AND server.R: when a folder holds
# ui.R and server.R, shiny::runApp() uses those two files and ignores app.R, so
# helpers.R, theme.R and bookmarking would never load. With these names app.R
# is the only entry point, on a laptop and on Posit Connect alike.
#
# local = TRUE prevents the sourced files from polluting the global
# environment beyond their explicit definitions; the symbols they create are
# still visible to shinyApp() because R's lexical scoping picks them up from
# the calling environment.

source("global.R",     local = TRUE)
source("helpers.R",    local = TRUE)
source("theme.R",      local = TRUE)
source("design.R",     local = TRUE)
source("guide.R",      local = TRUE)
source("app_ui.R",     local = TRUE)
source("app_server.R", local = TRUE)

# Bookmarking in "url" mode keeps the tab, statement, day and filters in the
# address bar, so a view can be shared by copying the link. ui is a function of
# the request (see app_ui.R), which bookmarking requires.
shinyApp(ui = ui, server = server, enableBookmarking = "url")
