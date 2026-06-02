# =============================================================================
# app.R
# Entry point for the FGS Flood Guidance Shiny dashboard.
#
# Sources global.R (library loads, env vars, constants), ui.R (layout), and
# server.R (reactive logic and outputs). Then hands the ui/server pair to
# shinyApp() to start the application.
#
# Posit Connect detects this file automatically as the app entry point. To
# run locally, open the project in RStudio (or VS Code with the R extension)
# and either click the Run App button or call shiny::runApp() at the console.
# =============================================================================

# local = TRUE prevents the sourced files from polluting the global
# environment beyond their explicit definitions; the symbols they create are
# still visible to shinyApp() because R's lexical scoping picks them up from
# the calling environment.

source("global.R", local = TRUE)
source("ui.R",     local = TRUE)
source("server.R", local = TRUE)

shinyApp(ui = ui, server = server)
