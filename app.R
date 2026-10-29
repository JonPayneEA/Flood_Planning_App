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

# Bookmarking in "server" mode saves the tab, statement, day and filters when
# someone presses Copy link, and the link carries only a short id
# (?_state_id_=...). The URL mode put every value in the address, and the Azure
# Application Gateway in front of Connect refuses such addresses with a 403
# (the quotes and the length). ui is a function of the request (see app_ui.R),
# which bookmarking requires.
# Set the store explicitly as well as through shinyApp(): the deployed app
# reported "url" even with the argument below, so the option is stated here,
# before the app object is built, and again when each session starts.
shiny::enableBookmarking("server")
shiny::shinyOptions(bookmarkStore = "server")

shinyApp(ui = ui, server = server, enableBookmarking = "server")
