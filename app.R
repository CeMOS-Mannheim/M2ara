# check if all required packages are installed
source("functions/checkInstalledPackages.R")
checkInstalledPackages(req_file = "req.txt")

if (!getOption("shiny.testmode", FALSE)) {
  knit("manual.Rmd", quiet = TRUE)
}

# load the settings once and hand them to UI and server explicitly
# (previously the UI created a global `defaults` that the server read implicitly)
source("functions/defaultSettingsHandler.R")
defaults <- defaultsSettingsHandler(userSavedSettings = "settings.csv")

source("components/ui.R")
source("components/server.R")

# Run the application
shinyApp(
  ui = appUI(defaults),
  server = function(input, output, session) {
    server(input, output, session, defaults = defaults)
  }
)
