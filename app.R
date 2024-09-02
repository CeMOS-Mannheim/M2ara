# check if all required packages are installed
source("functions/checkInstalledPackages.R")
checkInstalledPackages(req_file = "req.txt")

knit("manual.Rmd", quiet = TRUE)

source("components/ui.R")
source("components/server.R")

# Run the application
shinyApp(ui = ui, server = server)
