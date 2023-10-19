#### tidy up ####
# clear work space
rm(list = ls())
gc()

# check if all required packages are installed
# install missing packages
source("functions/checkInstalledPackages.R")
checkInstalledPackages()

knit("manual.Rmd", quiet = TRUE)

source("components/ui.R")
source("components/server.R")

# Run the application
shinyApp(ui = ui, server = server)
