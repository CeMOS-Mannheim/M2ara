(function(req_file, install = T, update = F, silent = F) {

  # Read text file containing required packages
  if(!file.exists("req.txt")) {
    warning("req.txt was not found. No packages installed or checked.")
    return()
  }

  req <- scan(req_file, character(), quiet = T)

  # Update packages
  if (update) {
    update.packages(repos = "https://cloud.r-project.org", ask = F)
  }

  # Install missing packages
  if (length(req) > 0 & install) {
    missing_packages <- req[!(req %in% installed.packages()[,"Package"])]
    if (length(missing_packages) > 0) {
      install.packages(
        missing_packages,
        repos = "https://cloud.r-project.org",
        dependencies = T,
        clean = T
      )
    }
  }



  # Load packages
  if (silent) {
    suppressPackageStartupMessages(invisible(lapply(req, library, character.only = T)))
  } else {
    lapply(req, library, character.only = T)
  }


})("req.txt", silent = F)


# check if all required packages are installed

source("functions/checkInstalledPackages.R")
checkInstalledPackages()

knit("manual.Rmd", quiet = TRUE)

source("components/ui.R")
source("components/server.R")

# Run the application
shinyApp(ui = ui, server = server)
