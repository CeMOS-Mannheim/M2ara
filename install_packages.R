setwd("/app")
cat("Installing packages...\n")
Sys.sleep(1)

source("functions/checkInstalledPackages.R")
checkInstalledPackages(req_file = "req.txt")
