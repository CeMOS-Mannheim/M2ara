checkInstalledPackages <- function(req_file = "req.txt") {

  # Read text file containing required packages
  if(!file.exists(req_file)) {
    warning(req_file, "was not found. No packages installed or checked.")
    return()
  }

  req <- scan(req_file, character(), quiet = TRUE)

  # Install missing packages
  if (length(req) > 0) {
    missing_packages <- req[!(req %in% installed.packages()[,"Package"])]
    if (length(missing_packages) > 0) {
      cat("Number of packages to install: ", length(missing_packages), "\n")
      install.packages(
        missing_packages,
        repos = "https://cloud.r-project.org",
        dependencies = TRUE,
        clean = TRUE
      )
    }
  }

  # Load packages
  suppressPackageStartupMessages(invisible(lapply(req, library, character.only = TRUE)))

  # options
  options(dplyr.summarise.inform = FALSE)

  # special case for packages not on CRAN
  if (!require("MALDIcellassay", character.only = TRUE)) {
    pak::pkg_install("CeMOS-Mannheim/MALDIcellassay")
    library(MALDIcellassay, character.only = TRUE)
  }
}
