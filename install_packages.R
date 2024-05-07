setwd("/app")
cat("Installing packages...\n")
Sys.sleep(1)

  # Read text file containing required packages
if(!file.exists("req.txt")) {
  warning("req.txt was not found. No packages installed or checked.")
  return()
}

req <- scan("req.txt", character(), quiet = T)
cat(length(req), "\n")


# Install missing packages
if (length(req)>0) {
  missing_packages <- req[!(req %in% installed.packages()[,"Package"])]
  cat(length(missing_packages) , "\n")
  if (length(missing_packages) > 0) {
    install.packages(
      missing_packages,
      repos = "https://cloud.r-project.org",
      dependencies = T,
      clean = T
    )
  }
}

 lapply(req, library, character.only = T)

