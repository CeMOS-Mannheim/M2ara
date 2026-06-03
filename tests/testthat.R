# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

library(testthat)

# Ensure NOT_CRAN is set so skip_on_cran() does not skip locally
Sys.setenv(NOT_CRAN = "true")

# make sure we are in app folder
find_req_txt <- function(start_dir = getwd()) {
  d <- normalizePath(start_dir, winslash = "/", mustWork = FALSE)
  while (d != dirname(d)) {
    if (file.exists(file.path(d, "req.txt"))) {
      return(file.path(d, "req.txt"))
    }
    d <- dirname(d)
  }
  stop("Could not find req.txt from ", start_dir)
}

reqpath <- dirname(find_req_txt())

setwd(reqpath)

cat("loading MALDIcellassay\n")
library(MALDIcellassay)
source("functions/checkInstalledPackages.R")

checkInstalledPackages()

source("functions/loadAllFunctions.R")
loadAllFunctions()

test_dir("tests/testthat", reporter = "progress")
