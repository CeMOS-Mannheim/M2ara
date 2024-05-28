# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files



library(testthat)

# make sure we are in app folder
reqpath <- dirname(common::file.find(pattern = "req.txt"))

if(!length(reqpath) == 1) {
  stop("could not find req.txt\n")
}

setwd(reqpath)

cat("loading MALDIcellassay\n")
library(MALDIcellassay)
source("functions/checkInstalledPackages.R")

checkInstalledPackages()

source("functions/loadAllFunctions.R")
loadAllFunctions()

test_dir("tests/testthat", reporter = "progress")
