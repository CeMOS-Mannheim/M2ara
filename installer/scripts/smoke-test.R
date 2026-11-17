#!/usr/bin/env Rscript
#
# Smoke test for the assembled installer payload: loads the app exactly like the
# launcher does (bundled library only) and renders the manual, which is the part
# of the start-up that touches the most packages.
#
# Usage: Rscript --vanilla smoke-test.R <payload-dir>

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop("usage: smoke-test.R <payload-dir>", call. = FALSE)
}

payload_dir <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
app_dir <- file.path(payload_dir, "app")
lib_dir <- file.path(payload_dir, "R", "library")

if (!dir.exists(app_dir)) stop("app directory missing: ", app_dir, call. = FALSE)
if (!dir.exists(lib_dir)) stop("R library missing: ", lib_dir, call. = FALSE)

# same setup as installer/run.R
.libPaths(lib_dir)

setwd(app_dir)

cat("R:", R.version.string, "\n")
cat("Library:", lib_dir, "\n")

# the check the app itself performs in app.R
source(file.path(app_dir, "functions", "checkInstalledPackages.R"))
checkInstalledPackages(req_file = "req.txt")

# the manual is knitted at every start-up (see app.R)
cat("Rendering manual.Rmd ...\n")
knitr::knit("manual.Rmd", quiet = TRUE)
if (!file.exists("manual.md")) {
  stop("manual.md was not rendered", call. = FALSE)
}

ui_file <- file.path(app_dir, "components", "ui.R")
if (!file.exists(ui_file)) stop("the app sources are incomplete", call. = FALSE)

# Load the app the same way shiny::runApp() does. This sources app.R, components/
# and functions/ and therefore catches syntax errors and missing functions that
# a plain package check would not notice.
cat("Loading the Shiny application ...\n")
app <- shiny::shinyAppDir(app_dir)
if (!inherits(app, "shiny.appobj")) {
  stop("the Shiny application could not be loaded", call. = FALSE)
}

cat("Smoke test passed.\n")
