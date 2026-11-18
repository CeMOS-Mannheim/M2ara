#!/usr/bin/env Rscript
#
# Smoke test for the assembled installer payload: loads the M2ara package from
# the bundled library only (exactly like the launcher), builds the UI and renders
# the manual. That covers the start-up paths that touch the most packages
# (shiny, htmltools, markdown, DT, plotly, ...).
#
# Usage: Rscript --vanilla smoke-test.R <payload-dir>

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop("usage: smoke-test.R <payload-dir>", call. = FALSE)
}

payload_dir <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
lib_dir <- file.path(payload_dir, "R", "library")

if (!dir.exists(lib_dir)) stop("R library missing: ", lib_dir, call. = FALSE)

# same setup as installer/run.R
.libPaths(c(lib_dir, .libPaths()))
setwd(payload_dir)

cat("R:", R.version.string, "\n")
cat("Library:", lib_dir, "\n")

cat("Loading the M2ara package ...\n")
suppressPackageStartupMessages(library(M2ara))
cat("M2ara", as.character(utils::packageVersion("M2ara")), "\n")

cat("Building the UI ...\n")
defaults <- M2ara:::defaultsSettingsHandler()
html <- as.character(M2ara:::appUI(defaults))
if (!nzchar(html)) stop("the UI is empty", call. = FALSE)
if (!grepl("showPeaks", html, fixed = TRUE)) {
  stop("the UI does not contain the expected controls", call. = FALSE)
}

cat("Rendering the manual ...\n")
manual <- system.file("manual.md", package = "M2ara")
if (!nzchar(manual) || !file.exists(manual)) {
  stop("manual.md is missing in the installed package", call. = FALSE)
}
rendered <- markdown::markdownToHTML(manual, fragment.only = TRUE)
if (!nzchar(rendered)) stop("the manual could not be rendered", call. = FALSE)

cat("Building the application object ...\n")
app <- M2ara::app()
if (!inherits(app, "shiny.appobj")) {
  stop("the Shiny application could not be created", call. = FALSE)
}

cat("Smoke test passed.\n")
