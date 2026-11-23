#!/usr/bin/env Rscript
# Screenshots for the manual and the README ------------------------------------
#
# Recreates the pictures in inst/figures that show the app. Run it after a UI
# change and check the result (see "What to check" below).
#
# Requirements
#   * the package is installed or loaded with pkgload (this script does that)
#   * shinytest2 and magick are installed
#   * the mzML test data of the package: either extracted in ./mzML or as
#     testdata_mzML.zip next to M2ARA_TESTDATA_DIR (see
#     tests/testthat/helper-setup.R, obtain_mzml_data())
#
# Usage
#   Rscript tools/make-manual-screenshots.R
#
# What it does
#   1. copies the mzML test data to a neutral folder (so no temp path appears in
#      the dataset card), starts the app with tests/testthat/settings_mzML_data.csv
#   2. processes the data and selects the feature with the best CRS (a curve worth
#      showing in the manual)
#   3. takes the screenshots of the tabs and cuts them to the figures
#
# What to check afterwards (the reason for the crops)
#   * clustering_curves.png: the numbers under the slider are complete
#   * clustering_metrics.png: no controls above, the metric titles are complete
#   * pca_scores.png / pca_loadings.png: axis titles complete
#   * interface.png / gui_example.png: "Binning [ppm]" on one line (wide window)
#   * selectFolder.png: only the dialog
#   * qc.png: plate map visible (needs data with spot ids)

Sys.setenv(NOT_CRAN = "true")

suppressPackageStartupMessages({
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("pkgload is needed to run the app from the source tree")
  }
  pkgload::load_all(".", quiet = TRUE)
  library(shinytest2)
  library(magick)
})

root <- normalizePath(".")
figure_dir <- file.path(root, "inst", "figures")
out_dir <- file.path(tempdir(), "manual-screenshots")
unlink(out_dir, recursive = TRUE)
dir.create(out_dir, recursive = TRUE)

# a fresh cache, so the run really processes the data
cache_dir <- file.path(tempdir(), "manual-screenshots-cache")
unlink(cache_dir, recursive = TRUE)
dir.create(cache_dir, recursive = TRUE)
Sys.setenv(M2ARA_CACHE_DIR = cache_dir)

# --- test data ----------------------------------------------------------------
obtain <- function() {
  if (dir.exists("mzML")) {
    return(normalizePath("mzML", winslash = "/"))
  }
  external <- Sys.getenv("M2ARA_TESTDATA_DIR", unset = "")
  zip <- if (nzchar(external)) file.path(external, "testdata_mzML.zip") else "testdata_mzML.zip"
  if (!file.exists(zip)) {
    stop("mzML test data not found; see tests/testthat/helper-setup.R")
  }
  utils::unzip(zip, exdir = ".")
  normalizePath("mzML", winslash = "/")
}

data_dir <- obtain()

# neutral folder, so the card in the screenshot does not show a temp path
example_dir <- file.path(tempdir(), "M2ara_example")
unlink(example_dir, recursive = TRUE)
dir.create(example_dir, recursive = TRUE)
file.copy(data_dir, example_dir, recursive = TRUE)
data_dir <- file.path(example_dir, "mzML")

# --- helpers ------------------------------------------------------------------
crop_write <- function(img, name, geometry) {
  tmp <- file.path(out_dir, paste0("tmp-", name))
  image_write(image_crop(img, geometry), tmp, format = "png")
  target <- file.path(figure_dir, name)
  for (i in 1:5) {
    if (isTRUE(tryCatch(file.copy(tmp, target, overwrite = TRUE), error = function(e) FALSE))) {
      message("written ", name)
      return(invisible(TRUE))
    }
    Sys.sleep(0.5)
  }
  warning("could not write ", name)
}

wait_for_results <- function(app, timeout = 900) {
  cell <- function() {
    app$get_js(paste0("(function(){var c=document.querySelector('#mzTable tbody tr td:nth-child(2)');",
                      " return c ? c.innerText.trim() : '';})()"))
  }
  for (i in seq_len(timeout)) {
    value <- cell()
    if (is.character(value) && grepl("^[0-9]", value)) {
      break
    }
    Sys.sleep(1)
  }
  Sys.sleep(4)
}

start_app <- function(width = 2200, height = 1250) {
  app_dir <- file.path(tempdir(), paste0("manual-app-", width))
  unlink(app_dir, recursive = TRUE)
  dir.create(app_dir, recursive = TRUE)
  settings <- read.csv(file.path(root, "tests", "testthat", "settings_mzML_data.csv"),
                       stringsAsFactors = FALSE)
  settings$dir <- data_dir
  settings$simpleLoadings <- FALSE
  utils::write.csv(settings, file.path(app_dir, "settings.csv"), row.names = FALSE)
  writeLines("M2ara::runApp()", file.path(app_dir, "app.R"))
  AppDriver$new(app_dir = app_dir, load_timeout = 120000, width = width, height = height)
}

# --- part 1: the tabs of the main window --------------------------------------
app <- start_app()
on.exit(app$stop(), add = TRUE)
Sys.sleep(3)

app$click("process", wait_ = FALSE)
wait_for_results(app)

# select the feature with the best CRS
header <- unlist(app$get_js("Array.from(document.querySelectorAll('#mzTable thead th')).map(t => t.innerText.trim())"))
crs_column <- match("CRS", header)
values <- unlist(app$get_js(sprintf(paste0("Array.from(document.querySelectorAll('#mzTable tbody tr')).map(r =>",
                                          " r.children[%d] ? r.children[%d].innerText.trim() : '')"),
                                  crs_column - 1, crs_column - 1)))
crs <- suppressWarnings(as.numeric(values))
app$run_js(sprintf(paste0("(function(){var r=document.querySelectorAll('#mzTable tbody tr')[%d];",
                          " if(r){var c=r.querySelector('input[type=checkbox]'); if(c){c.click();}}}())"),
                   which.max(crs) - 1))
Sys.sleep(5)

app$get_screenshot(file.path(out_dir, "interface.png"))
# the README shows the window, not the scrolled page
crop_write(image_read(file.path(out_dir, "interface.png")), "gui_example.png", "100%x1000+0+0")

# QC (the example data carries no spot ids, so the plate map needs data with them)
app$click(selector = "a[data-value='QC']")
Sys.sleep(5)
app$get_screenshot(file.path(out_dir, "qc_full.png"))
qc <- image_read(file.path(out_dir, "qc_full.png"))
crop_write(qc, "qc.png", "1350x1080+355+0")

# PCA: has to be computed, otherwise the plots are empty
app$click(selector = "a[data-value='PCA']")
Sys.sleep(3)
app$click("doPca", wait_ = FALSE)
Sys.sleep(25)
app$get_screenshot(file.path(out_dir, "pca_tab.png"), selector = ".tab-pane.active")
app$stop()

# the loadings titles need the wider window
app_wide <- start_app(width = 2600, height = 1300)
app_wide$click("process", wait_ = FALSE)
wait_for_results(app_wide)
app_wide$click(selector = "a[data-value='PCA']")
Sys.sleep(3)
app_wide$click("doPca", wait_ = FALSE)
Sys.sleep(25)
app_wide$get_screenshot(file.path(out_dir, "pca_tab_wide.png"), selector = ".tab-pane.active")

# cluster: the centroid label needs the wide window as well
app_wide$click(selector = "a[data-value='Cluster']")
Sys.sleep(2)
app_wide$click("doClust", wait_ = FALSE)
Sys.sleep(55)
app_wide$get_screenshot(file.path(out_dir, "clustering_tab.png"), selector = ".tab-pane.active")
app_wide$click(selector = "a[data-value='Settings']")
Sys.sleep(3)
app_wide$get_screenshot(file.path(out_dir, "settings.png"), selector = ".tab-pane.active")

# folder dialog last: it covers the page
app_wide$click(selector = "a[data-value='Main']")
Sys.sleep(2)
app_wide$click("dir")
Sys.sleep(4)
app_wide$get_screenshot(file.path(out_dir, "selectFolder.png"), selector = ".modal-content")
app_wide$stop()

# --- part 2: cut the figures ---------------------------------------------------
pca <- image_read(file.path(out_dir, "pca_tab_wide.png"))
pca_info <- image_info(pca)
crop_write(pca, "pca_scores.png", sprintf("%dx520+0+0", pca_info$width))
crop_write(pca, "pca_loadings.png", sprintf("%dx%d+0+551", pca_info$width, pca_info$height - 551))

cluster <- image_read(file.path(out_dir, "clustering_tab.png"))
cluster_info <- image_info(cluster)
# the numbers under the slider belong to the curves figure
crop_write(cluster, "clustering_curves.png", sprintf("%dx520+0+0", cluster_info$width))
# below the controls, above the metric titles
crop_write(cluster, "clustering_metrics.png",
           sprintf("%dx%d+0+512", cluster_info$width, cluster_info$height - 512))

file.copy(file.path(out_dir, "interface.png"), file.path(figure_dir, "interface.png"),
          overwrite = TRUE)
file.copy(file.path(out_dir, "settings.png"), file.path(figure_dir, "settings.png"),
          overwrite = TRUE)

message("done - check the figures in ", figure_dir)
