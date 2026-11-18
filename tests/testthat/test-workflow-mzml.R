library(shinytest2)
library(testthat)

test_that("mzML workflow: load spectra, process, download non-empty CSV", {
  # ------------------------------------------------------------------
  # 1. Obtain test data (figshare download, cached in mzML/)
  # ------------------------------------------------------------------
  skip_if_not(obtain_mzml_data(),
              "Could not obtain mzML test data (figshare WAF may block download).")
  withr::defer(clean_mzml_data())

  test_data_dir <- normalizePath("mzML", winslash = "/", mustWork = TRUE)

  # ------------------------------------------------------------------
  # 2. Prepare a temporary app directory with a settings.csv that points
  #    to the test data. M2ara reads and writes settings.csv in the
  #    working directory, so a dedicated directory keeps the test isolated.
  # ------------------------------------------------------------------
  app_dir <- file.path(tempdir(), "m2ara-mzml-workflow")
  unlink(app_dir, recursive = TRUE)
  dir.create(app_dir, recursive = TRUE, showWarnings = FALSE)

  writeLines("M2ara::runApp()", file.path(app_dir, "app.R"))

  mzml_settings <- read.csv("settings_mzML_data.csv",
                            stringsAsFactors = FALSE)
  mzml_settings$dir <- test_data_dir

  write.csv(mzml_settings, file.path(app_dir, "settings.csv"), row.names = FALSE)

  # ------------------------------------------------------------------
  # 3. Launch the Shiny app
  # ------------------------------------------------------------------
  app <- AppDriver$new(
    app_dir = app_dir,
    name   = "mzml-workflow",
    height = 1200,
    width  = 1600,
    load_timeout = 30000
  )
  withr::defer(app$stop())

  # ------------------------------------------------------------------
  # 4. Verify directory was pre-set from settings.csv
  # ------------------------------------------------------------------
  expect_equal(app$get_value(export = "infoState"), "dir_set")

  # ------------------------------------------------------------------
  # 5. Click "Load spectra"
  # ------------------------------------------------------------------
  app$click("load")

  app$wait_for_value(
    export  = "infoState",
    timeout = 120000    # 2 minutes for loading 144 mzML files
  )
  expect_equal(app$get_value(export = "infoState"), "loaded")

  # ------------------------------------------------------------------
  # 6. Click "Process spectra"
  # ------------------------------------------------------------------
  app$click("process")

  app$wait_for_value(
    export  = "infoState",
    timeout = 300000    # 5 minutes for full processing + curve fitting
  )
  expect_equal(app$get_value(export = "infoState"), "processed")

  # ------------------------------------------------------------------
  # 7. Download the peak table CSV
  # ------------------------------------------------------------------
  csv_path <- app$get_download("downloadTable")

  expect_true(file.exists(csv_path),
              info = "Downloaded CSV should exist on disk")

  csv_content <- read.csv(csv_path)
  expect_gt(nrow(csv_content), 0)
  expect_gt(sum(csv_content$CRS > 0, na.rm = TRUE), 0)
})
