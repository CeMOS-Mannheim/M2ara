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
  # 2. Prepare a temporary settings.csv with dir pointing to test data
  # ------------------------------------------------------------------
  app_root <- dirname(find_req_txt())

  mzml_settings <- read.csv("settings_mzML_data.csv",
                            stringsAsFactors = FALSE)
  mzml_settings$dir <- test_data_dir

  settings_path <- file.path(app_root, "settings.csv")

  # Backup current settings.csv (if any)
  if (file.exists(settings_path)) {
    backup_path <- file.path(app_root, "settings.csv.bak")
    file.copy(settings_path, backup_path, overwrite = TRUE)
    withr::defer({
      file.copy(backup_path, settings_path, overwrite = TRUE)
      unlink(backup_path)
    })
  } else {
    withr::defer(unlink(settings_path))
  }

  write.csv(mzml_settings, settings_path, row.names = FALSE)

  # ------------------------------------------------------------------
  # 3. Launch the Shiny app
  # ------------------------------------------------------------------
  app <- AppDriver$new(
    app_dir = app_root,
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
})
