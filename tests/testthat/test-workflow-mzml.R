library(shinytest2)
library(testthat)

test_that("mzML workflow: load spectra, process, export a complete archive", {
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
  # 7. Export the results (one ZIP with all tables)
  # ------------------------------------------------------------------
  zip_path <- app$get_download("exportRun")

  expect_true(file.exists(zip_path),
              info = "Downloaded archive should exist on disk")

  entries <- utils::unzip(zip_path, list = TRUE)$Name
  expect_true(all(c("peak_table.csv", "fitting_parameters.csv",
                    "intensity_matrix.csv", "run_info.json") %in% entries))

  unpacked <- file.path(tempdir(), "mzml_export")
  unlink(unpacked, recursive = TRUE)
  dir.create(unpacked, recursive = TRUE)
  withr::defer(unlink(unpacked, recursive = TRUE))
  utils::unzip(zip_path, exdir = unpacked)

  csv_content <- read.csv(file.path(unpacked, "peak_table.csv"))
  expect_gt(nrow(csv_content), 0)
  expect_gt(sum(csv_content$CRS > 0, na.rm = TRUE), 0)
})
