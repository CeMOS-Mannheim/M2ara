test_that("data can be loaded and processed from mzML",
          {

            timeNow <- function() {
              format(Sys.time(), "%H:%M:%OS")
            }
            setwd(dirname(common::file.find(pattern = "req.txt")))
            cat("Working directory is ", getwd(), "\n")

            # download test data and unzip
            cat(timeNow(), "downloading mzML test data...\n")
            curl::curl_download("https://figshare.com/ndownloader/files/46156788", "testdata_mzML.zip")
            cat(timeNow(), "unzip mzML test data...\n")
            unzip("testdata_mzML.zip")

            # overwrite settings
            if(!file.exists("tests/testthat/settings_mzML_data.csv")) {
              stop("Could not find settings_mzML_data.csv.\n")
            }
            fs::file_copy(path = "tests/testthat/settings_mzML_data.csv",
                          new_path = "settings.csv",
                          overwrite = TRUE)

            library(shinytest2)
            options(shiny.testmode = TRUE)
            cat(timeNow(), "Starting app...\n")
            app <- AppDriver$new(app_dir = getwd(),
                                 name = "M2ara_mzML load test",
                                 seed = 42,
                                 timeout = 2.4*1e5)

            cat(timeNow(), "loading spectra...\n")
            app$click("load")
            cat(timeNow(), "processing...\n")
            app$click("process")
            cat(timeNow(), "done!\n")
            exp <-app$get_values(export = TRUE)

            cat(timeNow(), "running tests...\n")
            expect_equal(exp$export$isSpectrumList, TRUE)
            expect_equal(exp$export$numSpec, 144)

            cat(timeNow(), "Sucess! Cleaning up...\n")
            fs::file_delete("settings.csv")
            fs::dir_delete("mzML")
            fs::file_delete("testdata_mzML.zip")
          })

