test_that("data can be loaded and processed from Bruker Flex",
          {

            timeNow <- function() {
              format(Sys.time(), "%H:%M:%OS")
            }
            setwd(dirname(common::file.find(pattern = "req.txt")))
            cat("Working directory is ", getwd(), "\n")

            # download test data and unzip
            cat(timeNow(), "downloading Bruker Flex test data...\n")
            curl::curl_download("https://figshare.com/ndownloader/files/46156791", "testdata_bruker.zip")
            cat(timeNow(), "unzip Bruker test data...\n")
            unzip("testdata_bruker.zip")

            # overwrite settings
            if(!file.exists("tests/testthat/settings_bruker_data.csv")) {
              stop("Could not find settings_bruker_data.csv.\n")
            }
            fs::file_copy(path = "tests/testthat/settings_bruker_data.csv",
                          new_path = "settings.csv",
                          overwrite = TRUE)

            library(shinytest2)
            options(shiny.testmode = TRUE)
            cat(timeNow(), "Starting app...\n")
            app <- AppDriver$new(app_dir = getwd(),
                                 name = "M2ara_bruker load test",
                                 seed = 42,
                                 timeout = 4.5*1e5)

            cat(timeNow(), "loading spectra...\n")
            app$click("load")
            Sys.sleep(120)
            cat(timeNow(), "processing...\n")
            app$click("process")
            cat(timeNow(), "done!\n")
            exp <-app$get_values(export = TRUE)

            cat(timeNow(), "running tests...\n")
            expect_equal(exp$export$isSpectrumList, TRUE)
            expect_equal(exp$export$numSpec, 88)

            cat(timeNow(), "Sucess! Cleaning up...\n")
            fs::dir_delete("Curve")
            fs::file_delete("settings.csv")
            fs::file_delete("testdata_bruker.zip")
          })

