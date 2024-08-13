library(testthat)

test_that("data can be loaded and processed from Bruker Flex",
          {

            timeNow <- function() {
              format(Sys.time(), "%H:%M:%OS")
            }
            setwd(dirname(common::file.find(pattern = "req.txt")))

            # download test data and unzip
            curl::curl_download("https://figshare.com/ndownloader/files/46156791", "testdata_bruker.zip")
            if(!file.exists("testdata_bruker.zip")) {
              stop("Downloading testdata_bruker.zip failed.")
            }
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

            app <- AppDriver$new(app_dir = getwd(),
                                 name = "M2ara_bruker load test",
                                 seed = 42,
                                 timeout = 100 * 1000,
                                 load_timeout = 30 * 1000,
                                 wait = TRUE)

            app$click("load")
            app$wait_for_idle(timeout = 300*1000)
            app$click("process")
            app$wait_for_idle(timeout = 300*1000)
            exp <-app$get_values(export = TRUE)

            expect_equal(exp$export$isSpectrumList, TRUE)
            expect_equal(exp$export$numSpec, 88)
            fs::dir_delete("Curve")
            fs::file_delete("settings.csv")
            fs::file_delete("testdata_bruker.zip")
          })

