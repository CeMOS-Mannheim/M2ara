library(testthat)
library(tibble)
test_that("data can be loaded and processed from mzML",
          {

              timeNow <- function() {
                format(Sys.time(), "%H:%M:%OS")
              }
              setwd(dirname(common::file.find(pattern = "req.txt")))

              # download test data and unzip
              curl::curl_download("https://figshare.com/ndownloader/files/46156788", "testdata_mzML.zip")
              if(!file.exists("testdata_mzML.zip")) {
                stop("Could not download testdata_mzML.zip.\n")
              }

              unzip("testdata_mzML.zip")

              fs::file_delete("testdata_mzML.zip")

              # overwrite settings
              if(!file.exists("tests/testthat/settings_mzML_data.csv")) {
                stop("Could not find settings_mzML_data.csv.\n")
              }
              fs::file_copy(path = "tests/testthat/settings_mzML_data.csv",
                            new_path = "settings.csv",
                            overwrite = TRUE)

              library(shinytest2)
              options(shiny.testmode = TRUE)

              cat("\n", timeNow(), "Starting app...\n")
              app <- AppDriver$new(app_dir = getwd(),
                                   name = "M2ara_mzML load test",
                                   seed = 42,
                                   timeout = 100 * 1000,
                                   load_timeout = 30 * 1000,
                                   wait = TRUE)

              Sys.sleep(5)
              app$wait_for_idle()
              cat(timeNow(), "App started. Loading data...\n")
              app$click("load")
              Sys.sleep(30)
              app$wait_for_idle(timeout = 120*1000)
              cat(timeNow(), "Start processing...\n")
              app$click("process")
              Sys.sleep(30)
              app$wait_for_idle(timeout = 120*1000)
              cat(timeNow(), "Processing done.\n")

              app$click("doPca")
              app$wait_for_idle()

              app$click("doClust")
              app$wait_for_idle()

              exp <- app$get_values(export = TRUE)

              expect_equal(exp$export$isSpectrumList, TRUE)
              expect_equal(exp$export$numSpec, 144)

              expect_equal(exp$export$infoState, "processed")

              expect_true(is_tibble(exp$export$pca$scores))
              expect_true(is_tibble(exp$export$pca$loadings))

              expect_true(latrend::is.lcModels(exp$export$clust))
              expect_true(is_tibble(extractLaClusters(exp$export$clust)))
              fs::file_delete("settings.csv")
              fs::dir_delete("mzML")
            })


