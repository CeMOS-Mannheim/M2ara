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
            Sys.sleep(30)
            cat(timeNow(), "done!\n")

            cat(timeNow(), "performing pca...\n")
            app$click("doPca")
            Sys.sleep(3)

            cat(timeNow(), "performing clustering...\n")
            app$click("doClust")
            Sys.sleep(5)

            cat(timeNow(), "extracting values...\n")
            exp <- app$get_values(export = TRUE)

            cat(timeNow(), "check loading sucess\n")
            expect_equal(exp$export$isSpectrumList, TRUE)
            expect_equal(exp$export$numSpec, 144)

            cat(timeNow(), "check processing sucess\n")
            expect_equal(exp$export$infoState, "processed")

            cat(timeNow(), "check PCA sucess\n")
            expect_true(is_tibble(exp$export$pca$scores))
            expect_true(is_tibble(exp$export$pca$loadings))

            cat(timeNow(), "check clustering sucess\n")
            expect_true(latrend::is.lcModels(exp$export$clust))
            expect_true(is_tibble(extractLaClusters(exp$export$clust)))
            cat(timeNow(), "Cleaning up...\n")
            fs::file_delete("settings.csv")
            fs::dir_delete("mzML")
            fs::file_delete("testdata_mzML.zip")
          })

