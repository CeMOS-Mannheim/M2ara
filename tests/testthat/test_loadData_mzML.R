test_that("data can be loaded and processed from mzML",
          {

            timeNow <- function() {
              format(Sys.time(), "%H:%M:%OS")
            }
            setwd(dirname(common::file.find(pattern = "req.txt")))

            # download test data and unzip
            curl::curl_download("https://figshare.com/ndownloader/files/46156788", "testdata_mzML.zip")
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
            app <- AppDriver$new(app_dir = getwd(),
                                 name = "M2ara_mzML load test",
                                 seed = 42,
                                 timeout = 2.4*1e5)

            app$click("load")
            app$click("process")
            Sys.sleep(30)

            app$click("doPca")
            Sys.sleep(3)

            app$click("doClust")
            Sys.sleep(5)

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
            fs::file_delete("testdata_mzML.zip")
          })

