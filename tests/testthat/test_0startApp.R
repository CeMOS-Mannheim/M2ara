library(testthat)
test_that("App starts.", {
  setwd(dirname(common::file.find(pattern = "req.txt")))
  # make sure settings.csv is not present
  if(file.exists("settings.csv")) {
    fs::file_delete("settings.csv")
  }

  library(shinytest2)
  options(shiny.testmode = TRUE)

  app <- AppDriver$new(app_dir = getwd(),
                       name = "M2ara_start test",
                       seed = 42,
                       timeout = 4.5*1e5)

  app$wait_for_idle()

  exp <- app$get_values(export = TRUE)

  expect_equal(exp$export$infoState, "inital")
})
