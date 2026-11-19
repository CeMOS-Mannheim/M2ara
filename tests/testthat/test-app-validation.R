# Does the app really show the message? The unit tests cover the checks, this
# test starts the app and looks at the notification.

test_that("implausible settings are reported and do not start a run", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app_dir <- withr::local_tempdir()
  settings <- data.frame(SNR = 0, dir = tempdir(), stringsAsFactors = FALSE)
  utils::write.csv(settings, file.path(app_dir, "settings.csv"), row.names = FALSE)
  writeLines("M2ara::runApp()", file.path(app_dir, "app.R"))

  app <- shinytest2::AppDriver$new(app_dir = app_dir, load_timeout = 60000)
  on.exit(app$stop(), add = TRUE)

  app$click("process", wait_ = FALSE)

  notification <- ""
  for (i in seq_len(60)) {
    notification <- app$get_text("#shiny-notification-panel")
    notification <- if (length(notification) == 0L) "" else
      gsub("\\s+", " ", paste(notification, collapse = " "))
    if (grepl("SNR has to be", notification)) {
      break
    }
    Sys.sleep(1)
  }

  expect_match(notification, "SNR has to be a number greater than 0")

  # nothing was processed and the user can correct the setting
  expect_equal(app$get_value(export = "infoState"), "dir_set")
})
