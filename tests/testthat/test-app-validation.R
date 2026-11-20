# Does the app really behave as intended? The unit tests cover the helpers, these
# tests start the app (shinytest2) and look at the state and the notifications.
# shiny::testServer is not an option here: the app needs shinyhelper, shinyFiles
# and custom messages, which all expect a real session.

startM2araApp <- function(dir = tempdir(), settings = data.frame()) {
  app_dir <- withr::local_tempdir()
  dir <- dir

  if (nrow(settings) == 0L) {
    settings <- data.frame(dir = dir, stringsAsFactors = FALSE)
  } else {
    settings$dir <- dir
  }

  utils::write.csv(settings, file.path(app_dir, "settings.csv"), row.names = FALSE)
  writeLines("M2ara::runApp()", file.path(app_dir, "app.R"))

  shinytest2::AppDriver$new(app_dir = app_dir, load_timeout = 60000)
}

notificationsOf <- function(app, pattern = NULL, timeout = 60) {
  text <- ""
  for (i in seq_len(timeout)) {
    text <- app$get_text("#shiny-notification-panel")
    text <- if (length(text) == 0L) "" else gsub("\\s+", " ", paste(text, collapse = " "))
    if (is.null(pattern) || grepl(pattern, text)) {
      break
    }
    Sys.sleep(1)
  }
  text
}

test_that("implausible settings are reported and do not start a run", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- startM2araApp(settings = data.frame(SNR = 0, stringsAsFactors = FALSE))
  on.exit(app$stop(), add = TRUE)

  app$click("process", wait_ = FALSE)
  notification <- notificationsOf(app, "SNR has to be")

  expect_match(notification, "SNR has to be a number greater than 0")

  # nothing was processed and the user can correct the setting
  expect_equal(app$get_value(export = "infoState"), "dir_set")
})

test_that("a folder without spectra ends in the format error", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  dir <- withr::local_tempdir()
  writeLines("not a spectrum", file.path(dir, "notes.txt"))

  app <- startM2araApp(dir = dir)
  on.exit(app$stop(), add = TRUE)

  app$click("process", wait_ = FALSE)
  notificationsOf(app, "Wrong file format|format")

  expect_equal(app$get_value(export = "infoState"), "loadErrorFormat")
})

test_that("the clustering does not start without data", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- startM2araApp()
  on.exit(app$stop(), add = TRUE)

  app$click(selector = "a[data-value='Cluster']")
  app$click("doClust", wait_ = FALSE)
  Sys.sleep(3)

  expect_false(isTRUE(app$get_value(output = "clustRunning")))
  expect_equal(app$get_value(export = "infoState"), "dir_set")
})
