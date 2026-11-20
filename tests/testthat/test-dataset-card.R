test_that("a path is split into the parent and the last folder", {
  parts <- datasetNameParts("C:/data/Nectar/November 2026/2026_11_05_tia_C28h_48h/curve")

  # the parent is the part that says what the dataset is, "curve" says nothing
  expect_equal(parts$parent, "2026_11_05_tia_C28h_48h")
  expect_equal(parts$last, "curve")

  # a short path has no separate parent
  expect_equal(datasetNameParts("C:/data/only_one")$parent, "only_one")
  expect_equal(datasetNameParts("C:/data/only_one")$last, "")
  # a trailing separator does not create an empty last part
  expect_equal(datasetNameParts("C:/data/only_one/")$parent, "only_one")
  expect_equal(datasetNameParts("C:\\data\\a\\b")$parent, "a")
  expect_equal(datasetNameParts("C:\\data\\a\\b")$last, "b")
  expect_equal(datasetNameParts(NULL)$full, "")
  expect_equal(datasetNameParts("")$full, "")
})

test_that("the entries of the folder are counted without reading them", {
  dir <- withr::local_tempdir()
  writeLines("x", file.path(dir, "1.mzML"))
  writeLines("x", file.path(dir, "2.mzML"))

  expect_equal(datasetEntryCount(dir), 2L)
  expect_true(is.na(datasetEntryCount(file.path(dir, "missing"))))
})

controlInput <- function(...) {
  utils::modifyList(list(
    SNR = 3, normMz = 760.585, normTol = 0.1, normMeth = "mz", alignTol = 0.1,
    binTol = 100, halfWindowSize = 20, peakMethod = "SuperSmoother",
    avgMethod = "mean", VarFilterMethod = "none", SinglePointRecal = TRUE,
    sqrtTrans = FALSE, smooth = TRUE, rmBl = TRUE, monoisotopicFilter = FALSE,
    concUnits = "uM"
  ), list(...))
}

test_that("the status follows the state of the app", {
  input <- controlInput()
  appData <- list(info_state = "initial", preprocessing = list(), run_signature = NULL)
  expect_equal(datasetStatus(appData, input)$class, "empty")

  appData$info_state <- "dir_set"
  expect_equal(datasetStatus(appData, input)$class, "pending")

  appData$info_state <- "loaded"
  expect_match(datasetStatus(appData, input)$text, "press Process")

  appData$info_state <- "loadErrorFormat"
  expect_equal(datasetStatus(appData, input)$class, "error")

  appData$info_state <- "fitErrorRecal"
  expect_equal(datasetStatus(appData, input)$class, "error")
})

test_that("a processed dataset knows whether the settings still fit", {
  input <- controlInput()
  appData <- list(info_state = "processed", preprocessing = list(),
                  run_signature = NULL)

  # without a remembered run the state is simply "up to date"
  expect_equal(datasetStatus(appData, input)$class, "ok")

  appData$run_signature <- list(
    settings = processingSettings(input),
    preprocessing = list(),
    concUnits = input$concUnits
  )
  expect_equal(datasetStatus(appData, input)$class, "ok")

  # a changed setting makes the shown result outdated
  appData$run_signature$settings$SNR <- 4
  status <- datasetStatus(appData, input)
  expect_equal(status$class, "changed")
  expect_match(status$text, "Re-process")

  # ... and so does another concentration unit
  appData$run_signature$settings$SNR <- 3
  input$concUnits <- "nM"
  expect_equal(datasetStatus(appData, input)$class, "changed")
})
test_that("a running job and reading are shown", {
  input <- controlInput()
  appData <- list(info_state = "loaded", preprocessing = list(),
                  run_signature = NULL)

  expect_equal(datasetStatus(appData, input, loading = TRUE)$text, "Reading spectra ...")
  expect_equal(datasetStatus(appData, input, running = TRUE)$text, "Processing ...")
  # both take precedence over the plain state
  appData$info_state <- "processed"
  expect_equal(datasetStatus(appData, input, running = TRUE)$class, "pending")
})
test_that("a broken path does not break the card", {
  input <- controlInput()

  # while the folder dialog is open the path can be NA, empty or missing
  for (broken in list(NA_character_, character(0), "", file.path(tempdir(), "nope"))) {
    appData <- list(selected_dir = broken, info_state = "initial",
                    preprocessing = list(), run_signature = NULL)
    expect_silent(card <- datasetCardUi(appData, input))
    expect_s3_class(card, "shiny.tag")
    expect_null(cacheLookup(appData, input))
  }

  # and the name parts never error
  expect_equal(datasetNameParts(NA_character_)$full, "")
  expect_equal(datasetNameParts(character(0))$full, "")
})