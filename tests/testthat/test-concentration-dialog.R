test_that("the table takes the concentrations from numeric names", {
  dir <- withr::local_tempdir()
  writeLines("x", file.path(dir, "0.0225.mzML"))
  writeLines("x", file.path(dir, "0.0032.mzML"))
  writeLines("x", file.path(dir, "blank.mzML"))

  table <- concentrationTable(dir, "mzml")

  expect_equal(nrow(table), 3L)
  expect_equal(table$file, c("0.0032.mzML", "0.0225.mzML", "blank.mzML"))
  expect_equal(table$concentration[1:2], c(0.0032, 0.0225))
  # the file without a number is empty and comes last
  expect_true(is.na(table$concentration[3]))
})

test_that("the table takes what a sheet in the folder already says", {
  dir <- withr::local_tempdir()
  writeLines("x", file.path(dir, "sampleA.mzML"))
  writeLines("x", file.path(dir, "sampleB.mzML"))
  writeLines(c("file,concentration", "sampleA,1", "sampleB,10"),
             file.path(dir, "samples.csv"))

  table <- concentrationTable(dir, "mzml")

  expect_equal(table$file, c("sampleA.mzML", "sampleB.mzML"))
  expect_equal(table$concentration, c(1, 10))
})

test_that("an incomplete table is reported", {
  table <- data.frame(file = c("a", "b"), concentration = c(1, NA),
                      stringsAsFactors = FALSE)
  expect_match(concentrationTableProblem(table), "No concentration for 1 file")

  expect_match(concentrationTableProblem(data.frame(file = character(0),
                                                    concentration = numeric(0))),
               "empty")

  good <- data.frame(file = c("a", "b"), concentration = c(1, 2),
                     stringsAsFactors = FALSE)
  expect_null(concentrationTableProblem(good))
})

test_that("the table becomes a sample sheet", {
  table <- data.frame(file = c("a.mzML", "b.mzML"), concentration = c(1, 2),
                      stringsAsFactors = FALSE)
  sheet <- concentrationSheet(table)

  expect_equal(names(sheet), c("file", "concentration"))
  expect_equal(sheet$concentration, c(1, 2))
})
test_that("files can be left out of the assignment", {
  table <- data.frame(file = c("a.mzML", "b.mzML", "c.mzML"),
                      concentration = c(1, 2, NA),
                      stringsAsFactors = FALSE)

  # read what the dialog collected: b is left out
  input <- list(concValue1 = 1, concValue2 = 2, concValue3 = NA,
                concUse1 = TRUE, concUse2 = FALSE, concUse3 = TRUE)
  filled <- concentrationDialogTable(table, input)

  expect_equal(filled$use, c(TRUE, FALSE, TRUE))
  expect_equal(excludedFiles(filled), "b.mzML")
  expect_equal(includedSheet(filled)$file, c("a.mzML", "c.mzML"))

  # c has no concentration but stays in, so the table is not usable
  expect_match(concentrationTableProblem(filled), "No concentration for 1 file")

  # nothing left in the analysis is reported
  none <- filled
  none$use <- FALSE
  expect_match(concentrationTableProblem(none), "No file is left")
})

test_that("spectra of left out files are found by name", {
  spectra <- list(1, 2, 3)
  names(spectra) <- c("0.5", "sampleA", "sampleB")

  left <- excludedSpectrum(spectra, c("sampleA.mzML"))
  expect_equal(left, c(FALSE, TRUE, FALSE))

  expect_equal(excludedSpectrum(spectra, character(0)), c(FALSE, FALSE, FALSE))
})
test_that("spectra without names never match an exclusion", {
  spectra <- list(1, 2)
  expect_equal(excludedSpectrum(spectra, c("a.mzML")), c(FALSE, FALSE))

  named <- list(1, 2)
  names(named) <- c("a", "b")
  expect_equal(excludedSpectrum(named, c("b.mzML")), c(FALSE, TRUE))
})