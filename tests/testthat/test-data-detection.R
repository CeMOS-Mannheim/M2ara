test_that("the format of a folder is recognised", {
  mzml <- withr::local_tempdir()
  writeLines("x", file.path(mzml, "1.mzML"))
  expect_equal(detectDataFormat(mzml), "mzml")

  bruker <- withr::local_tempdir()
  dir.create(file.path(bruker, "0_O13"))
  writeLines("x", file.path(bruker, "0_O13", "fid"))
  expect_equal(detectDataFormat(bruker), "bruker")

  both <- withr::local_tempdir()
  writeLines("x", file.path(both, "1.mzML"))
  dir.create(file.path(both, "0_O13"))
  writeLines("x", file.path(both, "0_O13", "fid"))
  expect_equal(detectDataFormat(both), "both")

  empty <- withr::local_tempdir()
  expect_true(is.na(detectDataFormat(empty)))
  expect_true(is.na(detectDataFormat(file.path(empty, "missing"))))
})

test_that("where the concentrations come from is recognised", {
  numeric <- withr::local_tempdir()
  writeLines("x", file.path(numeric, "0.0032.mzML"))
  writeLines("x", file.path(numeric, "0.0225.mzML"))

  source <- detectConcentrationSource(numeric, "mzml")
  expect_equal(source$kind, "file names")
  expect_equal(source$nFiles, 2L)
  expect_match(describeConcentrations(source), "from the file names \\(2 files\\)")

  # a sheet in the folder helps when the names are not numeric
  sheeted <- withr::local_tempdir()
  writeLines("x", file.path(sheeted, "sampleA.mzML"))
  writeLines(c("file,concentration", "sampleA,1"), file.path(sheeted, "samples.csv"))

  source <- detectConcentrationSource(sheeted, "mzml")
  expect_equal(source$kind, "sample sheet")
  expect_equal(source$sheet, "samples.csv")

  # neither: the missing names are reported
  broken <- withr::local_tempdir()
  writeLines("x", file.path(broken, "blank.mzML"))
  writeLines("x", file.path(broken, "1.mzML"))

  source <- detectConcentrationSource(broken, "mzml")
  expect_equal(source$kind, "missing")
  expect_equal(source$missing, "blank")
  expect_match(describeConcentrations(source), "missing for 1 of 2 files")
})

test_that("Bruker folders are checked by their folder names", {
  # a Bruker dataset is the folder that holds the fid files, named by concentration
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "0.0032"))
  writeLines("x", file.path(dir, "0.0032", "fid"))

  source <- detectConcentrationSource(dir, "bruker")
  expect_equal(source$kind, "file names")
})
