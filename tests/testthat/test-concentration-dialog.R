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
