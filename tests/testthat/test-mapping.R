write_sheet <- function(lines, ext = "csv") {
  path <- tempfile(fileext = paste0(".", ext))
  writeLines(lines, path)
  list(datapath = path, name = basename(path))
}

test_that("a sample sheet with file names is read", {
  sheet <- write_sheet(c("file,concentration,spot",
                         "0.0032,0.0032,K11;L11",
                         "0.0225,0.0225,K9;L9"))
  mapping <- processMappingFile(sheet)

  expect_equal(mapping$kind, "sheet")
  expect_equal(nrow(mapping$sheet), 2L)
  expect_equal(mapping$sheet$concentration, c(0.0032, 0.0225))
})

test_that("the legacy list of concentrations is still accepted", {
  sheet <- write_sheet(c("0.1", "1", "10"), ext = "txt")
  mapping <- processMappingFile(sheet)

  expect_equal(mapping$kind, "list")
  expect_equal(mapping$concentrations, c(0.1, 1, 10))
})

test_that("broken sample sheets are reported", {
  expect_error(processMappingFile(write_sheet(c("file", "a"))),
               "needs two columns")
  expect_error(processMappingFile(write_sheet(c("file,conc", "a,1"))),
               "needs the columns")
  expect_error(processMappingFile(write_sheet(c("file,concentration", "a,1", "a,2"))),
               "more than once")
  expect_error(processMappingFile(write_sheet(c("file,concentration", "a,high"))),
               "no valid file name or concentration")
  expect_error(processMappingFile(list(datapath = tempfile(fileext = ".xlsx"))),
               "csv")
})

test_that("a sample sheet is resolved per file", {
  sheet <- write_sheet(c("file,concentration", "0.0032,3.2e-3", "0.0225,2.25e-2"))
  mapping <- processMappingFile(sheet)

  # four spectra of one file share the concentration
  files <- c("0.0032", "0.0032", "0.0032", "0.0032", "0.0225")
  resolved <- resolveMapping(mapping, files)

  expect_equal(as.numeric(resolved), c(3.2e-3, 3.2e-3, 3.2e-3, 3.2e-3, 2.25e-2))
  expect_null(attr(resolved, "problem"))
})

test_that("a sample sheet with missing files is rejected", {
  sheet <- write_sheet(c("file,concentration", "a,1"))
  mapping <- processMappingFile(sheet)

  resolved <- resolveMapping(mapping, c("a", "b"))
  expect_length(resolved, 0)
  expect_match(attr(resolved, "problem"), "no concentration for these files: b")
})

test_that("unknown files in the sheet are reported but not an error", {
  sheet <- write_sheet(c("file,concentration", "a,1", "z,2"))
  mapping <- processMappingFile(sheet)

  resolved <- resolveMapping(mapping, c("a", "a"))
  expect_equal(as.numeric(resolved), c(1, 1))
  expect_equal(attr(resolved, "unknown"), "z")
})

test_that("the legacy list is applied per file and its length is checked", {
  mapping <- list(kind = "list", concentrations = c(1, 2))

  resolved <- resolveMapping(mapping, c("a", "a", "b"))
  expect_equal(as.numeric(resolved), c(1, 1, 2))

  tooShort <- resolveMapping(mapping, c("a", "b", "c"))
  expect_length(tooShort, 0)
  expect_match(attr(tooShort, "problem"), "sample sheet with 'file'")
})

test_that("the effective sample sheet summarises files and spots", {
  sheet <- sampleSheet(files = c("a", "a", "b"),
                       concentrations = c(1, 1, 2),
                       spots = c("K11", "L11", "K9"))

  expect_equal(sheet$file, c("a", "b"))
  expect_equal(sheet$concentration, c(1, 2))
  expect_equal(sheet$n_spectra, c(2L, 1L))
  expect_equal(sheet$spots, c("K11;L11", "K9"))

  withoutSpots <- sampleSheet(c("a", "a", "b"), c(1, 1, 2))
  expect_false("spots" %in% names(withoutSpots))
})
test_that("data file extensions are stripped, numeric names are kept", {
  expect_equal(stripDataExtension("0.0032.mzML"), "0.0032")
  expect_equal(stripDataExtension("Sample_A.fid"), "Sample_A")
  # a numeric file name must not lose its decimals
  expect_equal(stripDataExtension("0.0032"), "0.0032")
  expect_equal(stripDataExtension("0.5"), "0.5")
})
test_that("a sample sheet in the data folder is found", {
  dir <- withr::local_tempdir()
  writeLines(c("file,concentration", "a,1", "b,2"), file.path(dir, "samples.csv"))
  writeLines("x", file.path(dir, "a.mzML"))
  writeLines("x", file.path(dir, "b.mzML"))

  found <- findSampleSheet(dir)
  expect_equal(found$kind, NULL)
  expect_equal(found$file, "samples.csv")
  expect_equal(found$mapping$kind, "sheet")
  expect_null(found$error)

  # no sheet
  empty <- withr::local_tempdir()
  expect_null(findSampleSheet(empty)$mapping)

  # a broken sheet is reported but not fatal
  broken <- withr::local_tempdir()
  writeLines("file,concentration", file.path(broken, "samples.csv"))
  bad <- findSampleSheet(broken)
  expect_null(bad$mapping)
  expect_match(bad$error, "no rows")
})