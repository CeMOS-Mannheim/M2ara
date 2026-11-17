test_that("exportRun writes a complete archive for a processed run", {
  res <- MALDIcellassay::Blank2022res
  stats <- getStatistics(res)

  zip_file <- tempfile(fileext = ".zip")
  on.exit(unlink(zip_file), add = TRUE)

  exportRun(zip_file, res = res, stats = stats, settings = NULL,
            centroided = FALSE, plot_ready = TRUE)

  expect_true(file.exists(zip_file))

  entries <- utils::unzip(zip_file, list = TRUE)$Name
  expected <- c("README.md", "run_info.json", "peak_table.csv",
                "fitting_parameters.csv", "intensity_matrix.csv",
                "plots/curves_top.png", "plots/peaks_top.png")
  expect_true(all(expected %in% entries))

  unpacked <- file.path(tempdir(), "m2ara_export_test")
  unlink(unpacked, recursive = TRUE)
  dir.create(unpacked, recursive = TRUE)
  on.exit(unlink(unpacked, recursive = TRUE), add = TRUE)
  utils::unzip(zip_file, exdir = unpacked)

  # the tables must be readable again and contain the same number of features
  peak_table <- utils::read.csv(file.path(unpacked, "peak_table.csv"))
  expect_equal(nrow(peak_table), nrow(stats))
  expect_equal(nrow(utils::read.csv(file.path(unpacked, "intensity_matrix.csv"))),
               length(MALDIcellassay::getConc(res)))

  info <- jsonlite::fromJSON(file.path(unpacked, "run_info.json"))
  expect_equal(info$app$name, "M2ara")
  expect_true(info$dataset$spectra > 0)
  expect_gte(nrow(info$files), 5)
})

test_that("exportRun refuses to export without results", {
  res <- MALDIcellassay::Blank2022res
  zip_file <- tempfile(fileext = ".zip")
  on.exit(unlink(zip_file), add = TRUE)

  expect_warning(
    exportRun(zip_file, res = res, stats = data.frame(), plot_ready = FALSE),
    "Nothing to export"
  )
  expect_false(file.exists(zip_file))
})
