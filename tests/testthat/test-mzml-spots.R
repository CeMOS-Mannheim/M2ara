write_spot_mzml <- function(spots, file = tempfile(fileext = ".mzML")) {
  lines <- c('<?xml version="1.0" encoding="utf-8"?>', "<mzML>")
  for (i in seq_along(spots)) {
    attr <- if (is.na(spots[i])) {
      ""
    } else {
      sprintf(' spotID="chip=0 spot=%s"', spots[i])
    }
    lines <- c(lines, sprintf('<spectrum index="%d" id="frame=%d"%s defaultArrayLength="2">',
                              i - 1L, i, attr))
  }
  writeLines(c(lines, "</mzML>"), file)
  file
}

test_that("mzmlFileSpots reads the spot ids of a file", {
  file <- write_spot_mzml(c("K11", "L11", NA))

  expect_equal(mzmlFileSpots(file), c("K11", "L11"))
  expect_length(mzmlFileSpots(tempfile()), 0)
})

test_that("addMzmlSpots stores the spot as patch metadata", {
  skip_if_not_installed("MALDIquant")

  file <- write_spot_mzml(c("K11", "L11"))
  peaks <- lapply(1:2, function(i) {
    p <- MALDIquant::createMassPeaks(mass = c(100, 200), intensity = c(1, 2))
    MALDIquant::metaData(p) <- list(file = file, numberInFile = i)
    p
  })

  out <- addMzmlSpots(peaks)
  patches <- vapply(out, function(x) MALDIquant::metaData(x)[["patch"]], character(1))

  expect_equal(patches, c("K11", "L11"))
  expect_equal(MALDIcellassay::extractSpots(out), c("K11", "L11"))
})

test_that("addMzmlSpots keeps spectra without spot information", {
  skip_if_not_installed("MALDIquant")

  file <- write_spot_mzml(c(NA, NA))
  peaks <- lapply(1:2, function(i) {
    p <- MALDIquant::createMassPeaks(mass = c(100), intensity = c(1))
    MALDIquant::metaData(p) <- list(file = file, numberInFile = i)
    p
  })

  out <- addMzmlSpots(peaks)
  patches <- vapply(out, function(x) {
    value <- MALDIquant::metaData(x)[["patch"]]
    if (is.null(value)) "" else value
  }, character(1))

  expect_equal(patches, c("", ""))
})
