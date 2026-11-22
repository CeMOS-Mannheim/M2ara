validSettings <- function(...) {
  modifyList(list(SNR = 3, normMz = 760.585, normTol = 0.1, halfWindowSize = 20,
                  alignTol = 0.1, binTol = 100), list(...))
}

test_that("valid settings pass", {
  expect_length(validateProcessingSettings(validSettings()), 0L)
})

test_that("implausible settings are reported", {
  expect_match(validateProcessingSettings(validSettings(SNR = 0))[1], "SNR")
  expect_match(validateProcessingSettings(validSettings(SNR = NA))[1], "SNR")
  expect_match(validateProcessingSettings(validSettings(normMz = -1))[1], "normalization m/z")
  expect_match(validateProcessingSettings(validSettings(normTol = 0))[1], "normalization tolerance")
  expect_match(validateProcessingSettings(validSettings(halfWindowSize = 0))[1], "window size")
  expect_match(validateProcessingSettings(validSettings(alignTol = -0.1))[1], "alignment tolerance")
  expect_match(validateProcessingSettings(validSettings(binTol = 0))[1], "bin tolerance")

  # several problems at once
  expect_length(validateProcessingSettings(validSettings(SNR = 0, binTol = 0)), 2L)
})

test_that("the mass range of the spectra is known", {
  specs <- MALDIcellassay::Blank2022spec
  range <- spectraMassRange(specs)

  expect_length(range, 2L)
  expect_true(range[1L] < range[2L])
  # the default normalization mass is inside the range of this dataset
  expect_true(760.585 > range[1L] && 760.585 < range[2L])

  expect_null(spectraMassRange(NULL))
  expect_null(spectraMassRange(list()))
})
