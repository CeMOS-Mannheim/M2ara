test_that("detectOutliers detects outliers", {
  # create 40 spectra with 100 mz values each so that we can have
  # 5 concentrations with 4 replicates each
  # after that we test out outlier detection on that data
  spectra <- replicate(40, createMassSpectrum(mass = 1:100,
                                              intensity = abs(rnorm(n = 100, mean = 0, sd = 1))),
                       simplify = FALSE)
  # create 5 concentrations
  conc <- rep(1:5, each = 8)

  # now we mofify the intensity of some mz values so that we
  # actually find outliers
  # we set the intensity of the first 10 mz values of the first spectrum to 100

  # set intensity of first 10 mz values of first spectrum to 100
  spectra[[1]]@intensity[1:10] <- 100
  outliers <- detectOutliers(spectra, conc, n = 10, method = "n")
  # we expect that the first spectrum is an outlier
  expect_true(outliers[1])
})

test_that("detectOutliers detects if no outliers are present", {
  # create 40 spectra with 100 mz values each so that we can have
  # 5 concentrations with 4 replicates each
  # after that we test out outlier detection on that data
  spectra <- replicate(40, createMassSpectrum(mass = 1:100,
                                              intensity = abs(rnorm(n = 100))),
                       simplify = FALSE)
  # create 5 concentrations
  conc <- rep(1:5, each = 8)
  # we expect that no outliers are present
  outliers <- detectOutliers(spectra, conc, n = 10, method = "n")
  expect_false(any(outliers))
})
