test_that("mzML data can be loaded with expected properties", {
  skip_if_not(obtain_mzml_data(),
              "Could not obtain mzML test data (figshare WAF may block download).")

  spectra <- MALDIquantForeign::import("mzML", type = "mzML")

  expect_true(MALDIquant::isMassSpectrumList(spectra))
  expect_length(spectra, 144)

  # check valid intensity/mass values
  for (s in spectra) {
    expect_true(all(MALDIquant::mass(s) > 0))
    expect_true(all(MALDIquant::intensity(s) >= 0))
  }

  clean_mzml_data()
})
