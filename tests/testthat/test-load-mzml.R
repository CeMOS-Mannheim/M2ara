# Helper to obtain test data from figshare
# Returns TRUE if data was successfully extracted
obtain_mzml_data <- function() {
  zip_file <- "testdata_mzML.zip"
  data_dir <- "mzML"
  url <- "https://figshare.com/ndownloader/files/46156788"

  # Already extracted?
  if (dir.exists(data_dir)) return(TRUE)

  # Already downloaded but not extracted?
  if (file.exists(zip_file)) {
    utils::unzip(zip_file)
    return(dir.exists(data_dir))
  }

  # Try to download (skip if no internet / curl not available)
  if (!requireNamespace("curl", quietly = TRUE)) return(FALSE)
  if (!curl::has_internet()) return(FALSE)

  result <- tryCatch({
    curl::curl_download(url, zip_file, quiet = TRUE)
    if (file.exists(zip_file) && file.size(zip_file) > 1000) {
      utils::unzip(zip_file)
      dir.exists(data_dir)
    } else {
      FALSE
    }
  }, error = function(e) FALSE)
  result
}


clean_mzml_data <- function() {
  unlink("mzML", recursive = TRUE)
  unlink("testdata_mzML.zip")
}


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
