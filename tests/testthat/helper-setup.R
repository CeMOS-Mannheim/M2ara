#' Obtain mzML test data
#'
#' The data comes from figshare and is cached next to the tests. In environments
#' where the download is blocked (e.g. behind a WAF), point the environment
#' variable M2ARA_TESTDATA_DIR to a directory that contains either the extracted
#' folder (mzML/) or the archive (testdata_mzML.zip).
#'
#' Returns TRUE if data was successfully extracted
obtain_mzml_data <- function() {
  zip_file <- "testdata_mzML.zip"
  data_dir <- "mzML"
  url <- "https://figshare.com/ndownloader/files/46156788"

  # Already extracted?
  if (dir.exists(data_dir)) return(TRUE)

  # A prepared test data directory (e.g. from a manual download)
  external <- Sys.getenv("M2ARA_TESTDATA_DIR", unset = "")
  if (nzchar(external)) {
    external_dir <- file.path(external, data_dir)
    if (dir.exists(external_dir)) {
      file.copy(external_dir, ".", recursive = TRUE)
      return(dir.exists(data_dir))
    }

    external_zip <- file.path(external, zip_file)
    if (file.exists(external_zip)) {
      utils::unzip(external_zip, exdir = ".")
      return(dir.exists(data_dir))
    }
  }

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
#' Wait until an exported test value reaches the expected state
#'
#' shinytest2 waits for the app to be idle before it reads values, but the
#' background processing polls continuously, so the app is never idle while a
#' job runs. This helper polls the exported value directly.
wait_for_info_state <- function(app, state, timeout = 600) {
  started <- Sys.time()
  repeat {
    if (identical(app$get_value(export = "infoState"), state)) {
      return(TRUE)
    }
    if (as.numeric(difftime(Sys.time(), started, units = "secs")) > timeout) {
      return(FALSE)
    }
    Sys.sleep(1)
  }
}

clean_mzml_data <- function() {
  unlink("mzML", recursive = TRUE)
  unlink("testdata_mzML.zip")
}