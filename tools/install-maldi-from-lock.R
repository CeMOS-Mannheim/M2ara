#!/usr/bin/env Rscript
#
# Installs MALDIcellassay from the commit recorded in pak.lock. Dependencies are
# resolved from the frozen CRAN snapshot in R_SNAPSHOT (if set).
#
# Used by the Docker workflow; the installer uses the same spec helper via
# installer/scripts/install-packages.R.

source("tools/maldi-spec.R")

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite")
}

if (!file.exists("pak.lock")) {
  stop("pak.lock not found (run from the repository root)", call. = FALSE)
}

if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}

lock <- jsonlite::fromJSON("pak.lock", simplifyVector = FALSE)
entry <- Filter(function(pkg) identical(as.character(pkg$package), "MALDIcellassay"),
                lock$packages)

if (length(entry) == 0L) {
  stop("MALDIcellassay is not part of pak.lock", call. = FALSE)
}

sha <- entry[[1L]]$metadata$RemoteSha
if (is.null(sha)) sha <- entry[[1L]]$metadata$GithubSHA1
if (is.null(sha)) {
  stop("no commit recorded for MALDIcellassay in pak.lock", call. = FALSE)
}

snapshot <- Sys.getenv("R_SNAPSHOT", unset = "")
if (nzchar(snapshot)) {
  options(repos = c(CRAN = paste0("https://packagemanager.posit.co/cran/", snapshot)))
  cat("CRAN snapshot:", getOption("repos")[["CRAN"]], "\n")
}

spec <- maldiCellAssaySpec(sha)
cat("MALDIcellassay commit:", sha, "\n")
cat("pak spec:", spec, "\n")

pak::pkg_install(spec)
