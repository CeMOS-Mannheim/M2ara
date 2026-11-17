#!/usr/bin/env Rscript
#
# Prints the MALDIcellassay commit that pak.lock pins, so the workflows install
# exactly that commit instead of the current `main`.
#
# Usage: Rscript tools/locked-maldi-ref.R

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite")
}

if (!file.exists("pak.lock")) {
  stop("pak.lock not found (run from the repository root)", call. = FALSE)
}

lock <- jsonlite::fromJSON("pak.lock", simplifyVector = FALSE)

for (pkg in lock$packages) {
  if (identical(as.character(pkg$package), "MALDIcellassay")) {
    sha <- pkg$metadata$RemoteSha
    if (is.null(sha)) sha <- pkg$metadata$GithubSHA1
    if (is.null(sha)) stop("no commit recorded for MALDIcellassay in pak.lock", call. = FALSE)
    cat(as.character(sha), "\n")
    quit(status = 0L)
  }
}

stop("MALDIcellassay is not part of pak.lock", call. = FALSE)
