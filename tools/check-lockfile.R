#!/usr/bin/env Rscript
#
# Compares the package versions installed in a library with the versions pinned
# in pak.lock. Used by CI (Docker and installer) so that a build cannot
# silently ship package versions that differ from the lockfile.
#
# Usage: Rscript tools/check-lockfile.R [library] [lockfile]
#   library   defaults to .libPaths()[1]
#   lockfile  defaults to pak.lock

args <- commandArgs(trailingOnly = TRUE)

lib <- if (length(args) >= 1L && nzchar(args[[1L]])) {
  normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
} else {
  .libPaths()[1L]
}

lockfile <- if (length(args) >= 2L && nzchar(args[[2L]])) args[[2L]] else "pak.lock"

if (!file.exists(lockfile)) {
  stop("lockfile not found: ", lockfile, call. = FALSE)
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("jsonlite is required to read the lockfile", call. = FALSE)
}

lock <- jsonlite::fromJSON(lockfile, simplifyVector = FALSE)

wanted <- data.frame(
  package = vapply(lock$packages, function(p) as.character(p$package), character(1)),
  pinned = vapply(lock$packages, function(p) as.character(p$version), character(1)),
  stringsAsFactors = FALSE
)

installed <- installed.packages(lib.loc = lib)[, c("Package", "Version"), drop = FALSE]
installed <- data.frame(
  package = installed[, "Package"],
  installed = installed[, "Version"],
  stringsAsFactors = FALSE
)

merged <- merge(wanted, installed, by = "package", all.x = TRUE)

missing <- merged$package[is.na(merged$installed)]
mismatch <- merged[!is.na(merged$installed) & merged$installed != merged$pinned, , drop = FALSE]

cat("library:        ", lib, "\n")
cat("lockfile:       ", lockfile, "\n")
cat("pinned packages:", nrow(merged), "\n")

if (length(missing) > 0L) {
  cat("\nmissing in the library:\n  ", paste(missing, collapse = ", "), "\n")
}

if (nrow(mismatch) > 0L) {
  cat("\nversion mismatches:\n")
  print(mismatch, row.names = FALSE)
}

if (length(missing) > 0L || nrow(mismatch) > 0L) {
  stop("installed packages differ from pak.lock", call. = FALSE)
}

cat("\nall packages match pak.lock\n")
