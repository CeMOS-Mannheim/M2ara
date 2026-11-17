#!/usr/bin/env Rscript
#
# Compares the package versions installed in a library with the versions pinned
# in pak.lock. Used by CI (Docker and installer) so that a build cannot
# silently ship package versions that differ from the lockfile.
#
# Packages that ship with R itself (base and recommended, e.g. MASS, Matrix,
# lattice, class, cluster, codetools) are skipped: their version is tied to the
# R version and is therefore not something the lockfile can pin.
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

# packages that are part of R itself (base + recommended)
r_library <- file.path(R.home("library"))
r_packages <- rownames(installed.packages(lib.loc = r_library))
if (is.null(r_packages)) r_packages <- character()

installed <- installed.packages(lib.loc = lib)[, c("Package", "Version"), drop = FALSE]
installed <- data.frame(
  package = installed[, "Package"],
  installed = installed[, "Version"],
  stringsAsFactors = FALSE
)

merged <- merge(wanted, installed, by = "package", all.x = TRUE)
merged$in_target <- !is.na(merged$installed)
# recommended/base packages are installed with R itself and may also live in the
# checked library (the payload bundles R's library) - their version is tied to
# the R version, so they are never compared
merged$part_of_r <- merged$package %in% r_packages

missing <- merged$package[!merged$in_target & !merged$part_of_r]
mismatch <- merged[merged$in_target & !merged$part_of_r &
                     merged$installed != merged$pinned, , drop = FALSE]
skipped <- merged$package[merged$part_of_r]

cat("library:        ", lib, "\n")
cat("lockfile:       ", lockfile, "\n")
cat("pinned packages:", nrow(merged), "\n")
cat("part of R:      ", length(skipped), "\n")
if (length(skipped) > 0L) {
  cat("               ", paste(skipped, collapse = ", "), "\n")
}

if (length(missing) > 0L) {
  cat("\nmissing in the library:\n  ", paste(missing, collapse = ", "), "\n")
}

if (nrow(mismatch) > 0L) {
  cat("\nversion mismatches:\n")
  print(mismatch[, c("package", "pinned", "installed")], row.names = FALSE)
}

if (length(missing) > 0L || nrow(mismatch) > 0L) {
  stop("installed packages differ from pak.lock", call. = FALSE)
}

cat("\nall pinned packages match pak.lock\n")
