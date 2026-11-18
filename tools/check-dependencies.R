#!/usr/bin/env Rscript
#
# Compares the dependency lists of the app:
#   * packages in DESCRIPTION (Imports) that are not listed in req.txt
#   * packages that are only in req.txt (test/build helpers, e.g. shinytest2)
#
# Informational only: since the app is an R package, R installs the Imports as
# dependencies of M2ara anyway. The blocking checks are R CMD check,
# tools/check-lockfile.R and the payload smoke test.
#
# Run from the repository root: Rscript tools/check-dependencies.R

desc <- read.dcf("DESCRIPTION", fields = "Imports")
imports_field <- desc[1, "Imports"]

imports <- gsub("\\([^)]*\\)", "", imports_field)
imports <- trimws(unlist(strsplit(imports, ",")))
imports <- imports[nzchar(imports)]

req <- unique(trimws(scan("req.txt", character(), quiet = TRUE)))
req <- req[nzchar(req)]

# installed separately from GitHub by the installer
external <- c("MALDIcellassay")

missing <- setdiff(setdiff(imports, external), req)
extra <- setdiff(setdiff(req, "pak"), imports)

cat("DESCRIPTION Imports:", length(imports), "\n")
cat("req.txt packages:  ", length(req), "\n\n")

if (length(missing) > 0) {
  cat("in DESCRIPTION but not in req.txt (installed as dependencies):\n  ",
      paste(missing, collapse = ", "), "\n")
} else {
  cat("every imported package is listed in req.txt\n")
}

if (length(extra) > 0) {
  cat("only in req.txt (allowed, e.g. test or build helpers):\n  ",
      paste(extra, collapse = ", "), "\n")
}

cat("\ndependency lists compared (informational)\n")
