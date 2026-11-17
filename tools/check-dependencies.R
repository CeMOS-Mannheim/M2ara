#!/usr/bin/env Rscript
#
# Checks that the dependency lists of the app stay in sync:
#   * every package in DESCRIPTION (Imports) must be installed by the installer
#     (req.txt) - otherwise the installed app fails at start-up
#   * packages that are only in req.txt are reported, but are allowed (test and
#     build helpers such as shinytest2 or common)
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
  cat("missing in req.txt (imported by the app):\n  ",
      paste(missing, collapse = ", "), "\n")
} else {
  cat("every imported package is listed in req.txt\n")
}

if (length(extra) > 0) {
  cat("only in req.txt (allowed, e.g. test or build helpers):\n  ",
      paste(extra, collapse = ", "), "\n")
}

if (length(missing) > 0) {
  stop("req.txt is missing packages that the app imports: ",
       paste(missing, collapse = ", "), call. = FALSE)
}

cat("\ndependency lists are consistent\n")
