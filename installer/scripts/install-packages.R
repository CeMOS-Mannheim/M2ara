#!/usr/bin/env Rscript
#
# Installs every R package M2ara needs into the R library of the installer
# payload, so the resulting installer works without R or an internet connection
# on the target machine.
#
# Usage: Rscript --vanilla install-packages.R <payload-dir> <repo-dir>
#
# <payload-dir>  directory that will be shipped by the installer
#                (must already contain R/, see prepare-runtime.ps1)
# <repo-dir>     checkout of the M2ara repository (source of req.txt)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) {
  stop("usage: install-packages.R <payload-dir> <repo-dir>", call. = FALSE)
}

payload_dir <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
repo_dir <- normalizePath(args[[2]], winslash = "/", mustWork = TRUE)

lib_dir <- file.path(payload_dir, "R", "library")
if (!dir.exists(lib_dir)) {
  stop("bundled R library not found: ", lib_dir, call. = FALSE)
}

# Use *only* the bundled library: nothing installed on the CI runner may leak
# into the payload (base and recommended packages are part of R itself).
.libPaths(lib_dir)

# Frozen CRAN snapshot (see pak.lock): with R_SNAPSHOT set, the installation
# resolves exactly the versions recorded in pak.lock. Without it the current
# CRAN is used (local development only).
cran <- local({
  snapshot <- Sys.getenv("R_SNAPSHOT", unset = "")
  if (nzchar(snapshot)) {
    paste0("https://packagemanager.posit.co/cran/", snapshot)
  } else {
    "https://cloud.r-project.org"
  }
})
cat("CRAN repository:", cran, "\n")

options(
  repos = c(CRAN = cran),
  timeout = 1800,
  Ncpus = max(1L, parallel::detectCores()),
  warn = 1
)

req_file <- file.path(repo_dir, "req.txt")
if (!file.exists(req_file)) {
  stop("req.txt not found: ", req_file, call. = FALSE)
}

required <- unique(trimws(scan(req_file, character(), quiet = TRUE)))
required <- required[nzchar(required)]
cat("Packages listed in req.txt:", length(required), "\n")

installed_in_payload <- function() {
  rownames(installed.packages(lib.loc = lib_dir))
}

# --- CRAN packages ----------------------------------------------------------
# rmarkdown is not listed in req.txt, but app.R calls knitr::knit("manual.Rmd")
# at start-up and a bare R installation (unlike RStudio) does not bring it.
# Installing it here keeps the start-up independent of the user's R setup.
extra_packages <- c("rmarkdown")

# Plain install.packages() is used for the CRAN part: it is sequential, resolves
# dependencies itself and needs no helper tooling. pak (below) would also work,
# but it spawns helper processes for which it needs callr/processx, i.e. packages
# that are only installed during this very step.
cran_packages <- unique(c(setdiff(required, "pak"), extra_packages))
missing <- setdiff(cran_packages, installed_in_payload())
if (length(missing) > 0L) {
  cat("Installing", length(missing), "CRAN package(s):\n  ", paste(missing, collapse = ", "), "\n")
  # One retry: CRAN downloads occasionally time out on CI runners.
  for (attempt in 1:2) {
    install.packages(
      missing,
      lib = lib_dir,
      repos = cran,
      dependencies = c("Depends", "Imports", "LinkingTo"),
      quiet = FALSE
    )
    missing <- setdiff(cran_packages, installed_in_payload())
    if (length(missing) == 0L || attempt == 2) break
    cat("Retrying", length(missing), "package(s):", paste(missing, collapse = ", "), "\n")
  }
} else {
  cat("All req.txt packages are already present.\n")
}

# pak is part of req.txt (the app falls back to it for missing packages) and is
# the only reliable way to install a GitHub package with its dependencies.
if (!requireNamespace("pak", quietly = TRUE)) {
  cat("Installing pak ...\n")
  install.packages("pak", lib = lib_dir, repos = cran, quiet = TRUE)
}
if (!requireNamespace("pak", quietly = TRUE)) {
  stop("pak could not be installed into the payload library", call. = FALSE)
}

# --- MALDIcellassay (GitHub) ------------------------------------------------
# The commit comes from pak.lock (the workflow passes it in); a commit cannot be
# used as a git ref by pak, see tools/maldi-spec.R
maldi_ref <- Sys.getenv("MALDI_CELLASSAY_REF", unset = "")
spec_helper <- file.path(repo_dir, "tools", "maldi-spec.R")
if (file.exists(spec_helper)) {
  source(spec_helper)
  maldi_spec <- maldiCellAssaySpec(maldi_ref)
} else {
  maldi_spec <- paste0("CeMOS-Mannheim/MALDIcellassay@", maldi_ref)
}

if (!"MALDIcellassay" %in% installed_in_payload()) {
  cat("Installing ", maldi_spec, " ...\n", sep = "")
  pak::pkg_install(maldi_spec, upgrade = FALSE, ask = FALSE)
} else {
  cat("MALDIcellassay is already present in the payload library.\n")
}

# --- verification -----------------------------------------------------------
needed <- unique(c(required, extra_packages, "MALDIcellassay"))
missing <- setdiff(needed, installed_in_payload())
if (length(missing) > 0L) {
  stop(
    "the payload library is incomplete, missing: ",
    paste(missing, collapse = ", "),
    call. = FALSE
  )
}

cat("Checking that all packages can be loaded ...\n")
for (package in needed) {
  loaded <- suppressWarnings(suppressPackageStartupMessages(
    require(package, character.only = TRUE, quietly = TRUE)
  ))
  if (!isTRUE(loaded)) {
    stop("package cannot be loaded: ", package, call. = FALSE)
  }
}

# --- manifest ---------------------------------------------------------------
versions <- installed.packages(lib.loc = lib_dir)[, c("Package", "Version"), drop = FALSE]
versions <- versions[order(versions[, "Package"]), , drop = FALSE]

maldi_description <- packageDescription("MALDIcellassay", lib.loc = lib_dir)
maldi_sha <- if (is.null(maldi_description$RemoteSha)) "unknown" else as.character(maldi_description$RemoteSha)
if (length(maldi_sha) == 0L || is.na(maldi_sha)) maldi_sha <- "unknown"

library_files <- list.files(lib_dir, recursive = TRUE, full.names = TRUE)
library_size_mb <- round(sum(file.size(library_files), na.rm = TRUE) / 1024^2, 1)

manifest <- c(
  "M2ara installer payload",
  paste0("Created: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("R: ", R.version.string),
  paste0("MALDIcellassay ref: ", maldi_ref, " (", maldi_sha, ")"),
  paste0("Packages: ", nrow(versions), " (", library_size_mb, " MB)"),
  "",
  sprintf("%-30s %s", versions[, "Package"], versions[, "Version"])
)

manifest_file <- file.path(payload_dir, "PACKAGES.txt")
writeLines(manifest, manifest_file)

cat("Wrote", manifest_file, "\n")
cat("R packages ready (", nrow(versions), "packages, ", library_size_mb, " MB)\n", sep = "")
