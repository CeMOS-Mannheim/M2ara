#!/usr/bin/env Rscript
# Baseline of the fit metrics for the comparison with another fit engine -------
#
# Runs the example datasets of the M2ara publication through the app and keeps the
# export archive of each run. The archives contain everything needed to compare a
# new fit engine against the current one:
#
#   peak_table.csv          pEC50, R2 (= nplr gof), FZ, FV, FS, CRS
#   fitting_parameters.csv  mz, npar, bottom, top, xmid, scal, s
#   intensity_matrix.csv    intensities per concentration and m/z (enough to refit)
#   sample_sheet.csv        which file had which concentration
#   settings.csv            the settings of the run
#   run_info.json           versions (M2ara, MALDIcellassay, nplr) and the engine
#
# The app is driven by shinytest2, so the baseline goes through exactly the code
# path a user takes (no second implementation of the pipeline).
#
# Requirements
#   * the datasets from https://doi.org/10.6084/m9.figshare.25736541, one folder per
#     dataset and the matching "*_processing_settings_m2ara.csv" next to them
#   * shinytest2 (with a browser), the package loaded with pkgload
#
# Usage
#   Rscript tools/make-fit-reference.R [--data <dir>] [--out <dir>]
#
# Default: --data C:/data/M2ara_reference, --out tools/reference
#
# The data folder is expected to look like this (names are matched loosely, the
# settings are picked by the dataset name):
#
#   <data>/Unger2020_OATP2B1_inhibition_mzML/mzML/*.mzML
#   <data>/Weigt2018_BCR-ABL_inhibition_Dasatinib_BrukerFlex/<conc folders>/...
#   <data>/unger2020_processing_settings_m2ara.csv
#   ...
#
# Output per dataset: <out>/<dataset>/result.zip, metrics.csv, meta.json, and a
# summary in <out>/reference.csv.

Sys.setenv(NOT_CRAN = "true")

suppressPackageStartupMessages({
  pkgload::load_all(".", quiet = TRUE)
  library(shinytest2)
})

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(name, default) {
  hit <- match(name, args)
  if (is.na(hit) || hit == length(args)) default else args[[hit + 1L]]
}
data_dir <- normalizePath(get_arg("--data", "C:/data/M2ara_reference"), mustWork = FALSE)
out_dir <- get_arg("--out", file.path("tools", "reference"))

if (!dir.exists(data_dir)) {
  stop("data folder not found: ", data_dir, call. = FALSE)
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# the datasets of the publication; the folder name has to start with this
datasets <- c("Unger2020", "Weigt2018", "Weigt2019", "Koch2023", "Schmidt2024")

# the folder of a dataset is the one that carries the spectra
data_folder <- function(dir, dataset) {
  candidates <- list.dirs(dir, recursive = TRUE)
  candidates <- candidates[startsWith(basename(candidates), dataset) | startsWith(basename(dirname(candidates)), dataset)]
  candidates <- candidates[grepl(paste0(dataset), candidates, fixed = TRUE)]
  for (candidate in candidates) {
    entries <- list.files(candidate)
    if (any(grepl("\\.mzml$", entries, ignore.case = TRUE)) ||
        any(file.exists(file.path(candidate, entries, "fid")))) {
      return(candidate)
    }
  }
  # a dataset that is itself a Bruker folder
  entries <- list.files(candidates, ignore.case = TRUE)
  NA_character_
}

# settings file for a dataset (e.g. "unger2020_processing_settings_m2ara.csv")
settings_file <- function(dir, dataset) {
  files <- list.files(dir, pattern = "settings.*\\.csv$", full.names = TRUE)
  key <- tolower(dataset)
  hit <- files[grepl(key, tolower(basename(files)), fixed = TRUE)]
  if (length(hit) == 0L) NA_character_ else hit[[1L]]
}

run_dataset <- function(dataset) {
  folder <- data_folder(data_dir, dataset)
  settings <- settings_file(data_dir, dataset)
  if (is.na(folder) || is.na(settings)) {
    message("skip ", dataset, ": folder or settings not found")
    return(NULL)
  }

  message("running ", dataset, " (", folder, ")")
  app_dir <- file.path(tempdir(), paste0("reference-", dataset))
  unlink(app_dir, recursive = TRUE)
  dir.create(app_dir, recursive = TRUE)
  settings_df <- read.csv(settings, stringsAsFactors = FALSE)
  settings_df$dir <- normalizePath(folder, winslash = "/")
  utils::write.csv(settings_df, file.path(app_dir, "settings.csv"), row.names = FALSE)
  writeLines("M2ara::runApp()", file.path(app_dir, "app.R"))

  app <- AppDriver$new(app_dir = app_dir, load_timeout = 180000,
                       width = 1600, height = 1000)
  on.exit(app$stop(), add = TRUE)

  app$click("process", wait_ = FALSE)
  cell <- function() {
    app$get_js(paste0("(function(){var c=document.querySelector('#mzTable tbody tr td:nth-child(2)');",
                      " return c ? c.innerText.trim() : '';})()"))
  }
  done <- FALSE
  for (i in seq_len(1800)) {
    value <- cell()
    if (is.character(value) && grepl("^[0-9]", value)) {
      done <- TRUE
      break
    }
    Sys.sleep(1)
  }
  if (!done) {
    message("no result for ", dataset)
    return(NULL)
  }
  Sys.sleep(5)

  target <- file.path(out_dir, dataset)
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  app$click("exportRun", wait_ = FALSE)
  Sys.sleep(5)
  download <- app$get_download("exportRun")
  if (is.null(download) || !file.exists(download)) {
    message("no archive for ", dataset)
    return(NULL)
  }
  file.copy(download, file.path(target, "result.zip"), overwrite = TRUE)

  # a small summary next to the archive, so the comparison can be read without
  # unpacking everything
  unzip(file.path(target, "result.zip"), files = c("peak_table.csv",
                                                  "fitting_parameters.csv"),
        exdir = target, junkpaths = TRUE)
  stats <- utils::read.csv(file.path(target, "peak_table.csv"))
  params <- utils::read.csv(file.path(target, "fitting_parameters.csv"))
  merged <- merge(params, stats, by = c("mzIdx", "mz"), all.x = TRUE)
  utils::write.csv(merged, file.path(target, "metrics.csv"), row.names = FALSE)
  file.remove(file.path(target, "peak_table.csv"), file.path(target, "fitting_parameters.csv"))

  meta <- list(
    dataset = dataset,
    folder = normalizePath(folder, winslash = "/"),
    settings = normalizePath(settings, winslash = "/"),
    features = nrow(stats),
    engine = "nplr",
    versions = list(
      M2ara = as.character(utils::packageVersion("M2ara")),
      MALDIcellassay = as.character(utils::packageVersion("MALDIcellassay")),
      nplr = as.character(utils::packageVersion("nplr")),
      R = R.version.string
    )
  )
  writeLines(jsonlite::toJSON(meta, auto_unbox = TRUE, pretty = TRUE),
             file.path(target, "meta.json"))
  message("done ", dataset, ": ", nrow(stats), " features")
  data.frame(dataset = dataset, features = nrow(stats),
             pEC50_median = stats::median(stats$pEC50, na.rm = TRUE),
             R2_median = stats::median(stats$R2, na.rm = TRUE),
             CRS_median = stats::median(stats$CRS, na.rm = TRUE))
}

results <- lapply(datasets, function(d) tryCatch(run_dataset(d), error = function(e) {
  message("error for ", d, ": ", conditionMessage(e))
  NULL
}))
results <- do.call(rbind, Filter(Negate(is.null), results))

if (is.null(results) || nrow(results) == 0L) {
  stop("no dataset could be run", call. = FALSE)
}

utils::write.csv(results, file.path(out_dir, "reference.csv"), row.names = FALSE)
message("wrote ", file.path(out_dir, "reference.csv"))
