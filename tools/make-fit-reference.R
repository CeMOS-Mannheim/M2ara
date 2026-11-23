#!/usr/bin/env Rscript
# Baseline of the fit metrics for the comparison with another fit engine -------
#
# Runs the example datasets of the M2ara publication and keeps the export archive
# of each run. The archives contain everything needed to compare a new fit engine
# against the current one:
#
#   peak_table.csv          pEC50, R2 (= nplr gof), FZ, FV, FS, CRS
#   fitting_parameters.csv  mz, npar, bottom, top, xmid, scal, s
#   intensity_matrix.csv    intensities per concentration and m/z (enough to refit)
#   sample_sheet.csv        which file had which concentration
#   settings.csv            the settings of the run
#   run_info.json           versions (M2ara, MALDIcellassay, nplr) and the engine
#
# The run uses the same functions the app calls (loadSpectraData -> processRun ->
# storeResults -> exportRun), without the Shiny layer: same code, no browser, and
# the result is available directly.
#
# Requirements
#   * the datasets from https://doi.org/10.6084/m9.figshare.25736541, one folder
#     per dataset and the matching "*_processing_settings_m2ara.csv" next to them
#   * the package loaded with pkgload
#
# Usage
#   Rscript tools/make-fit-reference.R [--data <dir>] [--out <dir>] [--dataset <name>]
#
# Default: --data D:/m2ara_reference, --out tools/reference, all datasets.
#
# Output per dataset: <out>/<dataset>/result.zip, metrics.csv, meta.json, and a
# summary in <out>/reference.csv.

Sys.setenv(NOT_CRAN = "true")
suppressPackageStartupMessages(pkgload::load_all(".", quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(name, default) {
  hit <- match(name, args)
  if (is.na(hit) || hit == length(args)) default else args[[hit + 1L]]
}
data_dir <- normalizePath(get_arg("--data", "D:/m2ara_reference"), mustWork = FALSE)
out_dir <- get_arg("--out", file.path("tools", "reference"))
only <- get_arg("--dataset", "")

if (!dir.exists(data_dir)) {
  stop("data folder not found: ", data_dir, call. = FALSE)
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

datasets <- c("Unger2020", "Weigt2018", "Weigt2019", "Koch2023", "Schmidt2024")
if (nzchar(only)) {
  datasets <- datasets[startsWith(datasets, only)]
}

# the folder of a dataset: the one whose children are the concentrations (Bruker)
# or that holds the mzML files
data_folder <- function(dir, dataset) {
  candidates <- list.dirs(dir, recursive = TRUE)
  candidates <- candidates[grepl(tolower(dataset), tolower(candidates), fixed = TRUE)]
  for (candidate in sort(candidates)) {
    entries <- list.files(candidate)
    if (any(grepl("\\.mzml$", entries, ignore.case = TRUE))) {
      return(candidate)
    }
    subdirs <- list.dirs(candidate, recursive = FALSE)
    if (length(subdirs) == 0L) {
      next
    }
    childNames <- basename(subdirs)
    hasFid <- vapply(subdirs, function(d) any(tolower(list.files(d)) == "fid"), logical(1))
    if (any(hasFid) && all(!is.na(suppressWarnings(as.numeric(childNames))))) {
      return(candidate)
    }
  }
  NA_character_
}

settings_file <- function(dir, dataset) {
  files <- list.files(dir, pattern = "settings.*\\.csv$", full.names = TRUE)
  hit <- files[grepl(tolower(dataset), tolower(basename(files)), fixed = TRUE)]
  if (length(hit) == 0L) NA_character_ else hit[[1L]]
}

# the input list a settings.csv becomes (what the app reads from its widgets)
inputFromSettings <- function(settings) {
  list(
    fileFormat = settings$fileFormat,
    checkEmpty = isTRUE(settings$checkEmpty),
    smooth = isTRUE(settings$smooth),
    rmBl = isTRUE(settings$rmBl),
    sqrtTrans = isTRUE(settings$sqrtTrans),
    monoisotopicFilter = isTRUE(settings$monoisotopicFilter),
    SNR = settings$SNR,
    avgMethod = settings$avgMethod,
    normMeth = settings$normMeth,
    VarFilterMethod = settings$VarFilterMethod,
    SinglePointRecal = isTRUE(settings$SinglePointRecal),
    normMz = settings$normMz,
    normTol = settings$normTol,
    alignTol = settings$alignTol,
    binTol = settings$binTol,
    halfWindowSize = settings$halfWindowSize,
    peakMethod = settings$peakMethod,
    concUnits = settings$concUnits
  )
}

run_dataset <- function(dataset) {
  folder <- data_folder(data_dir, dataset)
  settingsPath <- settings_file(data_dir, dataset)
  if (is.na(folder) || is.na(settingsPath)) {
    message("skip ", dataset, ": folder or settings not found")
    return(NULL)
  }

  started <- Sys.time()
  message("running ", dataset, " (", folder, ")")

  settingsCsv <- utils::read.csv(settingsPath, stringsAsFactors = FALSE)
  input <- inputFromSettings(settingsCsv)

  appData <- emptyAppDataObject()
  appData$selected_dir <- folder
  # the app does this when a folder is picked; the loader only runs in this state
  setInfoState(appData, "dir_set")
  appData$preprocessing <- list(smooth = input$smooth,
                                rmBl = input$rmBl,
                                sqrtTrans = input$sqrtTrans,
                                monoisotopicFilter = input$monoisotopicFilter)

  appData <- isolate(loadSpectraData(input, appData, mapping = NULL))
  if (!identical(isolate(appData$info_state), "loaded")) {
    message("load failed for ", dataset, ": ", isolate(appData$load_problem))
    return(NULL)
  }

  # the app converts the concentrations to the unit of the UI before fitting
  appData <- isolate(setConcentrationUnit(appData, input))

  settings <- processingSettings(input)
  problems <- validateProcessingSettings(settings)
  if (length(problems) > 0L) {
    message("invalid settings for ", dataset, ": ", paste(problems, collapse = " "))
    return(NULL)
  }

  result <- isolate(processRun(appData$spec_all, appData$spec_idx,
                                appData$centroided, appData$preprocessing,
                                settings, dir = folder))
  if (!isTRUE(result$ok)) {
    message("processing failed for ", dataset)
    return(NULL)
  }

  appData <- isolate(storeResults(appData, res = result$res, input,
                                   stats = getStatistics(result$res)))

  target <- file.path(out_dir, dataset)
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  # zip() wants an absolute path that exists
  archive <- normalizePath(file.path(target, "result.zip"), winslash = "/", mustWork = FALSE)
  if (file.exists(archive)) {
    file.remove(archive)
  }
  isolate(exportRun(archive,
                    res = appData$res,
                    stats = appData$stats,
                    settings = settingsPath,
                    centroided = appData$centroided,
                    plot_ready = TRUE,
                    samples = appData$samples,
                    samples_source = appData$mapping_source))

  # parameters and statistics together, for a quick look
  extracted <- file.path(target, "extracted")
  dir.create(extracted, showWarnings = FALSE)
  unzip(archive, files = c("peak_table.csv", "fitting_parameters.csv"),
        exdir = extracted, junkpaths = TRUE)
  stats <- utils::read.csv(file.path(extracted, "peak_table.csv"))
  params <- utils::read.csv(file.path(extracted, "fitting_parameters.csv"))
  metrics <- merge(params, stats, by = "mzIdx", all.x = TRUE)
  utils::write.csv(metrics, file.path(target, "metrics.csv"), row.names = FALSE)
  unlink(extracted, recursive = TRUE)

  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  meta <- list(
    dataset = dataset,
    folder = normalizePath(folder, winslash = "/"),
    settings = normalizePath(settingsPath, winslash = "/"),
    features = nrow(stats),
    engine = "nplr",
    seconds = round(elapsed, 1),
    versions = list(
      M2ara = as.character(utils::packageVersion("M2ara")),
      MALDIcellassay = as.character(utils::packageVersion("MALDIcellassay")),
      nplr = as.character(utils::packageVersion("nplr")),
      R = R.version.string
    )
  )
  writeLines(jsonlite::toJSON(meta, auto_unbox = TRUE, pretty = TRUE),
             file.path(target, "meta.json"))

  message(sprintf("done %s: %d features in %.1f s", dataset, nrow(stats), elapsed))
  data.frame(dataset = dataset, features = nrow(stats), seconds = round(elapsed, 2),
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

summary_path <- file.path(out_dir, "reference.csv")
if (file.exists(summary_path) && !nzchar(only)) {
  file.remove(summary_path)
}
if (file.exists(summary_path)) {
  previous <- utils::read.csv(summary_path)
  previous <- previous[!previous$dataset %in% results$dataset, , drop = FALSE]
  results <- rbind(previous, results)
}
utils::write.csv(results, summary_path, row.names = FALSE)
message("wrote ", summary_path)
