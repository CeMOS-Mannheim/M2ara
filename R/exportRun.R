# Export of a processed run ----------------------------------------------------
#
# One ZIP archive with everything that is needed to reuse and reproduce the
# results outside M2ara: the three result tables, the settings that were used and
# a machine readable run_info.json. The archive layout is documented in
# README.md inside the archive.

exportRun <- function(file, res, stats, settings = NULL, centroided = FALSE,
                      plot_ready = FALSE, samples = NULL,
                      samples_source = NULL) {
  if (!isTRUE(plot_ready)) {
    warning("Nothing to export. Load and process data.")
    return(invisible(NULL))
  }

  dataset <- exportDatasetName(res)
  root <- tempfile(pattern = paste0("m2ara_", dataset, "_"))
  dir.create(root, recursive = TRUE)

  exported <- exportTables(res, stats, root)

  samples_file <- exportSampleSheet(samples, root)
  if (!is.null(samples_file)) {
    exported <- c(exported, samples_file)
  }

  settings_file <- exportSettings(settings, root)
  if (!is.null(settings_file)) {
    exported <- c(exported, settings_file)
  }

  manifest <- data.frame(
    file = exported,
    md5 = unname(tools::md5sum(file.path(root, exported))),
    stringsAsFactors = FALSE
  )

  writeLines(
    runInfoJson(res, centroided, settings_file, manifest, dataset, samples_file, samples,
                samples_source),
    file.path(root, "run_info.json")
  )
  writeLines(
    exportReadme(res, stats, centroided, manifest, dataset, samples_file),
    file.path(root, "README.md")
  )

  files <- list.files(root, recursive = TRUE, all.files = FALSE)
  files <- files[!dir.exists(file.path(root, files))]

  zip::zipr(file, files = files, root = root, mode = "mirror")

  invisible(file)
}

# name of the dataset (the folder the spectra came from, if the object knows it)
exportDatasetName <- function(res) {
  directory <- exportDirectory(res)
  if (is.na(directory) || !nzchar(directory)) {
    return("M2ara_dataset")
  }
  basename(directory)
}

exportDirectory <- function(res) {
  directory <- tryCatch(getDirectory(res), error = function(e) NULL)
  if (is.null(directory) || length(directory) == 0L || !nzchar(directory[1L])) {
    return(NA_character_)
  }
  as.character(directory[1L])
}

# the three result tables ------------------------------------------------------
exportTables <- function(res, stats, root) {
  readr::write_excel_csv(stats, file.path(root, "peak_table.csv"))

  readr::write_excel_csv(
    as.data.frame(getFittingParameters(res)),
    file.path(root, "fitting_parameters.csv")
  )

  intensity <- getIntensityMatrix(res) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(conc = getConc(res))

  readr::write_excel_csv(intensity, file.path(root, "intensity_matrix.csv"))

  c("peak_table.csv", "fitting_parameters.csv", "intensity_matrix.csv")
}

# the settings of this run -----------------------------------------------------
# effective assignment of files to concentrations, so a result can be traced
# back later
exportSampleSheet <- function(samples, root) {
  if (is.null(samples) || nrow(samples) == 0L) {
    return(NULL)
  }

  readr::write_excel_csv(samples, file.path(root, "sample_sheet.csv"))
  "sample_sheet.csv"
}

exportSettings <- function(settings, root) {
  if (is.null(settings) || length(settings) == 0L || !file.exists(settings)) {
    return(NULL)
  }
  file.copy(settings, file.path(root, "settings.csv"), overwrite = TRUE)
  "settings.csv"
}

# machine readable run information ---------------------------------------------
runInfoJson <- function(res, centroided, settings_file, manifest, dataset,
                        samples_file = NULL, samples = NULL,
                        samples_source = NULL) {
  version_of <- function(package) {
    tryCatch(
      as.character(utils::packageVersion(package)),
      error = function(e) NA_character_
    )
  }

  file_entries <- lapply(seq_len(nrow(manifest)), function(i) {
    list(file = manifest$file[i], md5 = manifest$md5[i])
  })

  info <- list(
    app = list(name = "M2ara", version = version_of("M2ara")),
    r = list(
      version = R.version.string,
      platform = R.version$platform,
      session_info = utils::capture.output(utils::sessionInfo())
    ),
    packages = list(
      MALDIcellassay = version_of("MALDIcellassay"),
      MALDIquant = version_of("MALDIquant"),
      # the fit engine decides what the metrics mean, so it is named here
      nplr = version_of("nplr")
    ),
    fit = list(
      engine = "nplr",
      parameters = "4 (5 as fallback, 2 for a flat line)",
      metrics = "pEC50 = -xmid, R2 = nplr::getGoodness()$gof"
    ),
    dataset = list(
      name = dataset,
      directory = exportDirectory(res),
      centroided = centroided,
      spectra = length(getAvgSpectra(res)),
      concentrations = getConc(res)
    ),
    settings = list(
      file = if (is.null(settings_file)) NA_character_ else settings_file
    ),
    samples = list(
      file = if (is.null(samples_file)) NA_character_ else samples_file,
      source = if (is.null(samples_source)) NA_character_ else samples_source,
      files = if (is.null(samples)) NA_integer_ else nrow(samples),
      concentrations = if (is.null(samples)) NA_integer_ else length(unique(samples$concentration)),
      spectra = if (is.null(samples)) NA_integer_ else sum(samples$n_spectra)
    ),
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    files = file_entries
  )

  jsonlite::toJSON(info, auto_unbox = TRUE, pretty = TRUE, na = "null")
}

# human readable documentation inside the archive ------------------------------
exportReadme <- function(res, stats, centroided, manifest, dataset,
                         samples_file = NULL) {
  created <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

  c(
    paste0("# M2ara export - ", dataset),
    "",
    paste0("Created: ", created),
    paste0("M2ara: ", tryCatch(as.character(utils::packageVersion("M2ara")),
                               error = function(e) "unknown")),
    paste0(
      "Data: ", if (isTRUE(centroided)) "centroided" else "profile",
      " spectra, ", length(getAvgSpectra(res)), " average spectra, ",
      length(getConc(res)), " concentrations"
    ),
    "",
    "## Contents",
    "",
    "| file | content |",
    "| --- | --- |",
    "| `settings.csv` | settings used for this run (same format as in the app) |",
    "| `peak_table.csv` | table from the Results tab, one row per m/z feature |",
    "| `fitting_parameters.csv` | fitted curve parameters per feature |",
    "| `intensity_matrix.csv` | intensities per concentration and m/z |",
    "| `run_info.json` | versions, session information and checksums of all files |",
    if (!is.null(samples_file)) {
      "| `sample_sheet.csv` | which file had which concentration and which spots |"
    },
    "",
    "## Columns of peak_table.csv",
    "",
    paste0("- `", names(stats), "`"),
    "",
    "## Reading the tables",
    "",
    "The CSV files are UTF-8 with `,` as separator and `.` as decimal mark,",
    "so they open directly in R, Python and Excel.",
    "",
    "## Reproducing this run",
    "",
    "1. Load the same data folder in M2ara.",
    "2. Use `settings.csv` from this archive (Settings tab) - it contains the",
    "   file format, normalization and preprocessing settings of this run.",
    "3. Press \"Process spectra\". The exported tables should match.",
    "",
    "## Citation",
    "",
    "M2ara: https://doi.org/10.1093/bioinformatics/btae694",
    "Code:  https://github.com/CeMOS-Mannheim/M2ara",
    ""
  )
}
