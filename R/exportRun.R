# Export of a processed run ----------------------------------------------------
#
# One ZIP archive with everything that is needed to reuse and reproduce the
# results outside M2ara: the three result tables, the settings that were used, a
# machine readable run_info.json and an overview figure of the top ranked
# features. The archive layout is documented in README.md inside the archive.

exportRun <- function(file, res, stats, settings = NULL, centroided = FALSE,
                      topN = 9, plot_ready = FALSE) {
  if (!isTRUE(plot_ready)) {
    warning("Nothing to export. Load and process data.")
    return(invisible(NULL))
  }

  dataset <- exportDatasetName(res)
  root <- tempfile(pattern = paste0("m2ara_", dataset, "_"))
  dir.create(file.path(root, "plots"), recursive = TRUE)

  exported <- exportTables(res, stats, root)

  settings_file <- exportSettings(settings, root)
  if (!is.null(settings_file)) {
    exported <- c(exported, settings_file)
  }

  exported <- c(exported, exportOverviewPlots(res, stats, centroided, topN, root))

  manifest <- data.frame(
    file = exported,
    md5 = unname(tools::md5sum(file.path(root, exported))),
    stringsAsFactors = FALSE
  )

  writeLines(
    runInfoJson(res, centroided, settings_file, manifest, dataset),
    file.path(root, "run_info.json")
  )
  writeLines(
    exportReadme(res, stats, centroided, manifest, dataset),
    file.path(root, "README.md")
  )

  files <- list.files(root, recursive = TRUE, all.files = FALSE)
  files <- files[!dir.exists(file.path(root, files))]

  # mode = "mirror" keeps the directory structure (plots/...) inside the archive
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
exportSettings <- function(settings, root) {
  if (is.null(settings) || length(settings) == 0L || !file.exists(settings)) {
    return(NULL)
  }
  file.copy(settings, file.path(root, "settings.csv"), overwrite = TRUE)
  "settings.csv"
}

# overview figure of the top ranked features -----------------------------------
exportOverviewPlots <- function(res, stats, centroided, topN, root) {
  if (!"CRS" %in% names(stats) || nrow(stats) == 0L) {
    return(character())
  }

  ranked <- order(stats$CRS, decreasing = TRUE, na.last = TRUE)
  ranked <- ranked[seq_len(min(topN, length(ranked)))]
  if (length(ranked) == 0L) {
    return(character())
  }

  feature_title <- function(mzIdx) {
    ggplot2::labs(title = paste0("m/z ", round(getMzFromMzIdx(res, mzIdx), 2)))
  }

  curves <- lapply(ranked, function(mzIdx) {
    plotCurves(res, mzIdx = mzIdx, errorbars = "none") + feature_title(mzIdx)
  })

  peaks <- lapply(ranked, function(mzIdx) {
    plotPeak(res, mzIdx = mzIdx, tol = 0.8, centroided = centroided,
             showPeaks = TRUE) +
      feature_title(mzIdx)
  })

  ggplot2::ggsave(
    file.path(root, "plots", "curves_top.png"),
    plot = patchwork::wrap_plots(curves, ncol = 3, guides = "collect"),
    device = "png", bg = "white", dpi = 300,
    width = 30, height = 24, units = "cm"
  )
  ggplot2::ggsave(
    file.path(root, "plots", "peaks_top.png"),
    plot = patchwork::wrap_plots(peaks, ncol = 3, guides = "collect"),
    device = "png", bg = "white", dpi = 300,
    width = 30, height = 24, units = "cm"
  )

  c("plots/curves_top.png", "plots/peaks_top.png")
}

# machine readable run information ---------------------------------------------
runInfoJson <- function(res, centroided, settings_file, manifest, dataset) {
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
      MALDIquant = version_of("MALDIquant")
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
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    files = file_entries
  )

  jsonlite::toJSON(info, auto_unbox = TRUE, pretty = TRUE, na = "null")
}

# human readable documentation inside the archive ------------------------------
exportReadme <- function(res, stats, centroided, manifest, dataset) {
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
    "| `plots/curves_top.png` | concentration-response curves of the top ranked features |",
    "| `plots/peaks_top.png` | corresponding peak plots |",
    "| `run_info.json` | versions, session information and checksums of all files |",
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
    "3. Press \"Process spectra\". The exported tables and figures should match.",
    "",
    "## Citation",
    "",
    "M2ara: https://doi.org/10.1093/bioinformatics/btae694",
    "Code:  https://github.com/CeMOS-Mannheim/M2ara",
    ""
  )
}
