# Concentration mapping ---------------------------------------------------------
#
# By default the concentration of a file is its name. When the names are not
# concentrations, a sample sheet maps file names to concentrations:
#
#   file,concentration,spot,group
#   0.0032,0.0032,K11;L11;M11;N11,
#
# One row per file; every spectrum of that file (i.e. every replicate) inherits
# the concentration. The legacy format (one concentration per line, applied in
# file order) is still accepted and reported as such.

# remove a data file extension, but keep names like "0.0032" intact
stripDataExtension <- function(x) {
  sub("\\.(mzml|mzxml|fid|txt|raw|tsf|d)$", "", x, ignore.case = TRUE)
}

# read a sample sheet (CSV/TSV) or the legacy list of concentrations
processMappingFile <- function(file) {
  if (is.null(file) || length(file) == 0L) {
    return(NULL)
  }

  path <- file$datapath
  ext <- tolower(tools::file_ext(path))

  if (!ext %in% c("txt", "csv", "tsv")) {
    stop("A sample sheet has to be a .csv, .tsv or .txt file.", call. = FALSE)
  }

  lines <- tryCatch(readr::read_lines(path), error = function(e) NULL)
  if (is.null(lines) || length(lines) == 0L) {
    stop("The sample sheet is empty.", call. = FALSE)
  }

  # legacy format: one concentration per line, nothing else
  numbers <- suppressWarnings(as.numeric(trimws(lines)))
  if (!anyNA(numbers)) {
    return(list(kind = "list", concentrations = numbers))
  }

  # table format, separated by comma, semicolon or tab
  sheet <- tryCatch(
    readr::read_delim(
      path,
      delim = if (ext == "tsv") "\t" else NULL,
      show_col_types = FALSE,
      trim_ws = TRUE,
      progress = FALSE
    ),
    error = function(e) NULL
  )

  if (!is.null(sheet) && nrow(sheet) == 0L) {
    stop("The sample sheet has no rows.", call. = FALSE)
  }

  if (is.null(sheet) || ncol(sheet) < 2L) {
    stop(paste0("The sample sheet needs two columns (file, concentration) or one ",
                "concentration per line."), call. = FALSE)
  }

  names(sheet) <- tolower(names(sheet))
  if (!all(c("file", "concentration") %in% names(sheet))) {
    stop("The sample sheet needs the columns 'file' and 'concentration'.",
         call. = FALSE)
  }

  sheet$file <- trimws(sheet$file)
  sheet$concentration <- suppressWarnings(as.numeric(sheet$concentration))

  bad <- which(is.na(sheet$concentration) | is.na(sheet$file) | !nzchar(sheet$file))
  if (length(bad) > 0L) {
    stop(sprintf("Row(s) %s of the sample sheet have no valid file name or concentration.",
                 paste(bad + 1L, collapse = ", ")), call. = FALSE)
  }

  if (anyDuplicated(sheet$file)) {
    stop(sprintf("The sample sheet lists these files more than once: %s.",
                 paste(unique(sheet$file[duplicated(sheet$file)]), collapse = ", ")),
         call. = FALSE)
  }

  list(kind = "sheet", sheet = as.data.frame(sheet))
}

# concentration of every spectrum from the mapping
#
# `files` are the file names of the spectra (without extension), so every
# spectrum of a file gets the same concentration. Returns NULL when the mapping
# does not fit the data; the reason is stored in the attribute "problem".
resolveMapping <- function(mapping, files) {
  if (is.null(mapping) || length(mapping) == 0L) {
    return(NULL)
  }

  normalize <- function(x) tolower(stripDataExtension(basename(x)))
  files <- as.character(files)
  keys <- normalize(files)
  uniqueKeys <- unique(keys)

  if (identical(mapping$kind, "list")) {
    concentrations <- mapping$concentrations
    if (length(concentrations) != length(uniqueKeys)) {
      out <- numeric(0)
      attr(out, "problem") <- sprintf(
        paste0("The concentration list has %d entries, but the folder contains %d ",
               "file(s). Use a sample sheet with 'file' and 'concentration' ",
               "columns instead of one concentration per line."),
        length(concentrations), length(uniqueKeys))
      return(out)
    }

    byFile <- stats::setNames(concentrations, sort(uniqueKeys))
    out <- unname(byFile[keys])
    attr(out, "problem") <- NULL
    return(out)
  }

  sheet <- mapping$sheet
  keysInSheet <- normalize(sheet$file)

  missingFiles <- setdiff(uniqueKeys, keysInSheet)
  if (length(missingFiles) > 0L) {
    out <- numeric(0)
    attr(out, "problem") <- sprintf(
      "The sample sheet has no concentration for these files: %s.",
      paste(missingFiles, collapse = ", "))
    return(out)
  }

  unknownFiles <- setdiff(keysInSheet, uniqueKeys)

  byFile <- stats::setNames(sheet$concentration, keysInSheet)
  out <- unname(byFile[keys])
  attr(out, "problem") <- NULL
  attr(out, "unknown") <- unknownFiles
  return(out)
}

# effective assignment of concentrations to files, as exported with the results
sampleSheet <- function(files, concentrations, spots = NULL, groups = NULL) {
  files <- as.character(files)
  hasSpots <- !is.null(spots)
  hasGroups <- !is.null(groups)

  # the exported sheet keeps the full file name (with extension), so a file can
  # be found again from the archive
  df <- tibble::tibble(file = basename(files),
                       concentration = as.numeric(concentrations),
                       spot = if (hasSpots) as.character(spots) else NA_character_,
                       group = if (hasGroups) as.character(groups) else NA_character_)

  out <- df %>%
    dplyr::group_by(.data$file, .data$concentration) %>%
    dplyr::summarise(
      n_spectra = dplyr::n(),
      spots = paste(unique(stats::na.omit(.data$spot[nzchar(.data$spot)])), collapse = ";"),
      group = paste(unique(stats::na.omit(.data$group[nzchar(.data$group)])), collapse = ";"),
      .groups = "drop"
    ) %>%
    dplyr::mutate(spots = dplyr::na_if(.data$spots, ""),
                  group = dplyr::na_if(.data$group, ""))

  if (!hasSpots) {
    out$spots <- NULL
  }
  if (!hasGroups) {
    out$group <- NULL
  }

  dplyr::arrange(out, .data$concentration, .data$file)
}
# sample sheet that is placed in the data folder (used when none was uploaded)
findSampleSheet <- function(dir) {
  if (is.null(dir) || length(dir) == 0L || !dir.exists(dir)) {
    return(list(mapping = NULL, error = NULL, file = NULL))
  }

  candidates <- list.files(dir, pattern = "^(samples|sample_sheet|metadata)\\.(csv|tsv|txt)$",
                           ignore.case = TRUE, full.names = TRUE)

  if (length(candidates) == 0L) {
    return(list(mapping = NULL, error = NULL, file = NULL))
  }

  file <- candidates[1]
  mapping <- tryCatch(processMappingFile(list(datapath = file)),
                      error = function(e) e)

  if (inherits(mapping, "error")) {
    return(list(mapping = NULL, error = conditionMessage(mapping), file = basename(file)))
  }

  list(mapping = mapping, error = NULL, file = basename(file))
}
# the loader only knows the file names without their extension (the spectra are
# named after the file). For the exported sheet the file on disk is restored, so
# it can be found again.
sheetFiles <- function(specNames, dir, fileFormat = "mzml") {
  specNames <- as.character(specNames)

  if (identical(fileFormat, "bruker")) {
    return(basename(specNames))
  }

  onDisk <- list.files(dir, pattern = "[.]mzml$", ignore.case = TRUE)
  if (length(onDisk) == 0L) {
    return(basename(specNames))
  }

  lookup <- stats::setNames(onDisk, tolower(stripDataExtension(onDisk)))
  mapped <- unname(lookup[tolower(stripDataExtension(specNames))])

  out <- basename(specNames)
  found <- !is.na(mapped)
  out[found] <- mapped[found]
  out
}