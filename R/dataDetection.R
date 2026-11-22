# What is in a data folder? -----------------------------------------------------
#
# When a folder is selected the app knows nothing about its content. A plain
# directory listing is enough to tell which format the spectra have and where the
# concentrations come from, so the sidebar can say it before anything is loaded.

# "mzml", "bruker", "both" or NA when nothing was recognised
detectDataFormat <- function(dir) {
  if (!hasUsableDir(dir)) {
    return(NA_character_)
  }

  entries <- list.files(dir)
  hasMzml <- any(grepl("\\.mzml$", entries, ignore.case = TRUE))

  subdirs <- list.dirs(dir, recursive = FALSE, full.names = TRUE)
  hasBruker <- length(subdirs) > 0L &&
    any(vapply(subdirs, function(d) {
      any(tolower(list.files(d)) == "fid")
    }, logical(1)))

  if (hasMzml && hasBruker) {
    return("both")
  }
  if (hasMzml) {
    return("mzml")
  }
  if (hasBruker) {
    return("bruker")
  }
  NA_character_
}

# how the concentrations would be assigned for this folder
#
# kind: "file names" (the names are numbers), "sample sheet" (a sheet in the
# folder maps them) or "missing" (neither).
detectConcentrationSource <- function(dir, fileFormat = NA_character_) {
  empty <- list(kind = "missing", sheet = NULL, nFiles = 0L,
                missing = character(0), upload = NULL)

  if (!hasUsableDir(dir)) {
    return(empty)
  }

  format <- if (is.na(fileFormat)) detectDataFormat(dir) else fileFormat
  names <- if (identical(format, "bruker")) {
    basename(list.dirs(dir, recursive = FALSE, full.names = TRUE))
  } else {
    entries <- list.files(dir)
    stripDataExtension(entries[grepl("\\.mzml$", entries, ignore.case = TRUE)])
  }

  if (length(names) == 0L) {
    return(empty)
  }

  numeric <- suppressWarnings(as.numeric(names))
  missing <- names[is.na(numeric)]

  if (length(missing) == 0L) {
    return(list(kind = "file names", sheet = NULL, nFiles = length(names),
                missing = character(0), upload = NULL))
  }

  sheet <- findSampleSheet(dir)
  if (!is.null(sheet$mapping)) {
    return(list(kind = "sample sheet", sheet = sheet$file, nFiles = length(names),
                missing = character(0), upload = NULL))
  }

  list(kind = "missing", sheet = NULL, nFiles = length(names),
       missing = missing, upload = NULL)
}

# one line for the sidebar
describeConcentrations <- function(source) {
  switch(
    source$kind,
    "file names" = sprintf("Concentrations: from the file names (%d files)",
                           source$nFiles),
    "sample sheet" = sprintf("Concentrations: from %s (%d files)",
                             source$sheet, source$nFiles),
    "missing" = sprintf("Concentrations: missing for %d of %d files",
                        length(source$missing), source$nFiles)
  )
}
