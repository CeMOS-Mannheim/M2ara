# The dataset card in the sidebar ----------------------------------------------
#
# Shows which dataset is in the app without printing a long path: the parent
# folder (the actual dataset name), the last folder, the number of entries and a
# coloured state. The full path and the settings that belong to the shown result
# are behind "Details".

# the dataset as two lines: parent folder (the name that says something) and the
# last folder, which on its own is often just "curve" or "data"
datasetNameParts <- function(dir) {
  if (length(dir) != 1L || is.na(dir) || !nzchar(dir)) {
    return(list(parent = "", last = "", full = ""))
  }

  clean <- gsub("[/\\\\]+$", "", as.character(dir))
  parts <- strsplit(clean, "[/\\\\]+")[[1]]
  parts <- parts[nzchar(parts)]
  # a drive letter is not a folder
  parts <- parts[!grepl("^[A-Za-z]:$", parts)]

  if (length(parts) == 0L) {
    return(list(parent = "", last = "", full = clean))
  }

  # for a deep path the name of the dataset is the folder above the last one;
  # "C:/data/name" has no such parent, the name itself is the interesting part
  n <- length(parts)
  list(
    parent = if (n >= 3L) parts[n - 1L] else parts[n],
    last = if (n >= 3L) parts[n] else "",
    full = clean
  )
}

# how many entries are in the folder (cheap listing, nothing is read)
datasetEntryCount <- function(dir) {
  if (!hasUsableDir(dir)) {
    return(NA_integer_)
  }
  length(list.files(dir))
}

# state of the app as a coloured dot and a short text
datasetStatus <- function(appData, input, running = FALSE, loading = FALSE,
                          cached = NULL) {
  state <- appData$info_state

  if (isTRUE(loading)) {
    return(list(class = "pending", text = "Reading spectra ..."))
  }
  if (isTRUE(running)) {
    return(list(class = "pending", text = "Processing ..."))
  }

  if (state %in% c("loadErrorNum", "loadErrorFormat", "loadErrorUniqueMz",
                   "loadErrorMapping", "errorMapping")) {
    # the loader knows what exactly went wrong (e.g. which files have no
    # concentration), so say that instead of a bare "failed"
    reason <- appData$load_problem
    if (is.null(reason) || !nzchar(reason)) {
      reason <- "Last load failed"
    }
    return(list(class = "error", text = reason))
  }
  if (state %in% c("fitErrorRecal", "fitErrorNorm", "fitErrorOther")) {
    return(list(class = "error", text = "Last run failed"))
  }

  if (identical(state, "loaded")) {
    return(list(class = "pending", text = "Loaded - press Process spectra"))
  }

  if (identical(state, "processed")) {
    current <- list(settings = processingSettings(input),
                    preprocessing = appData$preprocessing,
                    concUnits = input$concUnits)
    if (!is.null(appData$run_signature) &&
        !identical(current, appData$run_signature)) {
      return(list(class = "changed", text = "Settings changed - press Re-process"))
    }
    return(list(class = "ok", text = "Up to date"))
  }

  if (identical(state, "dir_set")) {
    if (!is.null(cached[["created"]])) {
      return(list(class = "pending",
                  text = sprintf("Cached result from %s - press Process spectra",
                                 format(cached$created, "%Y-%m-%d %H:%M"))))
    }
    return(list(class = "pending", text = "Not read yet"))
  }

  list(class = "empty", text = "No dataset selected")
}

# the card
datasetCardUi <- function(appData, input, running = FALSE, loading = FALSE) {
  parts <- datasetNameParts(appData$selected_dir)
  cached <- if (is.null(appData$run_signature)) cacheLookup(appData, input) else NULL
  status <- datasetStatus(appData, input, running = running, loading = loading,
                          cached = cached)

  if (!nzchar(parts$full)) {
    return(tags$div(
      class = "m2ara-dataset m2ara-dataset-empty",
      tags$div(class = "m2ara-dataset-meta", "No dataset selected")
    ))
  }

  entries <- datasetEntryCount(appData$selected_dir)

  spectra <- length(appData$spec_all)
  features <- if (is.null(appData$stats)) 0L else nrow(appData$stats)

  facts <- c(
    if (!is.na(entries)) sprintf("%d files", entries),
    if (spectra > 0L) sprintf("%d spectra", spectra),
    if (features > 0L) sprintf("%d features", features)
  )

  # the label above says "Last run", so only the value is shown. A cached result
  # of this dataset counts as a result, even before the app has used it.
  runs <- if (is.null(appData$run_signature)) {
    if (is.null(cached$created)) {
      "Never."
    } else {
      sprintf("Cached result from %s - not used yet.", format(cached$created, "%Y-%m-%d %H:%M"))
    }
  } else if (is.null(appData$run_time)) {
    # a run was started but never produced a result (the status above says why)
    "Never."
  } else {
    format(appData$run_time, "%Y-%m-%d %H:%M")
  }

  facts <- paste(facts, collapse = " \u00b7 ")

  # what the folder contains and where the concentrations come from
  dataFormat <- detectDataFormat(appData$selected_dir)
  dataSource <- detectConcentrationSource(appData$selected_dir, dataFormat)
  formatText <- switch(
    ifelse(is.na(dataFormat), "none", dataFormat),
    "mzml" = "mzML",
    "bruker" = "Bruker flex",
    "both" = "mzML and Bruker",
    "unknown format"
  )
  # an assignment made in this session wins over what the folder shows
  assigned <- appData$mapping
  sourceText <- if (!is.null(assigned) && identical(assigned$kind, "sheet")) {
    sprintf("concentrations from your assignment (%d files)", nrow(assigned$sheet))
  } else {
    switch(
      dataSource$kind,
      "file names" = "concentrations from the file names",
      "sample sheet" = sprintf("concentrations from %s", dataSource$sheet),
      "missing" = sprintf("concentrations missing for %d file(s)",
                          length(dataSource$missing))
    )
  }
  dataClass <- if (identical(dataSource$kind, "missing")) {
    "m2ara-dataset-meta m2ara-dataset-warn"
  } else {
    "m2ara-dataset-meta"
  }

  # the card shows the last two folders (the path would be too long and is
  # repeated in Details anyway)
  tags$div(
    class = "m2ara-dataset",
    title = parts$full,
    tags$div(class = "m2ara-dataset-name", title = parts$full, parts$parent),
    if (nzchar(parts$last))
      tags$div(class = "m2ara-dataset-meta", title = parts$full, parts$last),
    tags$div(class = "m2ara-dataset-meta", title = facts, facts),
    tags$div(class = dataClass, title = paste(formatText, sourceText, sep = " - "),
             sprintf("%s \u00b7 %s", formatText, sourceText)),
    if (length(appData$excluded_files) > 0L) {
      tags$div(class = "m2ara-dataset-meta m2ara-dataset-warn",
               sprintf("%d file(s) left out", length(appData$excluded_files)))
    },
    tags$div(class = paste("m2ara-status", status$class),
             tags$span(class = "m2ara-status-dot"),
             status$text),
    tags$details(
      class = "m2ara-dataset-details",
      tags$summary("Details"),
      tags$div(class = "m2ara-dataset-label", "Folder"),
      tags$div(class = "m2ara-dataset-path", parts$full),
      tags$div(class = "m2ara-dataset-label", "Last run"),
      tags$div(class = "m2ara-dataset-note", runs)
    )
  )
}
# Is there a cached result for this dataset and these settings? Only the small
# sidecar file is read, not the result itself.
cacheLookup <- function(appData, input) {
  dir <- appData$selected_dir
  if (!hasUsableDir(dir)) {
    return(NULL)
  }

  key <- tryCatch(
    processCacheKey(dir, processingSettings(input), appData$preprocessing,
                    mapping = appData$mapping),
    error = function(e) NULL
  )
  if (is.null(key)) {
    return(NULL)
  }

  processCacheMeta(key, dir)
}
# TRUE when a usable folder is selected (the path can be NULL, character(0) or NA
# while the user changes it)
hasUsableDir <- function(dir) {
  length(dir) == 1L && !is.na(dir) && nzchar(dir) && dir.exists(dir)
}