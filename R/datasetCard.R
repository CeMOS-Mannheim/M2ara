# The dataset card in the sidebar ----------------------------------------------
#
# Shows which dataset is in the app without printing a long path: the parent
# folder (the actual dataset name), the last folder, the number of entries and a
# coloured state. The full path and the settings that belong to the shown result
# are behind "Details".

# the dataset as two lines: parent folder (the name that says something) and the
# last folder, which on its own is often just "curve" or "data"
datasetNameParts <- function(dir) {
  if (is.null(dir) || length(dir) == 0L || !nzchar(dir)) {
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
  if (is.null(dir) || length(dir) == 0L || !dir.exists(dir)) {
    return(NA_integer_)
  }
  length(list.files(dir))
}

# state of the app as a coloured dot and a short text
datasetStatus <- function(appData, input) {
  state <- appData$info_state

  if (state %in% c("loadErrorNum", "loadErrorFormat", "loadErrorUniqueMz",
                   "loadErrorMapping", "errorMapping")) {
    return(list(class = "error", text = "Last load failed"))
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
    return(list(class = "pending", text = "Not read yet"))
  }

  list(class = "empty", text = "No dataset selected")
}

# the card
datasetCardUi <- function(appData, input) {
  parts <- datasetNameParts(appData$selected_dir)
  status <- datasetStatus(appData, input)

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

  runs <- if (is.null(appData$run_signature)) {
    "No run yet."
  } else if (is.null(appData$run_time)) {
    "The shown result was computed with the settings in the form."
  } else {
    sprintf("Last run: %s.", format(appData$run_time, "%Y-%m-%d %H:%M"))
  }

  facts <- paste(facts, collapse = " \u00b7 ")

  # the path is cut on its left side, so its end (the dataset) always stays
  # visible, however long the path is
  tags$div(
    class = "m2ara-dataset",
    title = parts$full,
    tags$div(class = "m2ara-path", dir = "rtl",
             tags$span(dir = "ltr", parts$full)),
    tags$div(class = "m2ara-dataset-meta", title = facts, facts),
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
