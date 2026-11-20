# Assigning concentrations by hand ------------------------------------------------
#
# When the file names are not numeric and no sample sheet is in the folder, the
# user can fill the concentrations in a table. The result is used as a sample
# sheet (the same format that is read from a file), so nothing else has to know
# where the assignment came from.

# the files of a folder with the concentration we know already
concentrationTable <- function(dir, fileFormat = NA_character_) {
  empty <- data.frame(file = character(0), concentration = numeric(0),
                      stringsAsFactors = FALSE)

  if (is.null(dir) || length(dir) == 0L || is.na(dir) || !dir.exists(dir)) {
    return(empty)
  }

  format <- if (is.na(fileFormat)) detectDataFormat(dir) else fileFormat

  files <- if (identical(format, "bruker")) {
    basename(list.dirs(dir, recursive = FALSE, full.names = TRUE))
  } else {
    entries <- list.files(dir)
    entries[grepl("\\.mzml$", entries, ignore.case = TRUE)]
  }

  if (length(files) == 0L) {
    return(empty)
  }

  # a sheet in the folder already knows more than the names
  sheet <- findSampleSheet(dir)
  if (!is.null(sheet$mapping) && identical(sheet$mapping$kind, "sheet")) {
    known <- sheet$mapping$sheet
    key <- function(x) tolower(stripDataExtension(basename(x)))
    match <- match(key(files), key(known$file))
    return(data.frame(
      file = files,
      concentration = as.numeric(known$concentration[match]),
      stringsAsFactors = FALSE
    ))
  }

  guessed <- suppressWarnings(as.numeric(stripDataExtension(files)))
  table <- data.frame(file = files, concentration = guessed,
                      stringsAsFactors = FALSE)
  table[order(is.na(table$concentration), table$concentration, table$file), ,
        drop = FALSE]
}

# can this table be used? NULL when it is fine, otherwise the reason
concentrationTableProblem <- function(table) {
  if (is.null(table) || nrow(table) == 0L) {
    return("The table is empty.")
  }
  if (anyNA(table$concentration)) {
    missing <- table$file[is.na(table$concentration)]
    return(sprintf("No concentration for %d file(s): %s",
                   length(missing),
                   paste(utils::head(missing, 5), collapse = ", ")))
  }
  if (!is.numeric(table$concentration) || any(!is.finite(table$concentration))) {
    return("The concentrations have to be numbers.")
  }
  NULL
}

# the sheet in the format that is read from a file
concentrationSheet <- function(table) {
  data.frame(file = table$file,
             concentration = as.numeric(table$concentration),
             stringsAsFactors = FALSE)
}

# the dialog
concentrationDialog <- function() {
  modalDialog(
    title = "Concentrations",
    size = "l",
    easyClose = FALSE,
    tags$p(style = "font-size: 90%; color: #555;",
           paste("One row per file. Double click a concentration to change it.",
                 "The assignment is used like a sample sheet.")),
    DT::DTOutput("concTable"),
    checkboxInput("concWriteSheet", "Write samples.csv into the folder", value = FALSE),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("applyConcentrations", "Use these concentrations",
                   icon = icon("check"))
    )
  )
}
