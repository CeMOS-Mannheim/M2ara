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

  used <- if ("use" %in% names(table)) table$use else rep(TRUE, nrow(table))
  if (!any(used)) {
    return("No file is left in the analysis.")
  }

  table <- table[used, , drop = FALSE]
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

# the dialog: one visible input per file, which is clearer than an editable table
concentrationDialog <- function(table) {
  rows <- lapply(seq_len(nrow(table)), function(i) {
    div(
      class = "m2ara-conc-row",
      div(class = "m2ara-conc-use",
          checkboxInput(inputId = paste0("concUse", i), label = NULL, value = TRUE)),
      tags$span(class = "m2ara-conc-file", title = table$file[i], table$file[i]),
      div(class = "m2ara-conc-input",
          numericInput(inputId = paste0("concValue", i),
                       label = NULL,
                       value = table$concentration[i],
                       step = 0.001,
                       width = "100%"))
    )
  })

  modalDialog(
    title = "Concentrations",
    size = "l",
    easyClose = FALSE,
    tags$p(style = "font-size: 90%; color: #555;",
           paste("Type the concentration of every file. Where the name is a",
                 "number it is already filled in. Untick a file to leave it out",
                 "of the analysis.")),
    div(class = "m2ara-conc-list", rows),
    checkboxInput("concWriteSheet",
                  "Save as samples.csv in the data folder (remembered next time)",
                  value = FALSE),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("applyConcentrations", "Use these concentrations",
                   icon = icon("check"))
    )
  )
}

# the table from the inputs of the dialog
concentrationDialogTable <- function(table, input) {
  values <- vapply(seq_len(nrow(table)), function(i) {
    value <- input[[paste0("concValue", i)]]
    if (is.null(value)) NA_real_ else suppressWarnings(as.numeric(value))
  }, numeric(1))

  used <- vapply(seq_len(nrow(table)), function(i) {
    value <- input[[paste0("concUse", i)]]
    if (is.null(value)) TRUE else isTRUE(value)
  }, logical(1))

  table$concentration <- values
  table$use <- used
  table
}

# files that were switched off
excludedFiles <- function(table) {
  if (is.null(table) || !"use" %in% names(table)) {
    return(character(0))
  }
  as.character(table$file[!table$use])
}

# the sheet of the files that stay in
includedSheet <- function(table) {
  keep <- if ("use" %in% names(table)) table$use else rep(TRUE, nrow(table))
  concentrationSheet(table[keep, , drop = FALSE])
}
