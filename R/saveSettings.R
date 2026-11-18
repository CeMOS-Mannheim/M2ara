# the settings as a data frame (one row); pure function so it can also run in
# a download handler or a background process
settingsDataFrame <- function(input) {
  inputList <- shiny::reactiveValuesToList(input)

  # filter inputs
  classes <- vapply(inputList, function(x) class(x)[1], character(1))
  sel_class <- "mzTable|shiny"
  sel_name <-  "mzTable|shiny|plotly|dir|preproc_settings|mapping|m2ara"

  fil_inputList <- inputList[!grepl(sel_class, classes)]
  fil_inputList <- fil_inputList[!grepl(sel_name, names(fil_inputList))]

  fil_inputList$smooth <-
    handlePreprocSettings(input$preproc_settings,
                          "smooth")

  fil_inputList$rmBl <-
    handlePreprocSettings(input$preproc_settings,
                          "rmBl")

  fil_inputList$sqrtTrans <-
    handlePreprocSettings(input$preproc_settings,
                          "sqrtTrans")

  fil_inputList$monoisotopicFilter <-
    handlePreprocSettings(input$preproc_settings,
                          "monoisotopicFilter")

  # add dir value
  if (!is.null(input$dir)) {
    dir_path <- as.character(parseDirPath(getVolumes(), input$dir))
    if(length(dir_path) > 0) {
      fil_inputList$dir <- dir_path
    }
  }

  fil_inputList
}

saveSettings <- function(input, filename = "settings.csv", info_state = NULL) {
  settings <- settingsDataFrame(input)

  tryCatch({
    utils::write.csv(settings, file = filename, row.names = FALSE)
    message("Inputs written to ", file.path(getwd(), filename))
  }, error = function(e) {
    warning("Failed to save settings: ", conditionMessage(e))
  })

  invisible(filename)
}