saveSettings <- function(input, filename = "settings.conf", info_state) {
  inputList <- reactiveValuesToList(input)

  # filter inputs
  classes <- vapply(inputList, function(x) class(x)[1], character(1))
  sel_class <- "mzTable|shiny"
  sel_name <-  "mzTable|shiny|plotly|dir"

  fil_inputList <- inputList[!grepl(sel_class, classes)]
  fil_inputList <- fil_inputList[!grepl(sel_name, names(fil_inputList))]

  dir <- as.character(parseDirPath(getVolumes(), input$dir))

  # add dir value
  if (info_state == "dir_set" & length(dir) > 0) {

    fil_inputList$dir <- as.character(parseDirPath(getVolumes(), input$dir))
  }

  write.csv(fil_inputList, file = filename, row.names = FALSE)
  cat("Inputs written to", file.path(getwd(), filename))
}
