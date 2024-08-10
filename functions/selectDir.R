selectDir <- function(appData, input) {
  vol <- getVolumes()

  # check if "dir" is set in defaults
  if (!is.null(defaults$dir)) {
    appData$selected_dir <- defaults$dir
    message("Dir set from loaded default value.\n")
    appData$info_state <- "dir_set"
  }

  ### choose dir ####
  shinyDirChoose(input,
                 "dir",
                 roots = vol,
                 allowDirCreate = FALSE,
                 defaultRoot = names(vol)[1])

  observeEvent(input$dir, {
    # check if folder was selected
    # prepare info massage
    appData$selected_dir <- parseDirPath(vol, input$dir)
    if (length(appData$selected_dir) > 0) {
      appData$info_state <- "dir_set"
    }
  })

  return(appData)
}
