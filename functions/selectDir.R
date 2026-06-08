selectDir <- function(appData, input, output = NULL) {
  vol <- getVolumes()
  userSelectedDir <- FALSE  # flag to skip initial trigger

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
      # show notification only on actual user interaction, not on init
      if (userSelectedDir && !is.null(output) && is.character(appData$selected_dir)) {
        msg <- paste0("Folder '", basename(appData$selected_dir), "' selected.")
        showNotification(msg, duration = 5, type = "default")
      }
      userSelectedDir <- TRUE
    }
    # ensure info text updates even if info_state doesn't change
    if (!is.null(output) && appData$info_state == "dir_set") {
      output$info1 <- renderText("Selected:")
      output$info2 <- renderText(appData$selected_dir)
      output$info3 <- renderText("Press load button.")
    }
  })

  return(appData)
}
