selectDir <- function(appData, input, output = NULL, defaults = NULL, session = NULL) {
  vol <- getVolumes()
  userSelectedDir <- FALSE  # flag to skip initial trigger

  # check if "dir" is set in defaults ([[]] instead of $: the generated defaults
  # have no "dir" column, and $ would warn about an unknown column)
  if (!is.null(defaults[["dir"]])) {
    appData$selected_dir <- defaults[["dir"]]
    message("Dir set from loaded default value.\n")
    setInfoState(appData, "dir_set")
  }

  ### choose dir ####
  shinyDirChoose(input,
                 "dir",
                 roots = vol,
                 allowDirCreate = FALSE,
                 defaultRoot = names(vol)[1])

  observeEvent(input$dir, {
    # check if folder was selected
    # the shinyFiles dialog can return nothing, an empty string or NA (e.g. when
    # it is cancelled); the app must never store a path that cannot be used
    chosen <- parseDirPath(vol, input$dir)
    if (length(chosen) != 1L || is.na(chosen) || !nzchar(chosen)) {
      return()
    }

    appData$selected_dir <- chosen

    # the format is read from the folder, the user does not have to choose it
    detected <- detectDataFormat(chosen)
    if (identical(detected, "both")) {
      showNotification(
        paste("The folder contains mzML and Bruker data - using mzML.",
              "Move the files you do not want into another folder."),
        type = "warning", duration = 15)
      detected <- "mzml"
    }
    if (detected %in% c("mzml", "bruker") && !is.null(session)) {
      updateSelectInput(session, "fileFormat", selected = detected)
    }
    {
      # a different folder invalidates the loaded spectra and the results
      appData$spec_all <- NULL
      appData$spec_idx <- NULL
      appData$res <- NULL
      appData$stats <- NULL
      appData$stats_original <- NULL
      appData$show_plot <- FALSE
      setInfoState(appData, "dir_set")
      # show notification only on actual user interaction, not on init
      if (userSelectedDir && !is.null(output) && is.character(appData$selected_dir)) {
        msg <- paste0("Folder '", basename(appData$selected_dir), "' selected.")
        showNotification(msg, duration = 5, type = "message")
      }
      userSelectedDir <- TRUE
    }
    # ensure info text updates even if info_state doesn't change
    if (!is.null(output) && appData$info_state == "dir_set") {
      output$info1 <- renderText("Selected:")
      output$info2 <- renderText(appData$selected_dir)
      output$info3 <- renderText("Press the process button.")
    }
  })

  return(appData)
}
