infoStateMassageHandler <- function(info_state, output) {
  switch(info_state,
         "inital" = {
           output$info1 <- renderText("Please select\na folder.")
           output$info2 <- renderText("")
           output$info3 <- renderText("")
         },
         "dir_set" = {
           output$info1 <- renderText("Selected:")
           output$info2 <- renderText(appData$selected_dir)
           output$info3 <- renderText("Press load button.")
           if(is.character(appData$selected_dir)) {
             msg <- paste0("Folder '", basename(appData$selected_dir), "' selected.")
             showNotification(msg, duration = 5, type = "default")
           }
         },
         "loaded" = {
           output$info1 <- renderText("Loaded:")
           output$info2 <- renderText(appData$selected_dir)
           output$info3 <- renderText("Press process button.")
           msg <- paste0("Dataset '", basename(appData$selected_dir), "' was sucessfully loaded.")
           showNotification(msg, duration = 5, type = "default")
         },
         "processed" = {
           output$info1 <- renderText(("Dataset:"))
           output$info2 <- renderText(appData$selected_dir)
           output$info3 <- renderText("Changed settings apply after re-processing.")
           msg <- paste0("Dataset '", basename(appData$selected_dir), "' was sucessfully processed")
           showNotification(msg, duration = 5, type = "default")
         },
         "fitErrorRecal" = {
           output$info1 <- renderText(("Dataset:"))
           output$info2 <- renderText(appData$selected_dir)
           output$info3 <- renderText("Could not be processed. Wrong lock-mass?")
           msg <- paste0("'", basename(appData$selected_dir), "' could not be processed. Wrong lock-mass?")
           showNotification(msg, duration = 15, type = "error")
         },
         "fitErrorNorm" = {
           output$info1 <- renderText(("Dataset:"))
           output$info2 <- renderText(appData$selected_dir)
           output$info3 <- renderText("Could not be processed. Wrong normalization-mass?")
           msg <- paste0("'", basename(appData$selected_dir), "' could not be processed. Wrong normalization-mass?")
           showNotification(msg, duration = 15, type = "error")
         },
         "RefMzError" = {
           output$info1 <- renderText(("Dataset:"))
           output$info2 <- renderText(appData$selected_dir)
           output$info3 <- renderText("Could not be processed. Increase tolerance?")
           msg <- paste0("'", basename(appData$selected_dir), "' could not be processed. Increase tolerance?")
           showNotification(msg, duration = 15, type = "error")
         },
         "fitErrorOther" = {
           output$info1 <- renderText(("Dataset:"))
           output$info2 <- renderText(appData$selected_dir)
           output$info3 <- renderText("Could not be processed. Unknown error.")
           msg <- paste0("'", basename(appData$selected_dir), "' could not be processed. Unknown error")
           showNotification(msg, duration = 15, type = "error")
         },
         "loadErrorNum" = {
           output$info1 <- renderText(("Failed to load:"))
           output$info2 <- renderText(appData$selected_dir)
           output$info3 <- renderText("All folders need to have concentrations as names.")
           msg <- paste0("Faild to load '", basename(appData$selected_dir), "'. All folders need to have concentrations as names.")
           showNotification(msg, duration = 15, type = "error")
         },
         "loadErrorFormat" = {
           output$info1 <- renderText(("Failed to load:"))
           output$info2 <- renderText(appData$selected_dir)
           output$info3 <- renderText("Wrong file format.")
           msg <- paste0("Faild to load '", basename(appData$selected_dir), "'. Wrong file format.")
           showNotification(msg, duration = 15, type = "error")
         },
         "loadErrorMapping" = {
           output$info1 <- renderText(("Failed to load:"))
           output$info2 <- renderText(appData$selected_dir)
           output$info3 <- renderText("Mapping error.")
           msg <- paste0("Faild to load '", basename(appData$selected_dir), "'. Mapping file error.\nNumber conc. != Number spectra.")
           showNotification(msg, duration = 15, type = "error")
         },
         "errorMapping" = {
           output$info1 <- renderText(("Failed to load:"))
           output$info2 <- renderText(appData$selected_dir)
           output$info3 <- renderText("Mapping error.")
           msg <- paste0("Mapping file error. No concentrations found.")
           showNotification(msg, duration = 15, type = "error")
         }
  )

  return(output)
}
