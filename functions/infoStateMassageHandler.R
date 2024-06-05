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
    },
    "loaded" = {
      output$info1 <- renderText("Loaded:")
      output$info2 <- renderText(appData$selected_dir)
      output$info3 <- renderText("Press process button.")
    },
    "processed" = {
      output$info1 <- renderText(("Dataset:"))
      output$info2 <- renderText(appData$selected_dir)
      output$info3 <- renderText("Changed settings apply after re-processing.")
    },
    "fitErrorRecal" = {
      output$info1 <- renderText(("Dataset:"))
      output$info2 <- renderText(appData$selected_dir)
      output$info3 <- renderText("Could not be processed. Wrong lock-mass?")
    },
    "fitErrorNorm" = {
      output$info1 <- renderText(("Dataset:"))
      output$info2 <- renderText(appData$selected_dir)
      output$info3 <- renderText("Could not be processed. Wrong normalization-mass?")
    },
    "RefMzError" = {
      output$info1 <- renderText(("Dataset:"))
      output$info2 <- renderText(appData$selected_dir)
      output$info3 <- renderText("Could not be processed. Increase tolerance?")
    },
    "fitErrorOther" = {
      output$info1 <- renderText(("Dataset:"))
      output$info2 <- renderText(appData$selected_dir)
      output$info3 <- renderText("Could not be processed. Unkown error.")
    },
    "loadErrorNum" = {
      output$info1 <- renderText(("Failed to load:"))
      output$info2 <- renderText(appData$selected_dir)
      output$info3 <- renderText("All folders need to have concentrations as names.")
    },
    "loadErrorFormat" = {
      output$info1 <- renderText(("Failed to load:"))
      output$info2 <- renderText(appData$selected_dir)
      output$info3 <- renderText("Wrong file format.")
    }
  )

  return(output)
}
