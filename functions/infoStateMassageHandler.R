infoStateMassageHandler <- function(info_state, output) {
  if (info_state == "inital") {
    output$info1 <- renderText("Please select\na folder.")
    output$info2 <- renderText("")
    output$info3 <- renderText("")
  }
  if (info_state == "dir_set") {
    output$info1 <- renderText("Selected:")
    output$info2 <- renderText(appData$selected_dir)
    output$info3 <- renderText("Press load button.")
  }
  if (info_state == "loaded") {
    output$info1 <- renderText("Loaded:")
    output$info2 <- renderText(appData$selected_dir)
    output$info3 <- renderText("Press process button.")
  }
  if (info_state == "processed") {
    output$info1 <- renderText(("Dataset:"))
    output$info2 <- renderText(appData$selected_dir)
    output$info3 <- renderText("Changed settings apply after re-processing.")
  }
  if (info_state == "fitErrorRecal") {
    output$info1 <- renderText(("Dataset:"))
    output$info2 <- renderText(appData$selected_dir)
    output$info3 <- renderText("Could not be processed. Wrong lock-mass?")
  }
  if (info_state == "fitErrorNorm") {
    output$info1 <- renderText(("Dataset:"))
    output$info2 <- renderText(appData$selected_dir)
    output$info3 <- renderText("Could not be processed. Wrong normalization-mass?")
  }
  if (info_state == "RefMzError") {
    output$info1 <- renderText(("Dataset:"))
    output$info2 <- renderText(appData$selected_dir)
    output$info3 <- renderText("Could not be processed. Increase tolerance?")
  }
  if (info_state == "fitErrorOther") {
    output$info1 <- renderText(("Dataset:"))
    output$info2 <- renderText(appData$selected_dir)
    output$info3 <- renderText("Could not be processed. Unkown error.\n")
  }

  return(output)
}
