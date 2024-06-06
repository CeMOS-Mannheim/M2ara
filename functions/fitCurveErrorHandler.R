fitCurveErrorHandler <- function(appData, prc, input) {
  if (is.null(prc)) {
    cat("Error: ")
    if (input$SinglePointRecal) {
      appData$info_state <- "fitErrorRecal"
      cat("Wrong lock-mass?\n")

    } else if (input$normMeth == "mz") {
      appData$info_state <- "fitErrorNorm"
      cat("Wrong normalization mass?\n")

    } else {
      appData$info_state <- "fitErrorOther"
      cat("Unkown error on fitting the curves.\n")
    }

    hide_spinner()
    return(FALSE)
  }
  return(TRUE)
}
