fitCurveErrorHandler <- function(res, input, info_state) {
  if (is.null(res)) {
    cat("Error: ")
    if (input$SinglePointRecal) {
      info_state("fitErrorRecal")
      cat("Wrong lock-mass?\n")
    } else if (input$normMeth == "mz") {
      info_state("fitErrorNorm")
      cat("Wrong normalization mass?\n")
    } else {
      info_state("fitErrorOther")
      cat("Unkown error on fitting the curves.\n")
    }

    hide_spinner()
    return(FALSE)
  }
  return(TRUE)
}
