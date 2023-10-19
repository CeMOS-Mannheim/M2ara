storeResults <- function(appData, res, input, stats) {
  appData$res <- res
  appData$preprocessing <- data.frame(
    smooth = input$smooth,
    rmBl = input$rmBl,
    sqrtTrans = input$sqrtTrans,
    monoisotopicFilter = input$monoisotopicFilter)
  appData$stats_original <- stats # copy of original stats for updates
  appData$stats <- stats

  return(appData)
}
