storeResults <- function(appData, res, input, stats) {
  appData$res <- res
  appData$preprocessing <- appData$preprocessing
  appData$stats_original <- stats # copy of original stats for updates
  appData$stats <- stats

  return(appData)
}
