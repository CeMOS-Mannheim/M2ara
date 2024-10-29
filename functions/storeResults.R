storeResults <- function(appData, res, input, stats) {
  appData$res <- res
  appData$preprocessing <- appData$preprocessing

  # rename Z', V', SSMD to FZ, FV and FS
  stats <- stats %>%
    rename("FZ" = `Z'`,
           "FV" = `V'`,
           "FS" = SSMD)


  appData$stats_original <- stats # copy of original stats for updates
  appData$stats <- stats

  appData$info_state <- "processed"
  appData$show_plot <- TRUE

  return(appData)
}
