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

  setInfoState(appData, "processed")
  appData$show_plot <- TRUE
  # only a result that reached this point was a run - a failed load must not
  # show up as "Last run"
  appData$run_time <- Sys.time()

  return(appData)
}
