clusterCurves <- function(res, nClusters = 10, centroidMethod = "mean") {
  message(MALDIcellassay:::timeNow(), " running clustering...\n")

  fits <- getCurveFits(res)
  if (length(fits) == 0) {
    warning("No curve fits found. Cannot run clustering.")
    return(NULL)
  }

  # subsample smooth fitted curve values (200 pts) to ~100 pts
  l <-
    seq_along(fits) %>%
    lapply(function(i) {
      y <- nplr::getYcurve(fits[[i]]$model)
      x <- nplr::getXcurve(fits[[i]]$model)

      # every 8th point gives ~50 pts for a smooth sigmoid
      sel <- seq(1, length(y), by = 4)

      tibble(y = y[sel],
             x = x[sel])
    })

  names(l) <- seq_along(fits)

  df <-
    l %>%
    bind_rows(.id = "mzIdx") %>%
    group_by(mzIdx) %>%
    mutate(y = (y - min(y, na.rm = TRUE)) /
             (max(y, na.rm = TRUE) - min(y, na.rm = TRUE))) %>%
    ungroup()

  # precompute the SBD distance matrix once — all k values share the same distances
  wide_mat <- df %>%
    tidyr::pivot_wider(id_cols = mzIdx,
                       names_from = x,
                       values_from = y) %>%
    dplyr::arrange(as.numeric(mzIdx)) %>%
    dplyr::select(-mzIdx) %>%
    as.matrix()

  dm <- proxy::dist(wide_mat, method = "SBD")
  message(MALDIcellassay:::timeNow(), " distance matrix computed (",
          attr(dm, "Size"), " series).\n")

  method <- latrend::lcMethodDtwclust(response = "y",
                             time = "x",
                             id = "mzIdx",
                             distance = "sbd",
                             centroid = centroidMethod,
                             seed = 42,
                             control = dtwclust::partitional_control(distmat = dm))

  methods <- latrend::lcMethods(method,
                       nClusters = 2:nClusters)

  models <- tryCatch({
    latrend::latrendBatch(
      methods, 
      data = df,
      verbose = TRUE)
  }, error = function(e) {
    msg <- conditionMessage(e)
    warning("Clustering failed: ", msg)
    message("Full error: ", msg, "\n")
    return(NULL)
  })

  nMod <- if (is.list(models)) length(models) else 0
  message(MALDIcellassay:::timeNow(), " clustering done (", nMod, " models).\n")
  return(models)
}

extractLaClusters <- function(models, k = 2) {
  model <- models[[k-1]]

  df <- tibble(mzIdx = as.numeric(latrend::ids(model)),
               cluster = latrend::trajectoryAssignments(model))
  return(df)
}

plotClusters <- function(models, k) {
  model <- models[[k-1]]

  p <- latrend::plot(model) +
    labs(y = "rel. Intensity [arb. u.]",
         x = "Log10 Concentration",
         title = NULL)
  p <- ggplotly(p)
  return(p)
}

plotTraj <- function(models, k) {
  model <- models[[k-1]]

  p <- latrend::plotClusterTrajectories(model) +
    labs(y = "rel. Intensity [arb. u.]",
         x = "Log10 Concentration",
         title = "Average Trajectories")
  p <- ggplotly(p)

  return(p)
}

plotClusterMetrics <- function(models) {
  nClust <- length(models) + 1  # models go from k=2..n
  p <- latrend::plotMetric(models, c("Dunn", "ASW", "WMAE", "WRSS", "CalinskiHarabasz")) +
    facet_wrap(~Metric,
               scales = "free_y",
               nrow = 1) +
    scale_x_continuous(breaks = seq(2, nClust, by = 2))
  p <- ggplotly(p)
  return(p)
}
