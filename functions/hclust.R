clusterCurves <- function(res, nClusters = 15) {
  fits <- getCurveFits(res)

  df <- seq_along(fits) %>%
    map_df(function(i) {
      y <- getYcurve(fits[[i]]$model)
      x <- getXcurve(fits[[i]]$model)

      sel <- seq(1, length(y), by = 4)

      tibble(y = y[sel],
             x = x[sel])
    },
    .id = "mzIdx") %>%
    group_by(mzIdx) %>%
    mutate(y = (y - min(y, na.rm = TRUE)) /
             (max(y, na.rm = TRUE) - min(y, na.rm = TRUE))) %>%
    ungroup()

  method <- latrend::lcMethodDtwclust(response = "y",
                             time = "x",
                             id = "mzIdx",
                             distance = "dtw_basic")

  methods <- latrend::lcMethods(method,
                       nClusters = 2:nClusters)
  models <- latrend::latrendBatch(methods, data = df, verbose = FALSE)

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
  p <- latrend::plotMetric(models, c("Dunn", "ASW", "WMAE", "WRSS", "CalinskiHarabasz")) +
    facet_wrap(~Metric,
               scales = "free_y",
               nrow = 1) +
    scale_x_continuous(breaks = c(2, 5, 10, 15))
  p <- ggplotly(p)
  return(p)
}
