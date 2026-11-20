# Data for the clustering -------------------------------------------------------
#
# The fitted curves are sampled and normalised here (fast, in the app), the
# clustering itself runs in a background process (clusterAsync.R).

# curves with a poor fit carry no usable shape, and curves without rise cannot be
# normalised at all. Both are dropped before the clustering and reported.
minClusterFitGoodness <- 0.8

# above this the distance matrix (n^2 doubles) becomes too large for the app;
# a MALDI dataset has a few hundred real signals, the rest is noise
maxClusterFeatures <- 2000L

curveClusterData <- function(res) {
  fits <- getCurveFits(res)
  if (length(fits) == 0) {
    return(NULL)
  }

  goodness <- vapply(fits, function(f) {
    g <- tryCatch(nplr::getGoodness(f$model), error = function(e) NULL)
    if (is.null(g) || is.null(g[["wgof"]])) NA_real_ else as.numeric(g[["wgof"]])
  }, numeric(1))

  ycurves <- lapply(fits, function(f) nplr::getYcurve(f$model))
  flat <- vapply(ycurves, function(y) diff(range(y, na.rm = TRUE)) < 1e-6, logical(1))
  poorFit <- !is.na(goodness) & goodness < minClusterFitGoodness

  keep <- !flat & !poorFit

  if (sum(keep) < 2L) {
    return(NULL)
  }

  # subsample smooth fitted curve values (200 pts) to ~50 pts
  l <- lapply(which(keep), function(i) {
    y <- ycurves[[i]]
    x <- nplr::getXcurve(fits[[i]]$model)

    # every 4th point gives ~50 pts for a smooth sigmoid
    sel <- seq(1, length(y), by = 4)

    tibble(y = y[sel], x = x[sel])
  })

  names(l) <- which(keep)

  curves <- l %>%
    bind_rows(.id = "mzIdx") %>%
    group_by(mzIdx) %>%
    mutate(y = (y - min(y, na.rm = TRUE)) /
             (max(y, na.rm = TRUE) - min(y, na.rm = TRUE))) %>%
    ungroup()

  attr(curves, "excludedFlat") <- sum(flat)
  attr(curves, "excludedFit") <- sum(poorFit)
  attr(curves, "features") <- sum(keep)

  curves
}

# the clustering itself (runs in the background process)
clusterCurvesFromData <- function(curves, nClusters = 10,
                                  centroidMethod = "mean",
                                  progress = function(step) NULL) {
  progress("curves")

  # precompute the SBD distance matrix once - all k values share the distances
  wide_mat <- curves %>%
    tidyr::pivot_wider(id_cols = mzIdx,
                       names_from = x,
                       values_from = y) %>%
    dplyr::arrange(as.numeric(mzIdx)) %>%
    dplyr::select(-mzIdx) %>%
    as.matrix()

  progress("distances")
  # the SBD distance is registered when dtwclust is loaded - in a fresh
  # background process it has not been loaded yet
  requireNamespace("dtwclust", quietly = TRUE)
  dm <- proxy::dist(wide_mat, method = "SBD")
  message(m2araTimeNow(), " distance matrix computed (",
          attr(dm, "Size"), " series).\n")

  method <- latrend::lcMethodDtwclust(response = "y",
                             time = "x",
                             id = "mzIdx",
                             distance = "sbd",
                             centroid = centroidMethod,
                             seed = 42,
                             control = dtwclust::partitional_control(distmat = dm))

  methods <- latrend::lcMethods(method, nClusters = 2:nClusters)

  progress("clustering")
  models <- tryCatch({
    latrend::latrendBatch(methods, data = curves, verbose = TRUE)
  }, error = function(e) {
    message("Full error: ", conditionMessage(e), "\n")
    return(NULL)
  })

  progress("done")
  models
}

# convenience: both steps in the current process (used by the tests)
clusterCurves <- function(res, nClusters = 10, centroidMethod = "mean") {
  message(m2araTimeNow(), " running clustering...\n")

  curves <- curveClusterData(res)
  if (is.null(curves)) {
    warning("No curve fits found. Cannot run clustering.")
    return(NULL)
  }

  models <- clusterCurvesFromData(curves, nClusters = nClusters,
                                  centroidMethod = centroidMethod)
  if (is.null(models)) {
    warning("Clustering failed.")
    return(NULL)
  }

  message(m2araTimeNow(), " clustering done (", length(models), " models).\n")
  models
}


extractLaClusters <- function(models, k = 2) {
  model <- models[[k-1]]

  df <- tibble(mzIdx = as.numeric(latrend::ids(model)),
               cluster = latrend::trajectoryAssignments(model))
  return(df)
}

# Fitted curves per cluster, the way latrend plots them (one facet per cluster).
#
# plotly does not clip traces to the facet panel and the default strip band lies
# at the top of the panel, so curves would be drawn over it. The strip is drawn
# without a background and inside the panel, so nothing can be hidden by it.
plotClusters <- function(models, k) {
  model <- models[[k - 1]]

  p <- latrend::plot(model) +
    labs(y = "rel. Intensity [arb. u.]",
         x = "Log10 Concentration",
         title = "Curves per cluster") +
    m2araTheme() +
    theme(strip.background = element_blank(),
          strip.text = element_text(hjust = 0, colour = "grey40"))

    ggplotly(p) %>% plotly::layout(margin = m2araPlotMargin())
}


plotTraj <- function(models, k) {
  model <- models[[k-1]]

  p <- latrend::plotClusterTrajectories(model) +
    labs(y = "rel. Intensity [arb. u.]",
         x = "Log10 Concentration",
         title = "Average Trajectories") +
    m2araTheme()

    ggplotly(p) %>% plotly::layout(margin = m2araPlotMargin())
}

plotClusterMetrics <- function(models) {
  nClust <- length(models) + 1  # models go from k=2..n
  p <- latrend::plotMetric(models, c("ASW", "Dunn", "DaviesBouldin", "WRSS")) +
    m2araTheme() +
    facet_wrap(~Metric,
               scales = "free_y",
               nrow = 1) +
    scale_x_continuous(breaks = seq(2, nClust, by = 2))

    ggplotly(p) %>% plotly::layout(margin = m2araPlotMargin())
}
# Which k do the metrics suggest?
#
# The silhouette (ASW) works with the SBD distances and is the most reliable
# index here; Dunn supports it. The suggestion is the k with the best ASW; when
# Dunn agrees, that is mentioned.
suggestClusters <- function(models) {
  ks <- seq_len(length(models)) + 1L  # models are k = 2..n

  asw <- tryCatch(as.numeric(latrend::metric(models, "ASW")),
                  error = function(e) rep(NA_real_, length(ks)))
  dunn <- tryCatch(as.numeric(latrend::metric(models, "Dunn")),
                   error = function(e) rep(NA_real_, length(ks)))

  if (length(asw) != length(ks) || all(is.na(asw))) {
    return(NULL)
  }

  bestAsW <- ks[which.max(asw)]
  bestDunn <- if (all(is.na(dunn))) NA_integer_ else ks[which.max(dunn)]

  list(k = bestAsW,
       asw = asw[which.max(asw)],
       dunnAgrees = identical(as.integer(bestAsW), as.integer(bestDunn)))
}

# text for the app
suggestClustersText <- function(models) {
  suggestion <- suggestClusters(models)
  if (is.null(suggestion)) {
    return("")
  }

  sprintf("Suggested k: %d (best silhouette at %.2f%s)",
          suggestion$k, suggestion$asw,
          if (isTRUE(suggestion$dunnAgrees)) ", Dunn agrees" else "")
}