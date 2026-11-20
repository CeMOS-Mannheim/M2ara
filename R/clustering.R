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

  # one call per k, so the app can show how many of them are done
  ks <- 2:nClusters
  models <- tryCatch({
    fits <- lapply(seq_along(ks), function(i) {
      progress(sprintf("clustering %d/%d (k = %d)", i, length(ks), ks[i]))
      latrend::latrend(methods[[i]], data = curves, verbose = FALSE)
    })
    latrend::as.lcModels(fits)
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

  ggplotly(normaliseCentroids(p)) %>% plotly::layout(margin = m2araPlotMargin())
}


plotTraj <- function(models, k) {
  model <- models[[k-1]]

  p <- latrend::plotClusterTrajectories(model) +
    labs(y = "rel. Intensity [arb. u.]",
         x = "Log10 Concentration",
         title = "Average Trajectories") +
    m2araTheme()

  ggplotly(normaliseCentroids(p)) %>% plotly::layout(margin = m2araPlotMargin())
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
# Three indices point to a k: the silhouette (ASW, larger is better), Dunn
# (larger is better) and Davies-Bouldin (smaller is better). All three work with
# the SBD distances. The suggestion is the k the most of them agree on, with the
# silhouette breaking a tie - it is the most reliable of the three. WRSS is not
# used, it decreases with every cluster and would always vote for the maximum k.
suggestClusters <- function(models) {
  ks <- seq_len(length(models)) + 1L  # models are k = 2..n

  metricOf <- function(name) {
    tryCatch(as.numeric(latrend::metric(models, name)),
             error = function(e) rep(NA_real_, length(ks)))
  }

  asw <- metricOf("ASW")
  dunn <- metricOf("Dunn")
  db <- metricOf("DaviesBouldin")

  if (length(asw) != length(ks) || all(is.na(asw))) {
    return(NULL)
  }

  bestOf <- function(values, better = c("max", "min")) {
    if (length(values) != length(ks) || all(is.na(values))) {
      return(NA_integer_)
    }
    if (match.arg(better) == "max") ks[which.max(values)] else ks[which.min(values)]
  }

  votes <- c(asw = bestOf(asw, "max"),
             dunn = bestOf(dunn, "max"),
             db = bestOf(db, "min"))
  votes <- votes[!is.na(votes)]

  counts <- table(votes)
  top <- names(counts)[counts == max(counts)]

  k <- if (length(top) == 1L) {
    as.integer(top)
  } else if (as.character(votes[["asw"]]) %in% top) {
    votes[["asw"]]
  } else {
    as.integer(top[1L])
  }

  agreeing <- names(votes)[votes == k]
  list(k = k,
       asw = asw[ks == k],
       agree = agreeing,
       best = votes)
}

# text for the app
suggestClustersText <- function(models) {
  suggestion <- suggestClusters(models)
  if (is.null(suggestion)) {
    return("")
  }

  agree <- suggestion$agree
  if (length(agree) == 0L) {
    agree <- "silhouette"
  }

  label <- c(asw = "Silhouette", dunn = "Dunn", db = "Davies-Bouldin")[agree]
  if (any(is.na(label))) {
    label <- label[!is.na(label)]
  }

  sprintf("Suggested k: %d (%s %s, silhouette %.2f)",
          suggestion$k,
          paste(label, collapse = " and "),
          if (length(agree) == 1L) "suggests" else "agree",
          suggestion$asw)
}
# The centroid of the SBD/DBA clustering is a shape prototype, not the mean of
# its members, so its values can lie outside the 0..1 range of the curves. It is
# scaled to the same range as the individual curves, which puts it in the middle
# of its cluster again.
normaliseCentroids <- function(p) {
  for (i in seq_along(p$layers)) {
    d <- p$layers[[i]]$data
    if (is.null(d) || !all(c("x", "y", "Cluster") %in% names(d)) ||
        "mzIdx" %in% names(d)) {
      next
    }

    p$layers[[i]]$data <- d %>%
      dplyr::group_by(.data$Cluster) %>%
      dplyr::mutate(
        y = (y - min(y, na.rm = TRUE)) /
          (max(y, na.rm = TRUE) - min(y, na.rm = TRUE))
      ) %>%
      dplyr::ungroup()
  }

  p
}
