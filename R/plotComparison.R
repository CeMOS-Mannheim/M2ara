# Feature comparison -----------------------------------------------------------
#
# Several features side by side: the concentration-response curves as one overlay
# (colour = m/z, the concentration is the x axis) and the peak profiles as a
# 2 x 2 grid that keeps the concentration colours.

# colours for the compared features: deliberately not the viridis palette that
# is used for the concentrations (Okabe-Ito, colour blind friendly)
featureColours <- function(n) {
  palette <- c("#D55E00", "#009E73", "#CC79A7", "#0072B2")
  rep(palette, length.out = n)
}

# intensities of one m/z in the single spectra, per concentration
singlePeakStats <- function(res, mz) {
  int <- vapply(getSinglePeaks(res), function(x) {
    idx <- match.closest(mz, mass(x), tolerance = 0.01)
    intensity(x)[idx]
  }, numeric(1))

  tibble::tibble(x = getConc(res), int = int) %>%
    dplyr::group_by(.data$x) %>%
    dplyr::summarise(
      sd = stats::sd(.data$int),
      sem = .data$sd / sqrt(dplyr::n()),
      .groups = "drop"
    )
}

#' Overlay of the curves of several features
#'
#' @param res       MALDIassay object
#' @param mzIdxs    indices of the features (max 4)
#' @param errorbars "none", "sd" or "sem"
#' @param normalize scale every curve to its own min/max
#'
#' @return ggplot object
plotCurvesOverlay <- function(res, mzIdxs, errorbars = "none", normalize = FALSE,
                               colour = NULL) {
  errorbars <- match.arg(errorbars, c("none", "sd", "sem"))
  mzIdxs <- mzIdxs[!is.na(mzIdxs)]
  if (length(mzIdxs) == 0L) {
    stop("no feature selected", call. = FALSE)
  }

  fits <- getCurveFits(res)[mzIdxs]
  mz_vals <- as.numeric(names(fits))

  curve_rows <- vector("list", length(fits))
  point_rows <- vector("list", length(fits))

  for (i in seq_along(fits)) {
    model <- fits[[i]]$model
    mz <- mz_vals[i]

    curve_rows[[i]] <- tibble::tibble(
      mz = mz,
      x = nplr::getXcurve(model),
      y = nplr::getYcurve(model)
    )

    stats <- singlePeakStats(res, mz)
    points <- tibble::tibble(
      mz = mz,
      x = nplr::getX(model),
      y = nplr::getY(model)
    )
    points$sd <- stats$sd[seq_len(nrow(points))]
    points$sem <- stats$sem[seq_len(nrow(points))]

    point_rows[[i]] <- points
  }

  curves <- dplyr::bind_rows(curve_rows)
  points <- dplyr::bind_rows(point_rows)

  if (isTRUE(normalize)) {
    scale01 <- function(d) {
      rng <- range(d$y, na.rm = TRUE)
      if (!is.finite(diff(rng)) || diff(rng) == 0) {
        d$y <- 0
      } else {
        d$y <- (d$y - rng[1]) / diff(rng)
      }
      d
    }
    curves <- dplyr::group_by(curves, .data$mz) %>%
      dplyr::group_modify(~ scale01(.x)) %>%
      dplyr::ungroup()
    points <- dplyr::group_by(points, .data$mz) %>%
      dplyr::group_modify(~ scale01(.x)) %>%
      dplyr::ungroup()
  }

  mz_labels <- sprintf("%.2f", mz_vals)
  points$mz <- factor(sprintf("%.2f", points$mz), levels = mz_labels)
  curves$mz <- factor(sprintf("%.2f", curves$mz), levels = mz_labels)

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = curves,
      ggplot2::aes(x = .data$x, y = .data$y, col = .data$mz),
      linewidth = 1, alpha = 0.8
    ) +
    ggplot2::geom_point(
      data = points,
      ggplot2::aes(x = .data$x, y = .data$y, col = .data$mz),
      size = 2, shape = 1, stroke = 0.8
    ) +
    ggplot2::scale_colour_manual(values = if (is.null(colour)) {
    featureColours(length(mz_vals))
  } else {
    rep(colour, length(mz_vals))
  }) +
    ggplot2::scale_x_continuous(labels = function(b) scales::label_scientific()(10^b),
                                breaks = unique(points$x)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(
      x = "Conc.",
      y = if (isTRUE(normalize)) "Intensity (normalized)" else "Intensity",
      col = "m/z",
      title = paste0(length(mzIdxs), " features: ",
                     paste(round(mz_vals, 2), collapse = ", "))
    )

  if (errorbars == "sd") {
    p <- p + ggplot2::geom_errorbar(
      data = points,
      ggplot2::aes(x = .data$x, ymin = .data$y - .data$sd, ymax = .data$y + .data$sd,
                   col = .data$mz),
      width = 0.1, alpha = 0.5
    )
  }
  if (errorbars == "sem") {
    p <- p + ggplot2::geom_errorbar(
      data = points,
      ggplot2::aes(x = .data$x, ymin = .data$y - .data$sem, ymax = .data$y + .data$sem,
                   col = .data$mz),
      width = 0.1, alpha = 0.5
    )
  }

  p
}
# peak profiles of several features as a grid (2 columns by default); every
# panel keeps its own axes, which a plotly subplot does not
peakGrid <- function(res, mzIdxs, tol, centroided, showPeaks, ncol = 2) {
  mzIdxs <- mzIdxs[!is.na(mzIdxs)]
  if (length(mzIdxs) == 0L) {
    stop("no feature selected", call. = FALSE)
  }

  plots <- lapply(mzIdxs, function(idx) {
    mz <- getMzFromMzIdx(res, idx)
    plotPeak(res, mzIdx = idx, tol = tol, centroided = centroided,
             showPeaks = showPeaks) +
      ggplot2::labs(title = paste0("m/z ", sprintf("%.2f", mz))) +
      # keep the target m/z in the middle, even when the window reaches past
      # the measured range
      ggplot2::coord_cartesian(xlim = c(mz - tol, mz + tol))
  })

  # the app theme (set in .onLoad) applies to all panels
  patchwork::wrap_plots(plots, ncol = ncol, guides = "collect")
}