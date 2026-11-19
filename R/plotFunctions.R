# The default theme of the app. It is applied in .onLoad() - top level code in a
# package runs at install time, not when the package is loaded, so a theme_set()
# call here would have no effect in the app session.
m2araTheme <- function() {
  ggplot2::theme_light(base_size = 16) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_text(
        margin = ggplot2::margin(2, 1, 2, 1, "mm"),
        size = 10
      )
    )
}

pcaPlot <- function(pca, conc, x, y, ellipseLevel, spots) {
  # the same component on both axes would draw the identity line
  if (identical(parse_number(x), parse_number(y))) {
    return(ggplotly(plateMessage(paste0(
      "x and y show the same component (", x, ").\n",
      "Pick another component for one of the axes."
    ))))
  }

  xnum <- parse_number(x)
  ynum <- parse_number(y)
  conc <- as.numeric(as.character(conc))

  exp <- round(pca[[3]], 1)

  df <- tibble(
    xax = pca[[1]] %>% pull(xnum),
    yax = pca[[1]] %>% pull(ynum),
    c = scales::scientific(conc),
    spot = spots
  )

  p <- df %>%
    ggplot(aes(x = xax,
               y = yax,
               col = c,
               text = spot)) +
    geom_point() +
    # own mapping so the ellipse does not inherit the plotly only `text`
    stat_ellipse(mapping = aes(x = xax, y = yax, group = c, col = c),
                 inherit.aes = FALSE,
                 level = ellipseLevel) +
    scale_color_viridis_d(end = 0.75, option = "C") +
    labs(col = "Conc. [M]",
         x = paste0(x, " (", exp[xnum], "% expl. var.)"),
         y = paste0(y, " (", exp[ynum], "% expl. var.)"))

  p <- ggplotly(p)

  return(p)
}

# loadings of the active features of one component, weakest to strongest
# loadings of the active features of one component, weakest to strongest
#
# At most perSide features per sign, and never more than maxBars in total: when
# one sign has fewer features the other one may use the free slots.
activeLoadings <- function(pca, pc, maxBars = 20, perSide = 10) {
  vals <- pca$loadings[[as.character(pc)]]
  active <- abs(vals) >= 0.01 * max(abs(vals))

  df <- tibble(mz = as.numeric(rownames(pca$loadings)),
               val = vals,
               active = active) %>%
    filter(.data$active)

  pos <- df %>% filter(.data$val > 0) %>% arrange(desc(.data$val))
  neg <- df %>% filter(.data$val < 0) %>% arrange(.data$val)

  nPos <- min(nrow(pos), perSide)
  nNeg <- min(nrow(neg), perSide)

  # a sign with fewer features than perSide lets the other one use the rest,
  # but never more than maxBars in total
  free <- maxBars - (nPos + nNeg)
  if (free > 0 && nrow(pos) > nPos) {
    nPos <- min(nrow(pos), nPos + free)
  } else if (free > 0 && nrow(neg) > nNeg) {
    nNeg <- min(nrow(neg), nNeg + free)
  }

  while (nPos + nNeg > maxBars) {
    if (nPos >= nNeg && nPos > 0L) {
      nPos <- nPos - 1L
    } else if (nNeg > 0L) {
      nNeg <- nNeg - 1L
    } else {
      break
    }
  }

  bind_rows(slice_head(pos, n = nPos),
            slice_head(neg, n = nNeg)) %>%
    arrange(.data$val)
}

loadingsPlot <- function(pca, pc, simple = TRUE, maxBars = 20, perSide = 10) {
  vals <- pca$loadings[[as.character(pc)]]
  # same definition as the active feature count above the plot
  nActive <- sum(abs(vals) >= 0.01 * max(abs(vals)))

  if (simple) {
    df_red <- activeLoadings(pca, pc, maxBars = maxBars, perSide = perSide)
    nShown <- nrow(df_red)
    plural <- if (nShown == 1L) "feature" else "features"

    title <- if (nShown < nActive) {
      sprintf("Top-Features for %s (%d of %d active %s)",
              pc, nShown, nActive, plural)
    } else {
      sprintf("Top-Features for %s (%d active %s)", pc, nShown, plural)
    }

    df_red <- df_red %>%
      mutate(sign = ifelse(.data$val > 0, "pos", "neg")) %>%
      mutate(mz = factor(.data$mz)) %>%
      mutate(mz = fct_reorder(.data$mz, .x = .data$val))

    p <- df_red %>%
      ggplot(aes(x = mz,
                 y = val,
                 fill = sign)) +
      geom_col(show.legend = FALSE) +
      geom_hline(yintercept = 0, alpha = 0.75, linetype = "dashed") +
      coord_flip() +
      labs(x = "m/z [Da]",
           y = paste0(pc, " Loading"),
           title = title) +
      theme(legend.position = "none")

  } else {
    df <- tibble(mz = as.numeric(rownames(pca$loadings)),
                 val = vals) %>%
      mutate(sign = ifelse(.data$val > 0, "pos", "neg"))

    p <- df %>%
      ggplot(aes(x = mz,
                 y = val,
                 col = sign)) +
      geom_linerange(aes(ymin = 0, ymax = val), show.legend = FALSE) +
      labs(x = "m/z [Da]",
           y = paste0(pc, " Loading"),
           title = sprintf("Feature importance for %s (%d of %d features active)",
                           pc, nActive, length(vals))) +
      theme(legend.position = "none")
  }

  ggplotly(p)
}

# short message instead of an empty plate (no m/z selected, no PCA data, ...)
# only metrics with a wide, strictly positive range make sense on a log scale
plateLogMetrics <- function() {
  c("Concentration", "Total Peak Intensity", "Normalization factor", "Selected-mz")
}

plateMessage <- function(label) {
  ggplot() +
    annotate("text", x = 0, y = 0, size = 5, colour = "grey35", label = label) +
    theme_void()
}

plateMapPlot <- function(appData,
                         stat = c("Concentration",
                                  "Total Peak Intensity",
                                  "Normalization factor",
                                  "Recal-shift",
                                  "Selected-mz",
                                  "PC-x",
                                  "PC-y",
                                  "Outlier-mz",
                                  "Outlier-all"),
                         PCs,
                         mz_idx = NULL,
                         format = 384,
                         log10 = FALSE) {
  res <- appData$res
  stat <- match.arg(stat)

  spots <- getSpots(res)


  # these metrics need a selected feature; without one the plate would fail
  # deep inside the plot with an obscure error
  needsMz <- stat %in% c("Selected-mz", "Outlier-mz")
  if (needsMz && (length(mz_idx) == 0L || is.na(mz_idx))) {
    return(plateMessage(paste0("This metric needs a selected feature.\n",
                               "Click a row in the table to choose one.")))
  }
  # the log scale is offered only for metrics where it is meaningful
  useLog <- isTRUE(log10) && stat %in% plateLogMetrics()

  switch(stat,
         "Concentration" = {
           lab <- "Conc."
           conc <- as.numeric(names(spots))
           if (useLog) {
             # same transformation as the curve plot: log10 of the
             # concentration, zeros one decade below the lowest value
             df <- tibble(spot = spots,
                          val = MALDIcellassay::transformConc2Log(conc))
           } else {
             df <- tibble(spot = spots,
                          val = fct_reorder(scales::scientific(conc), conc))
           }
         },
         "Total Peak Intensity" = {
           lab <- "Total peak int."
           df <- tibble(spot = spots,
                        val = vapply(getSinglePeaks(res),
                                     function(x) {
                                       sum(intensity(x))
                                     }, numeric(1)))
         },
         "Normalization factor" = {

           normMz <- getNormMz(appData$res)
           normTol <- getNormMzTol(appData$res)
           normMeth <- getNormMethod(appData$res)

           switch (normMeth,
                   "TIC" = {
                     lab <- paste0("Total ion current")
                   },
                   "mz" = {
                     lab <- paste0("Norm. factor\n","mz=", normMz, "+/-", normTol)
                   },
                   "median" = {
                     stat <- paste0("Median intensity")
                   },
                   "PQN" = {
                     lab <- paste0("PQN")
                   },
                   "none" = {
                     lab <- paste0("No normalization applied")
                   }
           )

           df <- tibble(spot = spots,
                        val = getAppliedNormFactors(res))
         },
         "Recal-shift" = {
           lab <- "Recal-shift"
           df <- tibble(spot = spots,
                        val = getAppliedMzShift(res))
         },
         "Selected-mz" = {
           if(!is.null(mz_idx)) {
             lab <- paste0("m/z=", round(getMzFromMzIdx(res, mzIdx = mz_idx), digits = 2))
             int <- getSingleSpecIntensity(res, mz_idx = mz_idx)
           } else {
             int <- rep(NA_integer_, length(spots))
           }

           df <- tibble(spot = spots,
                        val = int)
         },
         "PC-x" = {
           if(!is.null(appData$pca)) {
             xnum <- parse_number(PCs[1])
             lab <- PCs[1]
             df <- tibble(
               val = appData$pca[[1]] %>% pull(xnum),
               spot = spots)
           } else {
             lab <- "No PCA data yet.\nCalculate it in the PCA tab."
             df <- tibble(
               val = rep(NA_integer_, length(spots)),
               spot = spots)
           }
         },
         "PC-y" = {
           if(!is.null(appData$pca)) {
             ynum <- parse_number(PCs[2])
             lab <- PCs[2]
             df <- tibble(
               val = appData$pca[[1]] %>% pull(ynum),
               spot = spots)
           } else {
               lab <- "No PCA data yet.\nCalculate it in the PCA tab."
             df <- tibble(
               val = rep(NA_integer_, length(spots)),
               spot = spots)
           }
         },
         "Outlier-mz" = {
           lab <- paste0("Chauvenet\nCriterion\n",
                         "m/z=", round(getMzFromMzIdx(res, mzIdx = mz_idx), digits = 2))
           if(!is.null(mz_idx)) {
             df <- tibble(tibble(conc = getConc(res),
                                 spot = getSpots(res),
                                 intensity = getSingleSpecIntensity(res,
                                                                    mz_idx = mz_idx))) %>%
               group_by(conc) %>%
               mutate(val = MALDIcellassay::calculateChauvenetCriterion(intensity))
           }
         },
         "Outlier-all" = {
           lab <- paste0("Chauvenet\nCriterion\n",
                         "n(m/z)")
           wide <- getIntensityMatrix(res) %>%
             as_tibble() %>%
             mutate(conc = getConc(res),
                    spot = getSpots(res)) %>%
             pivot_longer(-c(conc, spot),
                          names_to = "mz",
                          values_to = "intensity") %>%
             group_by(conc, mz) %>%
             mutate(cheuv = MALDIcellassay::calculateChauvenetCriterion(intensity)) %>%
             group_by(spot) %>%
             count(cheuv) %>%
             pivot_wider(names_from = cheuv, values_from = n)

           # a dataset without any outlier has no TRUE column
           if (!"TRUE" %in% names(wide)) {
             wide[["TRUE"]] <- 0L
           }
           df <- wide %>%
             # spots without any outlier have no TRUE count
             mutate(val = ifelse(is.na(.data[["TRUE"]]), 0L, .data[["TRUE"]]))
         })

  # nothing to show (e.g. no PCA computed yet): a hint is nicer than an
  # empty plate
  if (all(is.na(df$val))) {
    return(plateMessage(lab))
  }

  p <- suppressWarnings(
    platetools::raw_map(data = df$val, well = df$spot, plate = format) +
      labs(fill = lab)
  )

  if(all(getSpots(res) == "")) {
   p <- p + geom_text(aes(x = 12, y = 8,
                          label = "No spot coordinates found."),
                      col = "darkred",
                      size = 10,
                      inherit.aes = FALSE)
  }

  if(stat == "Concentration") {
    if (useLog) {
      # the values are log10 concentrations, label them like the curve plot
      p <- p + scale_fill_viridis_c(labels = function(b) scales::scientific(10^b))
    } else {
      p <- p + scale_fill_viridis_d()
    }
    return(p)
  }


  if (useLog) {
    p <- p  +
      scale_fill_viridis_c(trans = "log10")
    return(p)
  }

  if (stat != "Outlier-mz") {
    p <- p +
      scale_fill_viridis_c()
  }
  return(p)
}

scorePlot <- function(stats, metric = c("CRS", "FV", "FZ", "log2FC", "pEC50", "FS")) {
  metric <- match.arg(metric)

  df <- stats %>%
    mutate(direction = if_else(log2FC < 0, "down", "up")) %>%
    select(c("mz", "direction")) %>%
    mutate(value = pull(stats, metric))

  if(metric %in% c("FV", "FZ")) {
    # cut V' and Z' at zero as lower values then zero just indicate bad models
    # and its prettier for visualization
    df <- df %>%
      mutate(value = if_else(value < 0, 0, value))

    limits <- c(-1, 1)
  }

  if(metric %in% c("CRS", "FV", "FZ", "FS")) {
    df <- df %>%
      mutate(value = if_else(direction == "down", -value, value))
  }

  minY <- if_else(metric == "pEC50", min(df$value, na.rm = TRUE), 0)

  ylab <- if_else(metric == "CRS", "Curve response score (%)", metric)

  p <-  df %>%
    ggplot(aes(x = mz, ymin = minY, ymax = value, col = direction)) +
    geom_linerange() +
    labs(x = "m/z",
         y = ylab,
         col = NULL)

  if(metric %in% c("FV", "FZ")) {
    p <- p +
      scale_y_continuous(limits = limits,
                         breaks = c(-1, -0.5, 0, 0.5, 1),
                         labels = c(1, 0.5, 0, 0.5 , 1))
  }

  if(metric %in% c("log2FC", "FS")) {
    absVal <- abs(df$value)
    absVal <- absVal[!is.infinite(absVal)]
    absMax <- max(absVal, na.rm = TRUE)
    p <- p +
      scale_y_continuous(limits = c(-absMax, absMax))
  }

  if(metric %in% c("CRS")) {
    p <- p +
      scale_y_continuous(limits = c(-100, 100),
                         breaks = c(-100, -50, 0, 50, 100),
                         labels = c(100, 50, 0, 50 , 100))
  }

  p <- ggplotly(p)

  return(p)
}

