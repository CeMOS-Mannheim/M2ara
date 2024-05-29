# set default theme
theme_set(theme_light(base_size = 16) +
            theme(panel.grid = element_blank(),
                  strip.text.x = element_text(margin = margin(2,1,2,1, "mm"),
                                              size = 10)))

generateSpecPlots <- function(res) {
  allPeaks <- getAvgPeaks(res)
  avgSpec <- mergeMassPeaks(allPeaks)

  mzRange <- range(mass(avgSpec))
  specNames <- c("Global Average",
                 paste(as.character(unique(getConc(res))), "M"))

  l <- c(avgSpec, allPeaks)

  p <- lapply(1:length(specNames), FUN = function(i) {

    df <- tibble(mz = mass(l[[i]]),
                 int = intensity(l[[i]]))
    pspec <- ggplot(df, aes(x = mz, ymin = 0, ymax = int)) +
      geom_linerange() +
      scale_x_continuous(limits = mzRange) +
      labs(x = "m/z",
           y = "Intensity",
           title = specNames[i])
    return(pspec)
  })
  return(p)
}

pcaPlot <- function(pca, conc, x, y, ellipseLevel, spots) {
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
    stat_ellipse(aes(group = c), level = ellipseLevel) +
    scale_color_viridis_d(end = 0.75, option = "C") +
    labs(col = "Conc. [M]",
         x = paste0(x, " (", exp[xnum], "% expl. var.)"),
         y = paste0(y, " (", exp[ynum], "% expl. var.)"))

  p <- ggplotly(p)

  return(p)
}

loadingsPlot <- function(pca, pc, simple = TRUE, n = 10) {
  pcnum <- parse_number(pc)

  df <- tibble(mz = rownames(pca[[2]]),
               val = pca[[2]] %>% pull(pcnum)) %>%
    mutate(mz = round(as.numeric(mz), 2)) %>%
    arrange(desc(val)) %>%
    mutate(sign = ifelse(val > 0, "pos", "neg"))
  if(simple) {
    df_red <- slice_head(df, n = n) %>%
      bind_rows(slice_tail(df, n = n)) %>%
      mutate(mz = factor(mz)) %>%
      mutate(mz = fct_reorder(mz, .x = val))

    p <- df_red %>%
      ggplot(aes(x = mz,
                 y = val,
                 fill = sign)) +
      geom_col(show.legend = FALSE) +
      geom_hline(yintercept = 0, alpha = 0.75, linetype = "dashed") +
      coord_flip() +
      labs(x = "m/z [Da]",
           y = paste0(pc, " Loading"),
           title = paste("Top-Features for", pc)) +
      theme(legend.position = "none")

  } else {
    p <- df %>%
      ggplot(aes(x = mz,
                 y = val,
                 col = sign)) +
      geom_linerange(aes(ymin = 0, ymax = val), show.legend = FALSE) +
      labs(x = "m/z [Da]",
           y = paste0(pc, " Loading"),
           title = paste("Feature importance for", pc)) +
      theme(legend.position = "none")
  }
  p <- ggplotly(p)
  return(p)
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
                         penalty,
                         mz_idx = NULL,
                         format = 384,
                         log10 = FALSE) {
  res <- appData$res
  stat <- match.arg(stat)

  spots <- getSpots(res)

  switch(stat,
         "Concentration" = {
           lab <- "Conc."
           conc <- as.numeric(names(spots))
           df <- tibble(spot = spots,
                        val = fct_reorder(scales::scientific(conc), conc))
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
           if(log10) {
             # log will lead to NA for negative values, display abs. shift
             lab <- "Abs. Recal-shift"
             df <- df %>%
               mutate(val = abs(val))
           }
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
             if(log10) {
               lab <- paste("Abs.", PCs[1])
               df <- tibble(
                 val = appData$pca[[1]] %>% pull(xnum),
                 spot = spots) %>%
                 mutate(val = (val-min(val))/(max(val)-min(val))*100)
             } else {
               lab <- PCs[1]
               df <- tibble(
                 val = appData$pca[[1]] %>% pull(xnum),
                 spot = spots)
             }
           } else {
             lab <- "No PCA data"
             df <- tibble(
               val = rep(NA_integer_, length(spots)),
               spot = spots)
           }
         },
         "PC-y" = {
           if(!is.null(appData$pca)) {
             ynum <- parse_number(PCs[2])
             if(log10) {
               lab <- paste("Abs.", PCs[2])
               df <- tibble(
                 val = appData$pca[[1]] %>% pull(ynum),
                 spot = spots) %>%
                 mutate(val = (val-min(val))/(max(val)-min(val))*100)
             } else {
               lab <- PCs[2]
               df <- tibble(
                 val = appData$pca[[1]] %>% pull(ynum),
                 spot = spots)
             }
           } else {
             lab <- "No PCA data"
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
               mutate(val = calculateChauvenetCriterion(intensity))
           }
         },
         "Outlier-all" = {
           lab <- paste0("Chauvenet\nCriterion\n",
                         "n(m/z)")
           df <- getIntensityMatrix(res) %>%
             as_tibble() %>%
             mutate(conc = getConc(res),
                    spot = getSpots(res)) %>%
             pivot_longer(-c(conc, spot),
                          names_to = "mz",
                          values_to = "intensity") %>%
             group_by(conc, mz) %>%
             mutate(cheuv = calculateChauvenetCriterion(intensity)) %>%
             group_by(spot) %>%
             count(cheuv) %>%
             pivot_wider(names_from = cheuv, values_from = n) %>%
             mutate(val = `TRUE`)
         })

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
    p <- p  +
      scale_fill_viridis_d()
    return(p)
  }

  if(log10 & !stat == "Outlier-mz") {
    p <- p  +
      scale_fill_viridis_c(trans = "log10")
    return(p)
  }

  if(!stat == "Outlier-mz") {
    p <- p +
      scale_fill_viridis_c()
  }
  return(p)
}

scorePlot <- function(stats, metric = c("CRS", "V'", "Z'", "log2FC", "pEC50", "SSMD")) {
  metric <- match.arg(metric)

  df <- stats %>%
    mutate(direction = if_else(log2FC < 0, "down", "up")) %>%
    select(c("mz", "direction")) %>%
    mutate(value = pull(stats, metric))

  if(metric %in% c("V'", "Z'")) {
    # cut V' and Z' at zero as lower values then zero just indicate bad models
    # and its prettier for visualization
    df <- df %>%
      mutate(value = if_else(value < 0, 0, value))

    limits <- c(-1, 1)
  }

  if(metric %in% c("CRS", "V'", "Z'", "SSMD")) {
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

  if(metric %in% c("V'", "Z'")) {
    p <- p +
      scale_y_continuous(limits = limits,
                         breaks = c(-1, -0.5, 0, 0.5, 1),
                         labels = c(1, 0.5, 0, 0.5 , 1))
  }

  if(metric %in% c("log2FC", "SSMD")) {
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

