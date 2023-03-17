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
      theme_light(base_size = 14) +
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

  exp <- round(pca[[3]], 1)

  df <- tibble(
    xax = pca[[1]] %>% pull(xnum),
    yax = pca[[1]] %>% pull(ynum),
    c = conc,
    spot = spots
  )

  df %>%
    ggplot(aes(x = xax,
               y = yax,
               col = c,
               text = spot)) +
    geom_point() +
    stat_ellipse(aes(group = c), level = ellipseLevel) +
    scale_color_viridis_d(end = 0.75, option = "C") +
    theme_light(base_size = 14) +
    labs(col = "Conc. [M]",
         x = paste0(x, " (", exp[xnum], "% expl. var.)"),
         y = paste0(y, " (", exp[ynum], "% expl. var.)"))
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
      theme_light(base_size = 14) +
      coord_flip() +
      labs(x = "m/z [Da]",
           y = paste0(pc, " Loading")) +
      theme(legend.position = "none")

    return(p)
  } else {
    p <- df %>%
      ggplot(aes(x = mz,
                 y = val,
                 col = sign)) +
      geom_linerange(aes(ymin = 0, ymax = val), show.legend = FALSE) +
      theme_light(base_size = 14) +
      labs(x = "m/z [Da]",
           y = paste0(pc, " Loading")) +
      theme(legend.position = "none")
    return(p)
  }
}

glmRegPlot <- function(model, penalty) {
  p <- model$model %>%
    finalize_model(tibble(penalty = 10^penalty)) %>%
    fit(data = model$prepData, formula = conc ~.) %>%
    predict(model$prepData) %>%
    mutate(truth = pull(model$prepData, conc)) %>%
    ggplot(aes(x = .pred, y = truth)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm") +
    ggpubr::stat_regline_equation(aes(label = paste0("R2adj.=", ..adj.rr..))) +
    coord_obs_pred() +
    labs(x = "Log-predicted conc.",
         y = "Log-true conc.") +
    theme_minimal(base_size = 14)

  return(p)
}

viPlot <- function(vi) {
  NumNonZeoroCoef <- vi %>%
    filter(Importance > 0) %>%
    pull(Importance) %>%
    length()

  p <- vi  %>%
    arrange(desc(Importance)) %>%
    slice_head(n = 20) %>%
    mutate(Variable = round(readr::parse_number(Variable), 3)) %>%
    mutate(imp = ifelse(Sign == "POS", Importance, -Importance)) %>%
    mutate(Variable = fct_reorder(as.factor(Variable), imp)) %>%
    ggplot(aes(x = Variable, y = imp, fill = Sign)) +
    geom_col() +
    geom_text(aes(x = 1, y = 0, label = paste0("#-features =", NumNonZeoroCoef))) +
    coord_flip() +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none") +
    labs(x = "m/z",
         y = "Variable importance")

  return(p)
}

plateMapPlot <- function(RV,
                         stat = c("Concentration",
                                  "Total Peak Intensity",
                                  "Normalization factor",
                                  "Recal-shift",
                                  "Selected-mz",
                                  "PC-x",
                                  "PC-y",
                                  "LASSO-error"),
                         PCs,
                         penalty,
                         mz_idx = NULL,
                         format = 384,
                         log10 = FALSE) {
  res <- RV$res
  stat <- match.arg(stat)

  spots <- getSpots(res)

  switch(stat,
         "Concentration" = {
           stat <- "Conc."
           df <- tibble(spot = spots,
                        val = as.numeric(names(spots)))
         },
         "Total Peak Intensity" = {
           stat <- "Total peak int."
           df <- tibble(spot = spots,
                        val = vapply(getSinglePeaks(res),
                                     function(x) {
                                       sum(intensity(x))
                                     }, numeric(1)))
         },
         "Normalization factor" = {
           stat <- "Norm. factor"
           df <- tibble(spot = spots,
                        val = getAppliedNormFactors(res))
         },
         "Recal-shift" = {
           df <- tibble(spot = spots,
                        val = getAppliedMzShift(res))
           if(log10) {
             # log will lead to NA for negative values, display abs. shift
             stat <- "Abs. Recal-shift"
             df <- df %>%
               mutate(val = abs(val))
           }
         },
         "Selected-mz" = {
           if(!is.null(mz_idx)) {
             stat <- paste0("m/z=", round(getMzFromMzIdx(res, mzIdx = mz_idx), digits = 2))
             int <- getSingleSpecIntensity(res, mz_idx = mz_idx)
           } else {
             int <- rep(NA_integer_, length(spots))
           }

           df <- tibble(spot = spots,
                        val = int)
         },
         "PC-x" = {
           if(!is.null(RV$pca)) {
             xnum <- parse_number(PCs[1])
             if(log10) {
               stat <- paste("Abs.", PCs[1])
               df <- tibble(
                 val = RV$pca[[1]] %>% pull(xnum),
                 spot = spots) %>%
                 mutate(val = (val-min(val))/(max(val)-min(val))*100)
             } else {
             stat <- PCs[1]
               df <- tibble(
                 val = RV$pca[[1]] %>% pull(xnum),
                 spot = spots)
             }
           } else {
             stat <- "No PCA data"
             df <- tibble(
               val = rep(NA_integer_, length(spots)),
               spot = spots)
           }
         },
         "PC-y" = {
           if(!is.null(RV$pca)) {
             ynum <- parse_number(PCs[2])
             if(log10) {
               stat <- paste("Abs.", PCs[2])
               df <- tibble(
                 val = RV$pca[[1]] %>% pull(ynum),
                 spot = spots) %>%
                 mutate(val = (val-min(val))/(max(val)-min(val))*100)
             } else {
               stat <- PCs[2]
               df <- tibble(
                 val = RV$pca[[1]] %>% pull(ynum),
                 spot = spots)
             }
           } else {
             stat <- "No PCA data"
             df <- tibble(
               val = rep(NA_integer_, length(spots)),
               spot = spots)
           }
         },
         "LASSO-error" = {
           if(!is.null(RV$model)) {
             df <- RV$model$model %>%
               finalize_model(tibble(penalty = 10^penalty)) %>%
               fit(data = RV$model$prepData, formula = conc ~.) %>%
               predict(RV$model$prepData) %>%
               mutate(truth = pull(RV$model$prepData, conc)) %>%
               mutate(val = abs(truth - .pred),
                      spot = spots)

             if(log10) {
               stat <- "log10(Abs. error)"
             } else {
               stat <- "Abs. error"
             }
           } else {
             stat <- "No LASSO data"
             df <- tibble(
               val = rep(NA_integer_, length(spots)),
               spot = spots)
           }
         })



  p <- platetools::raw_map(data = df$val, well = df$spot, plate = format) +
    labs(fill = stat) +
    theme_minimal(base_size = 16)

  if(log10) {
    p <- p  +
      scale_fill_viridis_c(trans = "log10")
  } else {
    p <- p +
      scale_fill_viridis_c()
  }

  return(p)
}
