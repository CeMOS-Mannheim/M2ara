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
