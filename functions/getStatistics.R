getStatistics <- function(res) {
  stats <-   getPeakStatistics(res, FALSE) %>%
    mutate(mz = as.numeric(mz)) %>%
    group_by(mz, mzIdx) %>%
    summarise(
      pIC50 = first(pIC50),
      R2 = first(R2),
      min = min(mean),
      max = max(mean),
      log2FC = log2(first(fc_window)),
      `abs. log2FC` = abs(log2FC)) %>%
    ungroup() %>%
    mutate(
      mz = round(mz, 3),
      pIC50 = round(pIC50, 2),
      R2 = round(R2, 2),
      min = round(min, 3),
      max = round(max, 3),
      log2FC = round(R2, 2),
      log2FC = round(R2, 2),
      `abs. log2FC` = round(R2, 2))

  return(stats)
}
