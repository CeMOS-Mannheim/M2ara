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
      `abs. log2FC` = abs(log2FC)
    ) %>%
    ungroup()

  return(stats)
}
