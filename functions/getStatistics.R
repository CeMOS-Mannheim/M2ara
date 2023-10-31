getStatistics <- function(res) {
  ssmd <- calculateSSMD(res)
  v <- calculateVPrime(res)
  z <- calculateZPrime(res)

  stats <-   getPeakStatistics(res, summarise = FALSE) %>%
    mutate(mz = as.numeric(mz)) %>%
    group_by(mz, mzIdx) %>%
    summarise(
      pIC50 = first(pIC50),
      R2 = first(R2),
      log2FC = log2(first(fc_window))
      ) %>%
    ungroup() %>%
    mutate(
      mz = round(mz, 3),
      pIC50 = round(pIC50, 2),
      R2 = round(R2, 2),
      log2FC = round(log2FC, 2),
      SSMD = round(ssmd, 2),
      `V'` = round(v, 2),
      `Z'` = round(z, 2)
      )

  return(stats)
}
