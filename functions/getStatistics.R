getStatistics <- function(res) {
  ssmd <- calculateSSMD(res, nConc = 2)
  v <- calculateVPrime(res, nConc = 2)
  z <- calculateZPrime(res, nConc = 2)

  logConc = log10(getConc(res))

  maxPotPIC50 <- max(-logConc[which(is.finite(logConc))], na.rm = TRUE)



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
      `Z'` = round(z, 2),
      score = round(
        if_else(v < 0 & z < 0, 0 , 1) *
        (if_else(z < 0, 0 , z) +
          if_else(v < 0, 0 , v) +
          ssmd/max(ssmd, na.rm = TRUE) +
          pIC50 / maxPotPIC50) / 4 * 100,
        1)
    )




  return(stats)
}
