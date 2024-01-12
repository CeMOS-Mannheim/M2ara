getScore <- function(z, v, log2FC) {
  maxFC <- 3

  absFC <- abs(log2FC)

  # limit to -0.5
  zScore <- if_else(z < -1, -1, z)


  # z > 0.5 is an excellent assay and this is what we aim for
  # z = 1.0 is hard to reach
  zScore <- if_else(zScore > 0,
                    if_else(zScore/0.5 > 1, 1, zScore/0.5),
                    zScore)

  # limit to -1
  vScore <- if_else(v < -1, -1, v)

  fcScore <- if_else(absFC > maxFC, 1, absFC/maxFC)

  score <- (zScore + vScore + fcScore) / 3 * 100

  # if metrics are really bad -> 0 score
  score <- if_else(z < -1 | v < -1, 0, score)

  return(round(score, 1))
}

getStatistics <- function(res) {
  ssmd <- calculateSSMD(res, nConc = 2)
  v <- calculateVPrime(res)
  z <- calculateZPrime(res, nConc = 2)

  logConc = log10(getConc(res))

  stats <-   getPeakStatistics(res, summarise = FALSE) %>%
    mutate(mz = as.numeric(mz)) %>%
    group_by(mz, mzIdx) %>%
    summarise(
      pIC50 = first(pIC50),
      R2 = first(R2),
      log2FC = log2(first(fc))
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
      Score = getScore(z = z, v = v, log2FC = log2FC)
    ) %>%
    mutate(Score = if_else(Score < 0, 0, Score))




  return(stats)
}
