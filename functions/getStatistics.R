CalculateCurveResponseScore <- function(z, v, log2FC) {
  maxFC <- 2.59 # top = 6 * bottom

  absFC <- abs(log2FC)

  # limit to -1
  zScore <- if_else(z < -0.5, -0.5, z)

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
  score <- if_else(z <= -0.5 | v <= -0.5, 0, score)

  return(round(score, 1))
}

getStatistics <- function(res) {
  ssmd <- calculateSSMD(res, nConc = 2)
  v <- calculateVPrime(res)
  z <- calculateZPrime(res, nConc = 2)

  suppressWarnings(
    stats <-   getPeakStatistics(res, summarise = FALSE) %>%
      mutate(mz = as.numeric(mz)) %>%
      group_by(mz, mzIdx) %>%
      summarise(
        pEC50 = first(pIC50),
        R2 = first(R2),
        log2FC = log2(first(fc))
      ) %>%
      ungroup() %>%
      mutate(
        mz = round(mz, 3),
        pEC50 = round(pEC50, 2),
        R2 = round(R2, 2),
        log2FC = round(log2FC, 2),
        SSMD = round(ssmd, 2),
        `V'` = round(v, 2),
        `Z'` = round(z, 2),
        CRS = CalculateCurveResponseScore(z = z, v = v, log2FC = log2FC)
      ) %>%
      mutate(CRS = if_else(CRS < 0, 0, CRS))
  )

  return(stats)
}
