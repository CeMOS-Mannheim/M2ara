generateSummaryText <- function(object) {
  mz <- round(getNormMz(object), digits = 2)
  varFilterMethod <- getVarFilterMethod(object)
  tol <- getNormMzTol(object)
  numPeaksTotal <- length(mass(getSinglePeaks(object)[[1]]))
  hiVarPeaks <- length(unique(getPeakStatistics(object)$mz))
  mzdev <- getRecalibrationError(object)
  meanMzShift <- round(mean(getAppliedMzShift(object)), 4)
  absMaxMzShift <- round(max(abs(getAppliedMzShift(object))), 4)
  conc <- getConc(object)

  # Compose normalization information
  if (getNormMethod(object) == "mz") {
    normStr <- paste("Normalization on m/z", mz, "+/-", tol, "Da.\n")
  } else {
    normStr <- paste("Normalization using", getNormMethod(object), "method.\n")
  }


  mzConcNormStr <- c("MALDIassay object",
                     "\n",
                     paste("Including", length(unique(conc)), "concentrations,\n"),
                     paste("ranging from", min(conc), "to", max(conc), "."),
                     "\n", normStr)



  if (object@settings$SinglePointRecal) {
    singlePointRecalStr<- c(paste("Single point recalibation on", mz, "with", tol, "Da tolerance."),
                            paste("Avg. mass shift before recal.:", meanMzShift, "Da. Max abs. shift:", absMaxMzShift, "Da."),
                            paste("Avg. mass shift after recal. :", round(mzdev$mean, 4), "Da. Max abs. shift:", round(mzdev$meanAbs, 4), "Da."),
                            "\n")
  }

  numPeaksStr <- c(paste("Found ", numPeaksTotal, " peaks (SNR ", getSNR(object), ") and ", hiVarPeaks, " high variance peaks"),
                   paste("using variance filtering method:", paste0(varFilterMethod, ".")),
                   "\n")

  if(object@settings$SinglePointRecal) {
    outputStr <- c(mzConcNormStr, singlePointRecalStr, numPeaksStr)
    return(outputStr)
  }

  outputStr <- c(mzConcNormStr, numPeaksStr)
  return(outputStr)

}
