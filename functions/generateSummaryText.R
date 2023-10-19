generateSummaryText <- function(object, smooth, rmBl, sqrtTrans, monoFil) {
  # helper functions
  parentDir <- function(path, times) {
    newPath <- path
    for(i in 1:times) {
      newPath <- dirname(newPath)
    }
    return(newPath)
  }

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


  concStr <- c("<h4>MALDIassay object</h4>",
               paste("Including", length(unique(conc)), "concentrations,\n"),
               paste("ranging from", min(conc), "to", max(conc), "."),
               "\n")

  # Compose processing steps
  if(any(sqrtTrans, smooth, rmBl, monoFil)) {
    preprocessStr <- paste0("<ul>",
                            ifelse(sqrtTrans, "<li>Square-root transformation</li>", ""),
                            ifelse(smooth, "<li>Savitzky Golay smoothing (half-window size = 3)</li>", ""),
                            ifelse(rmBl, "<li>Baseline removal (TopHat, half-window size = 3)</li>", ""),
                            ifelse(monoFil, "<li>Monoisotopic peak filter</li>", ""),
                            "</ul>"
    )
  } else (
    preprocessStr <- "No processing steps applied.\n\n"
  )

  if (object@settings$SinglePointRecal) {
    singlePointRecalStr<- c(paste("Single point recalibation on", mz, "with", tol, "Da tolerance."),
                            paste("Avg. mass shift before recal.:", meanMzShift, "Da. Max abs. shift:", absMaxMzShift, "Da."),
                            paste("Avg. mass shift after recal. :", round(mzdev$mean, 4), "Da. Max abs. shift:", round(mzdev$meanAbs, 4), "Da."),
                            "\n")
  }
  if(object@settings$monoisotopicFilter) {
    numPeaksStr <- c(paste0("Found ", numPeaksTotal, " <strong>monoisotopic peaks</strong> (SNR ", getSNR(object), ") and ", hiVarPeaks, " high variance peaks"),
                     paste("using variance filtering method:", paste0(varFilterMethod, ".")),
                     "\n")
  } else {
    numPeaksStr <- c(paste0("Found ", numPeaksTotal, " peaks (SNR ", getSNR(object), ") and ", hiVarPeaks, " high variance peaks"),
                     paste("using variance filtering method:", paste0(varFilterMethod, ".")),
                     "\n")
  }


  # merge meta data
  metaData <- MALDIquant:::.mergeMetaData(
    lapply(getAvgSpectra(object),
           function(x) {
             x@metaData
           })
  )
  # format meta data
  head <- c("<h4>Measurement method</h4>")
  instrument <- paste("<strong>Instrument:</strong>", metaData$instrument, metaData$spectrometerType, metaData$tofMode, "\n")
  method <- paste("<strong>Method:</strong>", metaData$acquisitionMethod, "\n")
  laser <- paste0("<strong>Laser:</strong> ", metaData$laserAttenuation, "%, ", metaData$laserShots, " shots @", metaData$laserShots/1e3, " kHz\n")
  path <- paste("<strong>Path:</strong>", parentDir(metaData$path[1], 4), "\n")

  instrumentSettingStr <- c(head, instrument, laser, method, path)

  if(object@settings$SinglePointRecal) {
    outputStr <- c(concStr, preprocessStr, normStr, singlePointRecalStr, numPeaksStr, instrumentSettingStr)
    return(outputStr)
  }

  outputStr <-
    markdown(
      paste0(
        c(concStr, preprocessStr, normStr, numPeaksStr, instrumentSettingStr),
        collapse = "<br>"))

  return(outputStr)
}
