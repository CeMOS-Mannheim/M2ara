preprocess <- function(spectra,
                       smooth,
                       rmBaseline,
                       sqrtTransform = FALSE,
                       smoothHalfWindowSize = 10,
                       smoothMethod = "SavitzkyGolay",
                       rmBlMethod = "TopHat",
                       SNR,
                       singlePointRecal,
                       normMz,
                       normTol,
                       normMeth,
                       alignTol,
                       halfWindowSize
) {
  nm <- names(spectra)
  if(!smooth & !rmBaseline & !sqrtTransform) {
    cat("No preprocessing selected. Returning unprocessed spectra.\n")
    return(spectra)
  }

  if(sqrtTransform) {
    cat("Applying variance stabilitzation by sqrt-transform...\n")
    spectra <- suppressWarnings(
      transformIntensity(spectra,
                         method = "sqrt")
    )
  }


  if(smooth) {
    cat("Smoothing...\n")
    spectra <- suppressWarnings(
      smoothIntensity(spectra,
                      method = smoothMethod,
                      halfWindowSize = smoothHalfWindowSize)
    )
  }

  if(rmBaseline) {
    cat("Removing baseline...\n")
    spectra <- suppressWarnings(
      removeBaseline(spectra,
                     method = rmBlMethod)
    )
  }

  names(spectra) <- nm
  cat("Detecting peaks...\n")

  peaks <- MALDIcellassay:::.detectPeaks(spectra,
                                         SNR = SNR,
                                         method = "SuperSmoother",
                                         halfWindowSize = halfWindowSize)
  prc <- tryCatch({
    prc <- MALDIcellassay:::.preprocess(peaks_single = peaks,
                                        spec = spectra,
                                        SinglePointRecal = singlePointRecal,
                                        normMz = normMz,
                                        normTol = normTol,
                                        normMeth = normMeth,
                                        alignTol = alignTol,
                                        allowNoMatches = TRUE)

    cat("Preprocessing done.\n")
    return(prc)
  },
  error = function(cond) {
    prc <- NULL

    message("Preprocessing failed with the following error:\n")
    message(conditionMessage(cond))
    return(prc)
  })


  return(prc)
}
