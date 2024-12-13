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
                       halfWindowSize,
                       peakMethod,
                       centroided = centroided
) {
  nm <- names(spectra)
  if(!smooth & !rmBaseline & !sqrtTransform) {
    message("No preprocessing selected. Returning unprocessed spectra.\n")
    return(spectra)
  }

  if(sqrtTransform) {
    message("Applying variance stabilitzation by sqrt-transform...\n")
    spectra <- suppressWarnings(
      transformIntensity(spectra,
                         method = "sqrt"))
  }



  if(smooth) {
    if(centroided) {
      message("Skipping smoothing because spectra are centroided.\n")
    } else {
      message("Smoothing...\n")
      spectra <- suppressWarnings(
        smoothIntensity(spectra,
                        method = smoothMethod,
                        halfWindowSize = smoothHalfWindowSize)
      )
    }
  }

  if(rmBaseline) {
    if(centroided) {
      message("Skipping baseline removal because spectra are centroided.\n")
    } else {
      message("Removing baseline...\n")
      spectra <- suppressWarnings(
        removeBaseline(spectra,
                       method = rmBlMethod)
      )
    }

  }

  names(spectra) <- nm
  if(centroided) {
    message("Skipping baseline removal because spectra are centroided.\n")
    peaks <- spectra
  } else {
    message("Detecting peaks...\n")
    peaks <- MALDIcellassay:::.detectPeaks(spectra,
                                           SNR = SNR,
                                           method = peakMethod,
                                           halfWindowSize = halfWindowSize)
  }

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
