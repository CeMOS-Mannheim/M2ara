preprocess <- function(spectra,
                       smooth,
                       rmBaseline,
                       sqrtTransform = FALSE,
                       smoothHalfWindowSize = 3,
                       smoothMethod = "SavitzkyGolay",
                       rmBlMethod = "TopHat") {
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
  cat("Preprocessing done.\n")
  return(spectra)
}
