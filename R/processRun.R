# Processing pipeline ----------------------------------------------------------
#
# Everything that turns the loaded spectra into a MALDIassay object, without
# shiny input or appData. The server runs this (optionally in a background
# process, see processRunAsync) and the tests use it directly.

# which error state the app shows when the preprocessing fails
processingErrorKind <- function(settings) {
  if (isTRUE(settings$SinglePointRecal)) {
    "recal"
  } else if (identical(settings$normMeth, "mz")) {
    "norm"
  } else {
    "other"
  }
}

# settings the pipeline needs; a plain list so it can be passed to a child
# process or hashed for the cache
processingSettings <- function(input) {
  list(
    SNR = input$SNR,
    SinglePointRecal = isTRUE(input$SinglePointRecal),
    normMz = input$normMz,
    normTol = input$normTol,
    normMeth = input$normMeth,
    alignTol = input$alignTol,
    halfWindowSize = input$halfWindowSize,
    peakMethod = input$peakMethod,
    avgMethod = input$avgMethod,
    binTol = input$binTol,
    VarFilterMethod = input$VarFilterMethod
  )
}

# @param spec_all      loaded spectra, names are the concentrations
# @param spec_idx      logical/numeric index of the spectra to use
# @param centroided    TRUE for centroided data
# @param preprocessing list(sqrtTrans, smooth, rmBl, monoisotopicFilter)
# @param settings      see processingSettings()
# @param progress      function(step) called between the stages
#
# @return list(ok = TRUE, res = <MALDIassay>) or
#         list(ok = FALSE, error = "recal"|"norm"|"other")
processRun <- function(spec_all, spec_idx, centroided, preprocessing, settings,
                       dir = NULL, progress = function(step) NULL) {
  progress("start")

  spec_all <- MALDIcellassay:::.repairMetaData(spec_all)

  progress("preprocess")
  prc <- preprocess(
    spectra = spec_all[spec_idx],
    sqrtTransform = preprocessing$sqrtTrans,
    smooth = preprocessing$smooth,
    rmBaseline = preprocessing$rmBl,
    SNR = settings$SNR,
    singlePointRecal = settings$SinglePointRecal,
    normMz = settings$normMz,
    normTol = settings$normTol,
    normMeth = settings$normMeth,
    smoothHalfWindowSize = 3,
    alignTol = settings$alignTol * 1e-3,
    halfWindowSize = settings$halfWindowSize,
    peakMethod = settings$peakMethod,
    centroided = centroided
  )

  if (is.null(prc)) {
    return(list(ok = FALSE, error = processingErrorKind(settings)))
  }

  progress("aggregate")
  avg <- MALDIcellassay:::.aggregateSpectra(
    spec = prc$spec,
    averageMethod = settings$avgMethod,
    SNR = settings$SNR,
    monoisotopicFilter = isTruthy(preprocessing$monoisotopicFilter),
    binTol = settings$binTol * 1e-6,
    normMz = settings$normMz,
    normTol = settings$normTol,
    halfWindowSize = settings$halfWindowSize,
    peakMethod = settings$peakMethod
  )

  # single spectra intensities on the signals of the average spectra: this gives
  # a higher sensitivity for the signal detection while statistics are still
  # calculated on the single spectra
  progress("intensity")
  singlePeaks <- extractIntensity(
    mz = as.numeric(colnames(avg$intmat)),
    peaks = prc$singlePeaks,
    spec = prc$spec,
    tol = settings$normTol
  )

  progress("fit")
  fits <- calculateCurveFit(
    intmat = avg$intmat,
    idx = filterVariance(apply(avg$intmat, 2, var),
                         method = settings$VarFilterMethod)
  )

  progress("statistics")
  stat_df <- MALDIcellassay:::calculatePeakStatistics(
    curveFits = fits,
    singlePeaks = singlePeaks,
    spec = prc$spec
  )

  res <- new(
    "MALDIassay",
    avgSpectra = avg$avgSpec,
    avgPeaks = avg$avgPeaksBinned,
    singlePeaks = singlePeaks,
    singleSpecSpots = extractSpots(prc$spec),
    normFactors = prc$normFac,
    mzShifts = prc$mzShift,
    fits = fits,
    stats = stat_df,
    included_specIdx = prc$idx,
    settings = list(
      Conc = as.numeric(names(singlePeaks)),
      normMz = settings$normMz,
      normTol = settings$normTol,
      varFilterMethod = settings$VarFilterMethod,
      monoisotopicFilter = isTruthy(preprocessing$monoisotopicFilter),
      alignTol = settings$alignTol * 1e-3,
      SNR = settings$SNR,
      normMeth = settings$normMeth,
      binTol = settings$binTol * 1e-6,
      SinglePointRecal = settings$SinglePointRecal,
      dir = dir
    )
  )

  progress("done")
  list(ok = TRUE, res = res)
}
