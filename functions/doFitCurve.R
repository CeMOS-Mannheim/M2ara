

doFitCurve <- function(appData, spec, input) {
  #res <- NULL
  tryCatch(
    res <<- fitCurve(spec = spec,
             dir = appData$selected_dir,
             conc = NA,
             unit = input$concUnits,
             normMeth = input$normMeth,
             SinglePointRecal = input$SinglePointRecal,
             monoisotopicFilter = input$monoisotopicFilter,
             normMz = input$normMz,
             normTol = input$normTol,
             binTol = input$binTol * 1e-6, # convert to ppm
             alignTol = input$alignTol,
             SNR = input$SNR,
             varFilterMethod = input$VarFilterMethod,
             saveIntensityMatrix = FALSE,
             plot = FALSE),
    error = function(e) e
  )

  cat("res is null? ", is.null(res), "\n")

  cat("Fitting successful.\n")

  return(res)
}
