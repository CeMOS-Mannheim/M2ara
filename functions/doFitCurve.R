doFitCurve <- function(appData, spec, input) {

  res <- tryCatch({
    res <-
      fitCurve(spec = spec,
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
               plot = FALSE)
    return(res)
  }, error = function(cond) {
    cat(conditionMessage(cond))
    return(NULL)
  })

  return(res)
}
