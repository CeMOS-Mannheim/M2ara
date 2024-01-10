

doFitCurve <- function(appData, spec, input) {

  #res <- NULL
  tryCatch({
    res <<- fitCurve(spec = spec,
                     conc = NA,
                     unit = input$concUnits,
                     normMeth = input$normMeth,
                     SinglePointRecal = input$SinglePointRecal,
                     monoisotopicFilter = input$monoisotopicFilter,
                     averageMethod =  input$avgMethod,
                     normMz = input$normMz,
                     normTol = input$normTol,
                     binTol = input$binTol * 1e-6, # convert to ppm,
                     alignTol = input$alignTol,
                     SNR = input$SNR,
                     varFilterMethod = input$VarFilterMethod)
      },
    error = function(e) e
  )
  if(is.null(res)) {
    cat("fitCurve returned null.\n")
  }


  return(res)
}
