loadSpectraData <- function(input, appData) {
  if (appData$info_state %in% c("dir_set",
                                "loadErrorNum",
                                "loadErrorFormat")) {
    show_spinner()

    # check if all spectra names are numeric/concentrations
    # for later: if pos and neg ctrls are included
    # checkSpecNames needs to return indices of the numeric folders
    if (!checkSpecNamesNumeric(appData$selected_dir, input$fileFormat)) {
      warning("Found folder names that could not be converted to numeric.
                All folders/spectra need to have concentrations as names.\n")
      appData$info_state <- "loadErrorNum"
      hide_spinner()
      return()
    }
    if (!checkSpecNamesFormat(appData$selected_dir, input$fileFormat)) {
      warning("Could not find data with '", input$fileFormat, "' file format.\n")
      appData$info_state <- "loadErrorFormat"
      hide_spinner()
      return()
    }

    switch(input$fileFormat,
           "bruker" = {
             spec_raw <- loadSpectra(appData$selected_dir)
           },
           "mzml" = {
             spec_raw <- loadSpectraMzML(appData$selected_dir)
           })
    appData$info_state <- "loaded"

    # make sure that the concentrations are in acending order
    spec_raw <- spec_raw[order(as.numeric(names(spec_raw)))]

    appData$org_conc <- as.numeric(names(spec_raw))

    appData$spec_all <- spec_raw
    if(input$checkEmpty) {
      message(MALDIcellassay:::timeNow(),  " check for empty spectra...\n")

      # MAD would be faster but may fail in some circumstances...
      peaks <- detectPeaks(appData$spec_all,
                           SNR = input$SNR,
                           method = "SuperSmoother")
      appData$spec_idx <- vapply(peaks,
                                 function(x) {
                                   ifelse(length(mz(x)) > 0, TRUE, FALSE)
                                 },
                                 FUN.VALUE = TRUE)
      message(MALDIcellassay:::timeNow(), " ",
              sum(appData$spec_idx), "/", length(appData$spec_all),
              " spectra retained.\n")
    } else {
      appData$spec_idx <- rep(TRUE, length(appData$spec_all))
    }


    hide_spinner()
  }
  return(appData)
}
