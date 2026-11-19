loadSpectraData <- function(input, appData, mapping) {
  if (appData$info_state %in% c("dir_set",
                                "loadErrorNum",
                                "loadErrorFormat")) {
    show_spinner()

    checkEmpty <- input$checkEmpty

    # check if all spectra names are numeric/concentrations
    # for later: if pos and neg ctrls are included
    # checkSpecNames needs to return indices of the numeric folders
    if(!length(mapping)>0) {
      if (!checkSpecNamesNumeric(appData$selected_dir, input$fileFormat)) {
        warning("Found folder names that could not be converted to numeric and no mapping file found.
                All folders/spectra need to have concentrations as names or mapping file with conc. needed.\n")
        appData$info_state <- "loadErrorNum"
        hide_spinner()
        return()
      }
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
             # at the moment we assume Bruker flex data to always be continuous
             appData$centroided <- FALSE
           },
           "mzml" = {

             # check if data is centroided for first file
             # assume rest is also centroided
             f <- list.files(appData$selected_dir,
                             recursive = FALSE,
                             full.names = TRUE,
                             pattern = "mzML")
             centroided <- any(stringr::str_detect(readLines(f[1],
                                                             n = -1L),
                                                   pattern = "centroid spectrum"))

             if(centroided) {
               message("      found centroided data. Will not check for empty spectra.")
             }

             # the mzML files may contain chromatograms, the parser warns for
             # every one of them - that is noise and not actionable
             spec_raw <- withCallingHandlers(
               loadSpectraMzML(appData$selected_dir, centroided = centroided),
               warning = function(w) {
                 if (grepl("tag is not supported", conditionMessage(w), fixed = TRUE)) {
                   invokeRestart("muffleWarning")
                 }
               }
             )
             # spot coordinates from the mzML ids, used by the plate map
             spec_raw <- addMzmlSpots(spec_raw)
             appData$centroided <- centroided
             checkEmpty <- FALSE
           })
    
    unique_mz <- purrr::map_lgl(spec_raw, function(x) {
      mz <- mass(x)
      umz <- unique(mz)
      
      # first level check, if length dont match we are done
      if(length(mz) != length(umz)) {
        return(FALSE)
      }

      # detailed check: if all elements match all mz must be unique
      if(all(mz == umz)) {
        return(TRUE)
      }

      return(FALSE)
    })

    if(any(!unique_mz)) {
      warning("Not all mz are unique. Did you use tims resolved data?\n")
        appData$info_state <- "loadErrorUniqueMz"
        hide_spinner()
        return()
    }
    
    
    # concentrations: from the mapping (per file) or from the file names
    files <- names(spec_raw)
    spots <- vapply(spec_raw, function(x) {
      patch <- metaData(x)[["patch"]]
      if (is.null(patch) || length(patch) == 0L) "" else paste(patch, collapse = " ")
    }, character(1))

    # a sample sheet in the data folder is used when none was uploaded
    if (is.null(mapping) || length(mapping) == 0L) {
      auto <- findSampleSheet(appData$selected_dir)
      mapping <- auto$mapping
      if (!is.null(auto$error)) {
        warning("Ignoring ", auto$file, " in the data folder: ", auto$error, "\n")
      }
    }

    concentrations <- NULL
    if (!is.null(mapping) && length(mapping) > 0L) {
      concentrations <- resolveMapping(mapping, files)
      problem <- attr(concentrations, "problem")
      if (is.null(concentrations) || !is.null(problem)) {
        appData$load_problem <- problem
        appData$info_state <- "loadErrorMapping"
        hide_spinner()
        return()
      }

      unknown <- attr(concentrations, "unknown")
      if (!is.null(unknown) && length(unknown) > 0L) {
        message("Sample sheet lists files that are not in the folder: ",
                paste(unknown, collapse = ", "))
      }
      message("Changed spectra names using the sample sheet.\n")
    } else {
      concentrations <- suppressWarnings(as.numeric(files))
      if (anyNA(concentrations)) {
        appData$load_problem <- paste0(
          "These names are not numeric concentrations: ",
          paste(unique(files[is.na(concentrations)]), collapse = ", "),
          ". Provide a sample sheet with file names and concentrations.")
        appData$info_state <- "loadErrorNum"
        hide_spinner()
        return()
      }
    }

    names(spec_raw) <- concentrations
    appData$samples <- sampleSheet(files, concentrations, spots)


    appData$info_state <- "loaded"

    # make sure that the concentrations are in acending order
    spec_raw <- spec_raw[order(as.numeric(names(spec_raw)))]

    appData$org_conc <- as.numeric(names(spec_raw))

    appData$spec_all <- spec_raw
    if(checkEmpty) {
      message(m2araTimeNow(),  " check for empty spectra...\n")

      # MAD would be faster but may fail in some circumstances...
      peaks <- detectPeaks(appData$spec_all,
                           SNR = input$SNR,
                           method = "SuperSmoother")
      appData$spec_idx <- vapply(peaks,
                                 function(x) {
                                   ifelse(length(mz(x)) > 0, TRUE, FALSE)
                                 },
                                 FUN.VALUE = TRUE)
      message(m2araTimeNow(), " ",
              sum(appData$spec_idx), "/", length(appData$spec_all),
              " spectra retained.\n")
    } else {
      appData$spec_idx <- rep(TRUE, length(appData$spec_all))
    }


    hide_spinner()
  }
  return(appData)
}
