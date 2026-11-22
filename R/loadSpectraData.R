loadSpectraData <- function(input, appData, mapping) {
  if (appData$info_state %in% c("dir_set",
                                "loadErrorNum",
                                "loadErrorFormat")) {
    showM2araSpinner()

    checkEmpty <- input$checkEmpty

    # a sample sheet in the data folder counts like an uploaded one, so it has to
    # be found before the file names are checked
    if (is.null(mapping) || length(mapping) == 0L) {
      auto <- findSampleSheet(appData$selected_dir)
      mapping <- auto$mapping
      if (!is.null(auto$error)) {
        warning("Ignoring ", auto$file, " in the data folder: ", auto$error, "\n")
      }
    }

    # check if all spectra names are numeric/concentrations
    # for later: if pos and neg ctrls are included
    # checkSpecNames needs to return indices of the numeric folders
    if(!length(mapping)>0) {
      if (!checkSpecNamesNumeric(appData$selected_dir, input$fileFormat)) {
        warning("Found folder names that could not be converted to numeric and no mapping file found.
                All folders/spectra need to have concentrations as names or mapping file with conc. needed.\n")
        setInfoState(appData, "loadErrorNum")
        hideM2araSpinner()
        return()
      }
    }

    if (!checkSpecNamesFormat(appData$selected_dir, input$fileFormat)) {
      warning("Could not find data with '", input$fileFormat, "' file format.\n")
      setInfoState(appData, "loadErrorFormat")
      hideM2araSpinner()
      return()
    }

    switch(input$fileFormat,
           "bruker" = {
             # only sub folders that contain a fid file are datasets, every
             # other folder or file in the directory is ignored
             dirs <- list.dirs(appData$selected_dir, recursive = FALSE, full.names = TRUE)
             isDataset <- vapply(dirs, function(d) {
               any(tolower(list.files(d)) == "fid")
             }, logical(1))
             skipped <- basename(dirs)[!isDataset]
             appData$skipped <- skipped
             spec_raw <- loadSpectra(appData$selected_dir, filter = skipped)
             # at the moment we assume Bruker flex data to always be continuous
             appData$centroided <- FALSE
           },
           "mzml" = {
             files <- list.files(appData$selected_dir, recursive = FALSE)
             isMzml <- grepl("\\.mzml$", files, ignore.case = TRUE)
             skipped <- files[!isMzml]
             appData$skipped <- skipped

             # check if data is centroided for first file
             # assume rest is also centroided
             f <- file.path(appData$selected_dir, files[isMzml])
             centroided <- any(stringr::str_detect(readLines(f[1],
                                                             n = -1L),
                                                   pattern = "centroid spectrum"))

             if(centroided) {
               message("      found centroided data. Will not check for empty spectra.")
             }

             # the mzML files may contain chromatograms, the parser warns for
             # every one of them - that is noise and not actionable
             spec_raw <- withCallingHandlers(
               loadSpectraMzML(appData$selected_dir, filter = skipped,
                               centroided = centroided),
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

    if (length(appData$skipped) > 0L) {
      message("Skipped ", length(appData$skipped), " entry(ies) that are no ",
              input$fileFormat, " dataset: ",
              paste(utils::head(appData$skipped, 5), collapse = ", "),
              if (length(appData$skipped) > 5L) " ..." else "")
    }
    
    # files the user left out in the concentration dialog are not used at all
    if (length(appData$excluded_files) > 0L) {
      keep <- !excludedSpectrum(spec_raw, appData$excluded_files)
      if (any(!keep)) {
        message(m2araTimeNow(), " left out ", sum(!keep), " file(s): ",
                paste(utils::head(unique(names(spec_raw)[!keep]), 5), collapse = ", "),
                "\n")
        spec_raw <- spec_raw[keep]
      }
    }
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
        setInfoState(appData, "loadErrorUniqueMz")
        hideM2araSpinner()
        return()
    }
    
    
    # concentrations: from the mapping (per file) or from the file names
    files <- names(spec_raw)
    spots <- vapply(spec_raw, function(x) {
      patch <- metaData(x)[["patch"]]
      if (is.null(patch) || length(patch) == 0L) "" else paste(patch, collapse = " ")
    }, character(1))

    concentrations <- NULL
    if (!is.null(mapping) && length(mapping) > 0L) {
      concentrations <- resolveMapping(mapping, files)
      problem <- attr(concentrations, "problem")
      if (is.null(concentrations) || !is.null(problem)) {
        appData$load_problem <- problem
        setInfoState(appData, "loadErrorMapping")
        hideM2araSpinner()
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
        setInfoState(appData, "loadErrorNum")
        hideM2araSpinner()
        return()
      }
    }

    names(spec_raw) <- concentrations

    # the optional group column (e.g. biological vs technical replicate) is kept
    # in the exported sample sheet
    groups <- NULL
    if (!is.null(mapping) && identical(mapping$kind, "sheet") &&
        "group" %in% names(mapping$sheet)) {
      sheetGroups <- stats::setNames(mapping$sheet$group,
                                     tolower(stripDataExtension(mapping$sheet$file)))
      groups <- unname(sheetGroups[tolower(stripDataExtension(files))])
    }

    appData$samples <- sampleSheet(sheetFiles(files, appData$selected_dir, input$fileFormat),
                                   concentrations, spots, groups)
    appData$mapping_source <- if (is.null(mapping)) {
      "file names"
    } else if (identical(mapping$kind, "list")) {
      "concentration list"
    } else {
      "sample sheet"
    }


    setInfoState(appData, "loaded")

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


    hideM2araSpinner()
  }
  return(appData)
}
