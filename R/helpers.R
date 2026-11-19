# One button: as soon as a folder is selected, "Process spectra" loads the
# spectra (if necessary) and processes them.
handleButtonStatus <- function(appData) {
  disable("process")

  observeEvent(appData$info_state, {
    if (appData$info_state %in% c("dir_set", "loaded", "processed")) {
      enable("process")
    } else {
      disable("process")
    }
  }, ignoreNULL = FALSE)
}

setConcentrationUnit <- function(appData, input) {
  if(!input$concUnits == "M") {
    message("Changing concentrations to ", input$concUnits, ".\n")
  }

  unitFactor <- switch (input$concUnits,
                        "M" = 1,
                        "mM" = 1e-3,
                        "uM" = 1e-6,
                        "nM" = 1e-9,
                        "pM" = 1e-12,
                        "fM" = 1e-15,
                        1)

  # change concentrations according to unit
  names(appData$spec_all) <- appData$org_conc * unitFactor

  return(appData)
}

checkSpecNamesNumeric <- function(dir, fileFormat) {
  # check if all spectra names/folders have a numeric as name
  # we assume that this numeric is the corresponding concentration
  switch(fileFormat,
         "bruker" = {
           dirs <- basename(list.dirs(dir, recursive = FALSE))

           # convert folder names to numeric.
           # this will return NA if the folder name could not be converted.
           num <- suppressWarnings(as.numeric(dirs))
         },
         "mzml" = {
           files <- basename(list.files(dir, recursive = FALSE))
           files <- tools::file_path_sans_ext(files)
           # convert folder names to numeric.
           # this will return NA if the folder name could not be converted.
           num <- suppressWarnings(as.numeric(files))
         })

  hasNumericNames <- !any(is.na(num))

  return(hasNumericNames)
}

checkSpecNamesFormat <- function(dir, fileFormat) {
  # check if all spectra names/folders have the correct file extension
  # for bruker files we just assume that if we have folders and we find a fid
  # we are good to go
  switch(fileFormat,
         "bruker" = {
           dirs <- basename(list.dirs(dir, recursive = FALSE))

           fidFiles <- list.files(dir, pattern = "fid", recursive = TRUE)

           if(length(dirs) < 1 | length(fidFiles) < 1) {
             return(FALSE)
           }

           return(length(fidFiles)>=length(dirs))
         },
         "mzml" = {
           files <- basename(list.files(dir, pattern = ".mzml|.mzML|.MZML",  recursive = FALSE))

           if(length(files) > 0) {
             return(TRUE)
           }

           return(FALSE)
         })
}

# Merge the meta data lists of several spectra: union of the keys, unique
# values per key. MALDIquant keeps an equivalent helper internal, the app only
# needs the merged values for the measurement summary.
mergeMetaData <- function(metaData) {
  if (length(metaData) == 0L) {
    return(list())
  }

  keys <- unique(unlist(lapply(metaData, names)))
  merged <- lapply(keys, function(key) {
    unique(unlist(lapply(metaData, function(x) x[[key]])))
  })
  names(merged) <- keys
  merged
}

checkMetaData <- function(object) {
  # check if meta data is coming from bruker device
  metaData <- mergeMetaData(
    lapply(getAvgSpectra(object),
           function(x) {
             x@metaData
           })
  )

  terms <- c(metaData$instrument, metaData$spectrometerType,
             metaData$tofMode, metaData$acquisitionMethod,
             metaData$laserAttenuation, metaData$laserShots,
             metaData$laserShots, metaData$path)

  if(any(is.null(terms))) {
    return(FALSE)
  }

 return(TRUE)
}

#' Extract directory path
#'
#' @param object Object of class MALDIassay
#'
#' @return
#' List, containing the data used to do the fits as well as the nlpr curve fit .
getDirectory <- function(object) {
  if (!methods::is(object, "MALDIassay")) {
    stop("object must be of class 'MALDIassay'", call. = FALSE)
  }
  return(object@settings$dir)
}
# current time for console messages (kept local instead of using an internal of
# MALDIcellassay)
m2araTimeNow <- function() {
  format(Sys.time(), "%H:%M")
}