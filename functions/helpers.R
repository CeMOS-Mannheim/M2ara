setConcentrationUnit <- function(appData, input) {
  if(!input$concUnits == "M") {
    message("Changing concentrations to ", input$concUnits, ".\n")
  }

  unitFactor <- switch (input$concUnits,
                        "M" = 1,
                        "mM" = 1e-3,
                        "µM" = 1e-6,
                        "nM" = 1e-9,
                        "pM" = 1e-12,
                        "fM" = 1e-15)

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

checkMetaData <- function(object) {
  # check if meta data is coming from bruker device
  metaData <- MALDIquant:::.mergeMetaData(
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
