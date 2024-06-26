checkSpecNames <- function(dir) {
  # function to check if all spectra names/folders have a numeric as name
  # we assume that this numeric is the corresponding concentration

  dirs <- basename(list.dirs(dir, recursive = FALSE))

  # convert folder names to numeric.
  # this will return NA if the folder name could not be converted.
  numDirs <- suppressWarnings(as.numeric(dirs))

  return(any(is.na(numDirs)))
}

checkMetaData <- function(object) {
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
