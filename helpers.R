checkSpecNames <- function(dir) {
  # function to check if all spectra names/folders have a numeric as name
  # we assume that this numeric is the corresponding concentration

  dirs <- basename(list.dirs(dir, recursive = FALSE))

  # convert folder names to numeric.
  # this will return NA if the folder name could not be converted.
  numDirs <- suppressWarnings(as.numeric(dirs))

  return(any(is.na(numDirs)))
}
