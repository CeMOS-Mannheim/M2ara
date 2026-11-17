processMappingFile <- function(file) {
  req(file)
  ext <- tools::file_ext(file$datapath)

  if(!ext == "txt") {
    warning("Please upload a txt file")
    return()
  }

  mapping <- read_lines(file$datapath)

  mapping <- as.numeric(mapping)

  if(any(is.na(mapping))) {
    warning("Mapping file needs to contain concentrations only")
    return()
  }

  return(mapping)
}

