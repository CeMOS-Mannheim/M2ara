# Spot coordinates of mzML files ----------------------------------------------
#
# MALDIquantForeign does not keep the spot of an mzML spectrum, so the plate map
# had no coordinates for mzML data. The spot is part of the spectrum id
# (`spotID="chip=0 spot=K11"`); it is read from the file and stored as the
# `patch` metadata, which is where `extractSpots()` looks for it.

# spot labels of one mzML file, one entry per spectrum (document order)
mzmlFileSpots <- function(file) {
  if (!file.exists(file)) {
    return(character(0))
  }

  lines <- tryCatch(readLines(file, warn = FALSE), error = function(e) character(0))
  hits <- regmatches(lines, regexec('spotID="[^"]*spot=([^";]+)"', lines))
  hits <- hits[lengths(hits) == 2L]

  if (length(hits) == 0L) {
    return(character(0))
  }

  trimws(vapply(hits, `[`, character(1), 2L))
}

# attach the spot of every spectrum as `patch` metadata
addMzmlSpots <- function(spec) {
  files <- vapply(spec, function(x) {
    file <- metaData(x)[["file"]]
    if (is.null(file) || length(file) == 0L) NA_character_ else file
  }, character(1))

  if (!any(!is.na(files))) {
    return(spec)
  }

  spot_cache <- list()

  for (i in seq_along(spec)) {
    file <- files[i]
    if (is.na(file)) {
      next
    }

    if (is.null(spot_cache[[file]])) {
      spot_cache[[file]] <- mzmlFileSpots(file)
    }
    spots <- spot_cache[[file]]

    if (length(spots) == 0L) {
      next
    }

    number <- metaData(spec[[i]])[["numberInFile"]]
    if (is.null(number) || length(number) == 0L || is.na(number)) {
      number <- i
    }
    number <- as.integer(number)

    if (number < 1L || number > length(spots)) {
      next
    }

    meta <- metaData(spec[[i]])
    meta[["patch"]] <- spots[number]
    metaData(spec[[i]]) <- meta
  }

  spec
}
