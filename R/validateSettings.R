# Checks of the processing settings ---------------------------------------------
#
# Settings are only bounded by the widgets in the sidebar. A value like SNR = 0
# passes the UI but makes the pipeline fail with a curve fitting error, so the
# settings are checked before a run starts and the reason is reported.

validateProcessingSettings <- function(settings) {
  problems <- character(0)

  if (!isFiniteNumber(settings$SNR) || settings$SNR <= 0) {
    problems <- c(problems, "SNR has to be a number greater than 0.")
  }
  if (!isFiniteNumber(settings$normMz) || settings$normMz <= 0) {
    problems <- c(problems, "The normalization m/z has to be greater than 0.")
  }
  if (!isFiniteNumber(settings$normTol) || settings$normTol <= 0) {
    problems <- c(problems, "The normalization tolerance has to be greater than 0.")
  }
  if (!isFiniteNumber(settings$halfWindowSize) || settings$halfWindowSize < 1) {
    problems <- c(problems, "The peak window size has to be at least 1.")
  }
  if (!isFiniteNumber(settings$alignTol) || settings$alignTol < 0) {
    problems <- c(problems, "The alignment tolerance cannot be negative.")
  }
  if (!isFiniteNumber(settings$binTol) || settings$binTol <= 0) {
    problems <- c(problems, "The bin tolerance has to be greater than 0.")
  }

  problems
}

isFiniteNumber <- function(x) {
  length(x) == 1L && is.numeric(x) && !is.na(x) && is.finite(x)
}

# mass range of a list of spectra
spectraMassRange <- function(spectra) {
  if (is.null(spectra) || length(spectra) == 0L) {
    return(NULL)
  }

  ranges <- lapply(spectra, function(s) if (length(mass(s)) == 0L) NULL else range(mass(s)))
  ranges <- Filter(Negate(is.null), ranges)
  if (length(ranges) == 0L) {
    return(NULL)
  }

  c(min(vapply(ranges, `[[`, numeric(1), 1L)),
    max(vapply(ranges, `[[`, numeric(1), 2L)))
}
