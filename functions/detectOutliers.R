#' Detect outliers
#'
#' @description This function takes a list of spectra and the
#' associated concentrations.
#' It extracts all intensities for all mz values per spectra.
#' Per concentration and mz value MALDIcellassay::calculateChauvenetCriterion
#' is called which returns a logical.
#' If the logical is TRUE the intensity is an outlier.
#'
#' @param spectra list of MALDIquant::MassSpectrum or MassPeaks
#' @param conc    numeric vector, concentrations
#' @param n       integer, number of mz values to consider
#' @param method  character, "mz" or "n"
#' @param mzIdx   integer of length 1, index of mz value to check if it is an
#'                outlier (only used if method = "mz")
#' @return        logicals vector, TRUE if mz value is an outlier
#'
#' @importFrom MALDIquant mass intensity isMassSpectrumList isMassPeaksList
#' @importFrom purrr map
#' @importFrom tibble tibble
#' @importFrom dplyr mutate group_by summarize bind_rows
#' @export

detectOutliers <- function(spectra, conc, n = 1, method = c("mz", "n"), mzIdx) {
  method <- match.arg(method)

  # check if spectra is a list of MALDIquant::MassSpectrum or MassPeaks
  stopifnot(isMassSpectrumList(spectra) | isMassPeaksList(spectra))

  # check if conc is a vector of numeric values
  stopifnot(is.numeric(conc))

  # check if n is a number
  stopifnot(is.numeric(n))

  # check if length of conc and spectra is the same
  if (length(conc) != length(spectra)) {
    stop("length of conc and spectra must be the same")
  }
  # check if n is smaller than the number of mz values
  if (n > length(mass(spectra[[1]]))) {
    stop("n must be smaller than the number of mz values")
  }
  # check if n is smaller than 1
  if (n < 1) {
    stop("n must be greater than 0")
  }

  # extract intensities for all mz values per spectra
  outlier <- map(seq_along(spectra), function(i) {
    tibble(mz = mass(spectra[[i]]),
           intensity = intensity(spectra[[i]]),
           conc = conc[i])
  }) %>%
    bind_rows(.id = "spectrum") %>%
    group_by(mz, conc) %>%
    mutate(outlier = calculateChauvenetCriterion(intensity)) %>%
    ungroup() %>%
    group_by(spectrum) %>%
    summarize(outlier = if (method == "mz") {
      # check if specified mz value is an outlier
      outlier <- outlier[mzIdx]
      # if outlier is NA set to FALSE
      outlier[is.na(outlier)] <- FALSE
      outlier
    } else {
      # if method is n
      # check if n mz values are an outlier
      sum(outlier) >= n
    }) %>%
    # ungroup
    ungroup() %>%
    # extract outlier column
    pull(outlier)

  return(outlier)
}


