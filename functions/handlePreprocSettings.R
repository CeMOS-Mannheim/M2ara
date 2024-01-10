handlePreprocSettings <- function(preproc_settings, parameter) {
  stopifnot(is.character(parameter))
  stopifnot(length(parameter) == 1)
  options <- c("smooth", "rmBl", "sqrtTrans", "monoisotopicFilter")
  if(!parameter %in% options) {
    stop("Unknown preprocessing setting: ", parameter, "\n",
         "Known options are ", paste(options, collapse = ", "))
  }

  if(parameter %in% preproc_settings) {
    return(TRUE)
  }
  return(FALSE)
}

handleDefaultPreprocSetting <- function(smooth_lgl, rmBl_lgl, sqrtTrans_lgl, monoisotopicFilter_lgl) {
  sel <- c(smooth_lgl, rmBl_lgl, sqrtTrans_lgl, monoisotopicFilter_lgl)

  res <-  c("smooth", "rmBl", "sqrtTrans", "monoisotopicFilter")
  names(res) <- c("Smooth", "Remove baseline", "Sqrt-trans.", "Monoisotopic")

  return(list(res[sel]))
}
