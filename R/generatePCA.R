# Sparse PCA of the single spectra ---------------------------------------------
#
# The intensity matrix has one row per spectrum and one column per feature, i.e.
# the same features the curves are fitted for. Features are therefore addressed
# by their index in `getAllMz()`, no matching or rounding is involved.

generatePCA <- function(res, num_PC, alpha = 1e-2, beta = 1e-3,
                        transform = c("as_processed", "log10"),
                        verbose = FALSE, max_iter = 750) {
  transform <- match.arg(transform)

  intmat <- getIntensityMatrix(res)

  if (nrow(intmat) <= num_PC) {
    stop("More spectra than components are needed for a PCA (", nrow(intmat),
         " spectra, ", num_PC, " components).", call. = FALSE)
  }

  if (transform == "log10") {
    if (any(intmat < 0, na.rm = TRUE)) {
      stop("A log transform needs non-negative intensities.", call. = FALSE)
    }
    intmat <- log10(intmat + 1)
  }

  # the intensity matrix of detected peaks can contain NA (peak missing in a
  # spectrum) - treat them as no signal
  naReplaced <- sum(is.na(intmat))
  if (naReplaced > 0L) {
    intmat[is.na(intmat)] <- 0
  }

  # constant features would divide by zero when scaling and carry no information
  keep <- apply(intmat, 2, function(v) {
    s <- stats::sd(v)
    is.finite(s) && s > 0
  })
  intmat <- intmat[, keep, drop = FALSE]

  # features are addressed by their index in the feature list of the curves
  mzIdx <- match(as.numeric(colnames(intmat)), getAllMz(res))
  if (anyNA(mzIdx)) {
    stop("The PCA features do not match the features of the curves.",
         call. = FALSE)
  }

  pca <- sparsepca::spca(X = intmat,
                         k = num_PC,
                         alpha = alpha,
                         beta = beta,
                         center = TRUE,
                         scale = TRUE,
                         max_iter = max_iter,
                         tol = 5e-5,
                         verbose = verbose)

  scores <- tibble::as_tibble(pca$scores, .name_repair = "minimal")
  loadings <- as.data.frame(pca$loadings)
  rownames(loadings) <- colnames(intmat)

  pcNames <- paste0("PC", seq_len(ncol(loadings)))
  colnames(scores) <- pcNames
  colnames(loadings) <- pcNames

  # the sign of a component is arbitrary; make the largest loading positive so
  # that scores, loadings and the peak table stay comparable between runs
  for (j in seq_along(pcNames)) {
    if (loadings[which.max(abs(loadings[[j]])), j] < 0) {
      loadings[[j]] <- -loadings[[j]]
      scores[[j]] <- -scores[[j]]
    }
  }

  # explained variance: relative to the variance of the whole (standardised)
  # matrix, not to the k computed components
  scaled <- scale(intmat, center = TRUE, scale = TRUE)
  totalVar <- sum(scaled^2) / (nrow(scaled) - 1)
  vars <- pca$sdev^2
  vars[is.na(vars)] <- 0
  explainedVar <- pmin(vars / totalVar * 100, 100)

  # a component without any active feature carries no information
  active <- colSums(abs(as.matrix(loadings)) > 1e-8)

  if (verbose) {
    cat("Active features per component:",
        paste(pcNames, active, sep = ": ", collapse = ", "), "\n")
  }

  list(scores = scores,
       loadings = loadings,
       explainedVar = explainedVar,
       mzIdx = mzIdx,
       active = active,
       dropped = sum(!keep),
       naReplaced = naReplaced,
       transform = transform,
       alpha = alpha,
       beta = beta)
}

# loadings of the given components, joined to the curves by feature index
extractLoadings <- function(pca, res, sel1, sel2) {
  sel <- as.character(c(sel1, sel2))

  out <- pca$loadings[, sel, drop = FALSE]
  out <- cbind(mzIdx = pca$mzIdx, out)

  tibble::as_tibble(out)
}

# components without any active loading are not offered in the UI
nonEmptyComponents <- function(pca) {
  keep <- pca$active > 0
  if (!any(keep)) {
    return(colnames(pca$loadings))
  }
  colnames(pca$loadings)[keep]
}
