generatePCA <- function(res, num_PC) {

  intmat <- intensityMatrix(getSinglePeaks(res))

  # select only those m/z values that were fitted to curves
  all_mz <- round(as.numeric(colnames(intmat)), digits = 3)

  intmat <- intmat[,which(all_mz %in% getAllMz(res))]
  cat("Performing PCA...\n")
  scaled <- scale(intmat)

  pca <- prcomp(scaled, rank. = num_PC)

  scores <- as_tibble(pca$x)
  loadings <- as_tibble(pca$rotation, rownames = NA)
  vars <- pca$sdev^2
  percExp<- vars/sum(vars)*100

  cat("Done!\n")
  return(list(scores = scores,
              loadings = loadings,
              explaindVar = percExp))
}
