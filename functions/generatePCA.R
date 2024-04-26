generatePCA <- function(res, num_PC, alpha = 1e-3, beta = 1e-3, verbose = FALSE, max_iter = 750) {

  intmat <- getIntensityMatrix(res)

  cat("Performing PCA...\n")

  pca <- spca(X = intmat,
       k = num_PC,
       alpha = alpha,
       beta = beta,
       center = TRUE,
       scale = FALSE,
       max_iter = max_iter,
       tol = 5e-5,
       verbose = verbose)

  scores <- as_tibble(pca$scores, .name_repair = "minimal")
  colnames(scores) <- paste0("PC", 1:num_PC)

  loadings <- as_tibble(pca$loadings, rownames = NA)
  rownames(loadings) <- colnames(intmat)
  colnames(loadings) <- paste0("PC", 1:num_PC)

  vars <- pca$sdev^2
  # make sure to not have NA values
  vars <- ifelse(is.na(vars), 0, vars)
  percExp<- vars/sum(vars)*100

  cat("Done!\n")
  return(list(scores = scores,
              loadings = loadings,
              explaindVar = percExp))
}

extractLoadings <- function(pca, res, sel1, sel2) {
  sel <- c(sel1, sel2)

  res <- pca$loadings %>%
    as_tibble(rownames = "mz") %>%
    mutate(mz = as.numeric(mz)) %>%
    mutate(mzIdx = match.closest(x = mz,
                                 table = getAllMz(res),
                                 tolerance = 0.1)) %>%
    select(mzIdx, {{sel}})

  return(res)
}
