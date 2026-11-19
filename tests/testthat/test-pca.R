test_that("generatePCA explains variance against the total variance", {
  res <- MALDIcellassay::Blank2022res
  pca <- generatePCA(res, num_PC = 3, alpha = 1e-2)

  intmat <- MALDIcellassay::getIntensityMatrix(res)
  spca <- sparsepca::spca(X = intmat, k = 3, alpha = 1e-2, beta = 1e-3,
                          center = TRUE, scale = TRUE, max_iter = 750,
                          tol = 5e-5, verbose = FALSE)
  total <- sum(scale(intmat)^2) / (nrow(intmat) - 1)
  expected <- pmin(spca$sdev^2 / total * 100, 100)

  expect_equal(pca$explainedVar, expected, tolerance = 1e-8)
  # the components of a sparse PCA do not explain all of the variance
  expect_lt(sum(pca$explainedVar), 100)
})

test_that("generatePCA makes the largest loading of every component positive", {
  res <- MALDIcellassay::Blank2022res
  pca <- generatePCA(res, num_PC = 3, alpha = 1e-2)

  biggest <- vapply(pca$loadings, function(v) v[which.max(abs(v))], numeric(1))
  expect_true(all(biggest > 0))
})

test_that("generatePCA addresses the features of the curves", {
  res <- MALDIcellassay::Blank2022res
  pca <- generatePCA(res, num_PC = 3, alpha = 1e-2)

  expect_false(anyNA(pca$mzIdx))
  expect_equal(length(pca$mzIdx), ncol(MALDIcellassay::getIntensityMatrix(res)))
  expect_true(all(pca$mzIdx %in% getStatistics(res)$mzIdx))

  loadings <- extractLoadings(pca, res, "PC1", "PC2")
  expect_equal(names(loadings), c("mzIdx", "PC1", "PC2"))
  expect_equal(nrow(dplyr::left_join(getStatistics(res), loadings, by = "mzIdx")),
               nrow(getStatistics(res)))
})

test_that("empty components are recognised", {
  res <- MALDIcellassay::Blank2022res
  pca <- generatePCA(res, num_PC = 5, alpha = 1e-1)

  expect_true(any(pca$active == 0))
  expect_true(all(nonEmptyComponents(pca) %in% names(pca$active)[pca$active > 0]))
})

test_that("the log transform is applied on the intensity matrix", {
  res <- MALDIcellassay::Blank2022res
  plain <- generatePCA(res, num_PC = 3, alpha = 1e-2, transform = "as_processed")
  logged <- generatePCA(res, num_PC = 3, alpha = 1e-2, transform = "log10")

  expect_equal(plain$transform, "as_processed")
  expect_equal(logged$transform, "log10")
  expect_false(isTRUE(all.equal(plain$loadings, logged$loadings)))
})

test_that("generatePCA needs more spectra than components", {
  res <- MALDIcellassay::Blank2022res
  n <- nrow(MALDIcellassay::getIntensityMatrix(res))

  expect_error(generatePCA(res, num_PC = n), "More spectra than components")
})
test_that("generatePCA reports the parameters of the run", {
  res <- MALDIcellassay::Blank2022res
  pca <- generatePCA(res, num_PC = 3, alpha = 1e-2, beta = 5e-3)

  expect_equal(pca$alpha, 1e-2)
  expect_equal(pca$beta, 5e-3)
  expect_equal(pca$transform, "as_processed")
})
test_that("the PCA plot refuses the same component on both axes", {
  res <- MALDIcellassay::Blank2022res
  pca <- generatePCA(res, num_PC = 3, alpha = 1e-2)

  identical_axes <- pcaPlot(pca,
                            conc = factor(MALDIcellassay::getConc(res)),
                            x = "PC1", y = "PC1",
                            ellipseLevel = 0.67,
                            spots = MALDIcellassay::getSpots(res))
  expect_s3_class(identical_axes, "plotly")
  expect_equal(identical_axes$x$data[[1]]$mode, "text")

  normal <- pcaPlot(pca,
                    conc = factor(MALDIcellassay::getConc(res)),
                    x = "PC1", y = "PC2",
                    ellipseLevel = 0.67,
                    spots = MALDIcellassay::getSpots(res))
  expect_gt(length(normal$x$data), 1)
})
test_that("active features use a threshold relative to the component", {
  res <- MALDIcellassay::Blank2022res
  pca <- generatePCA(res, num_PC = 5, alpha = 1e-2, transform = "log10")

  absLoadings <- abs(as.matrix(pca$loadings))
  expected <- colSums(absLoadings >= 0.01 * apply(absLoadings, 2, max))

  expect_equal(as.integer(pca$active), as.integer(expected))
  # far fewer features than non-zero values would suggest
  expect_true(all(pca$active > 0))
})