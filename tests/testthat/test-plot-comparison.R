test_that("plotCurvesOverlay draws one curve per feature", {
  res <- MALDIcellassay::Blank2022res
  stats <- getStatistics(res)
  sel <- order(stats$CRS, decreasing = TRUE)[1:3]

  p <- plotCurvesOverlay(res, sel, errorbars = "none")
  expect_s3_class(p, "ggplot")

  colours <- unique(ggplot2::ggplot_build(p)$data[[1]]$colour)
  expect_equal(length(colours), length(sel))

  # normalisation scales every curve into [0, 1]
  p_norm <- plotCurvesOverlay(res, sel, errorbars = "none", normalize = TRUE)
  built <- ggplot2::ggplot_build(p_norm)$data[[1]]
  expect_true(all(built$y >= -1e-8 & built$y <= 1 + 1e-8))
})

test_that("plotCurvesOverlay validates its input", {
  res <- MALDIcellassay::Blank2022res
  expect_error(plotCurvesOverlay(res, integer(0)), "no feature selected")
  expect_error(plotCurvesOverlay(res, 1, errorbars = "nope"), "arg")
})

test_that("peakGrid combines the peak profiles of several features", {
  res <- MALDIcellassay::Blank2022res

  g <- peakGrid(res, 1:2, tol = 0.8, centroided = FALSE, showPeaks = TRUE)
  expect_s3_class(g, "patchwork")
  expect_equal(length(g$patches$plots) + 1L, 2L)

  expect_error(peakGrid(res, integer(0), tol = 0.8, centroided = FALSE,
                        showPeaks = TRUE), "no feature selected")
})
test_that("the plate map renders every metric and hints without a feature", {
  res <- MALDIcellassay::Blank2022res
  appData <- list(res = res, pca = NULL)

  # metrics that need a selected feature show a hint instead of an error
  for (stat in c("Selected-mz", "Outlier-mz")) {
    p <- plateMapPlot(appData, stat = stat, PCs = c("PC1", "PC2"))
    expect_s3_class(p, "ggplot")
    expect_s3_class(p$data, "waiver")
  }

  # the remaining metrics render a plate
  for (stat in c("Concentration", "Total Peak Intensity", "Recal-shift", "Outlier-all")) {
    p <- plateMapPlot(appData, stat = stat, PCs = c("PC1", "PC2"), mz_idx = 2)
    expect_s3_class(p, "ggplot")
    expect_s3_class(p$data, "data.frame")
  }
})
test_that("the plate map explains a missing PCA and missing outliers", {
  res <- MALDIcellassay::Blank2022res
  appData <- list(res = res, pca = NULL)

  # without PCA the PC metrics show a message instead of an empty plate
  for (stat in c("PC-x", "PC-y")) {
    p <- plateMapPlot(appData, stat = stat, PCs = c("PC1", "PC2"))
    expect_s3_class(p$data, "waiver")
  }

  # Outlier-all renders a plate even when a dataset has no outlier at all
  p <- plateMapPlot(appData, stat = "Outlier-all", PCs = c("PC1", "PC2"), mz_idx = 2)
  expect_s3_class(p$data, "data.frame")
})

test_that("the plate map switches to a log scale", {
  res <- MALDIcellassay::Blank2022res
  appData <- list(res = res, pca = NULL)

  lin <- plateMapPlot(appData, stat = "Total Peak Intensity", PCs = c("PC1", "PC2"),
                      log10 = FALSE)
  log <- plateMapPlot(appData, stat = "Total Peak Intensity", PCs = c("PC1", "PC2"),
                      log10 = TRUE)

  expect_s3_class(lin, "ggplot")
  expect_s3_class(log, "ggplot")
  # the transformed scale is applied (the scales differ)
  expect_false(identical(lin$scales$scales, log$scales$scales))
})
test_that("the overlay works for a single feature and can normalize it", {
  res <- MALDIcellassay::Blank2022res
  # just take the first feature of the dataset
  sel <- 1L

  p <- plotCurvesOverlay(res, sel, errorbars = "none")
  expect_s3_class(p, "ggplot")

  pNorm <- plotCurvesOverlay(res, sel, errorbars = "none", normalize = TRUE)
  expect_s3_class(pNorm, "ggplot")

  # after normalizing, the curve runs from 0 to 1
  built <- ggplot2::ggplot_build(pNorm)
  ys <- unlist(lapply(built$data, function(d) d$y))
  expect_true(min(ys) >= 0 && max(ys) <= 1)
})