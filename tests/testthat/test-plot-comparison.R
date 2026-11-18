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
