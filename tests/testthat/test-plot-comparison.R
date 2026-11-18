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

test_that("plotPeaksGrid returns a plotly grid for several features", {
  res <- MALDIcellassay::Blank2022res

  g <- plotPeaksGrid(res, 1:2, tol = 0.8, centroided = FALSE, showPeaks = TRUE)
  expect_s3_class(g, "plotly")

  # 2 x 1 panels, each with the traces of the concentration series
  expect_true(length(g$x$data) >= 2)
})
test_that("featureRowStyles maps m/z to the feature colours", {
  styles <- featureRowStyles(c(406.91, 428.00))

  expect_equal(names(styles), c("406.91", "428.00"))
  expect_equal(vapply(styles, `[[`, "", "colour"), c("#D55E00", "#009E73"))
  expect_true(all(grepl("^rgba\\(", vapply(styles, `[[`, "", "background"))))

  # the feature colours must differ from the viridis concentration colours
  expect_false(any(vapply(styles, `[[`, "", "colour") %in% c("#440154", "#21908C", "#FDE725")))
})