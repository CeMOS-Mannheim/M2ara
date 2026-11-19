test_that("the curves are prepared for the clustering", {
  res <- MALDIcellassay::Blank2022res
  curves <- curveClusterData(res)

  expect_s3_class(curves, "data.frame")
  expect_true(all(c("mzIdx", "x", "y") %in% names(curves)))
  # one curve per fitted feature
  expect_equal(length(unique(curves$mzIdx)),
               length(MALDIcellassay::getCurveFits(res)))
  # normalised to 0..1 per feature
  expect_true(min(curves$y) >= 0 && max(curves$y) <= 1)

  # only a processed dataset has curve fits to cluster
  expect_error(curveClusterData(MALDIcellassay::Blank2022spec))
})

test_that("clustering from the prepared data matches the convenience function", {
  res <- MALDIcellassay::Blank2022res
  curves <- curveClusterData(res)

  fromData <- clusterCurvesFromData(curves, nClusters = 4, centroidMethod = "mean")
  expect_length(fromData, 3L)  # k = 2, 3, 4

  assignments <- extractLaClusters(fromData, k = 3)
  expect_equal(nrow(assignments), length(unique(curves$mzIdx)))
  expect_true(all(as.integer(assignments$cluster) %in% 1:3))

  # the same input gives the same clusters (fixed seed)
  again <- clusterCurvesFromData(curves, nClusters = 4, centroidMethod = "mean")
  expect_equal(extractLaClusters(again, k = 3)$cluster, assignments$cluster)
})

test_that("progress is reported while clustering", {
  res <- MALDIcellassay::Blank2022res
  curves <- curveClusterData(res)

  stages <- character(0)
  clusterCurvesFromData(curves, nClusters = 3, centroidMethod = "mean",
                        progress = function(step) stages <<- c(stages, step))

  expect_equal(stages, c("curves", "distances", "clustering", "done"))
})

test_that("the clustering runs in a background process", {
  skip_on_cran()

  res <- MALDIcellassay::Blank2022res
  curves <- curveClusterData(res)

  job <- clusterCurvesAsync(curves, nClusters = 3, centroidMethod = "mean")
  on.exit(clusterJobCleanup(job), add = TRUE)

  for (i in seq_len(300)) {
    if (clusterJobDone(job)) break
    Sys.sleep(0.5)
  }

  expect_true(clusterJobDone(job))
  models <- clusterJobResult(job)
  expect_length(models, 2L)  # k = 2, 3
})
