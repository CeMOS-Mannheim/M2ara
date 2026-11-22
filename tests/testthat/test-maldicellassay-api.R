# M2ara uses a few internals of MALDIcellassay for the step-wise pipeline. This
# guard fails loudly when one of them is renamed or its arguments change, instead
# of the app breaking at runtime.

test_that("the MALDIcellassay internals used by M2ara are unchanged", {
  ns <- asNamespace("MALDIcellassay")

  expected <- list(
    ".repairMetaData" = "spec",
    ".aggregateSpectra" = c("spec", "averageMethod", "SNR", "monoisotopicFilter",
                            "binTol", "normMz", "normTol", "halfWindowSize",
                            "peakMethod", "verbose"),
    "calculatePeakStatistics" = c("curveFits", "singlePeaks", "spec"),
    ".detectPeaks" = c("spec", "SNR", "method", "halfWindowSize"),
    ".preprocess" = c("peaks_single", "spec", "SinglePointRecal", "normMz",
                      "normTol", "normMeth", "alignTol", "allowNoMatches",
                      "verbose")
  )

  for (fun in names(expected)) {
    expect_true(exists(fun, envir = ns, inherits = FALSE), info = fun)
    expect_true(is.function(get(fun, envir = ns)), info = fun)

    args <- names(formals(get(fun, envir = ns)))
    missing <- setdiff(expected[[fun]], args)
    expect_length(missing, 0L)
  }
})

test_that("M2ara does not use MALDIcellassay internals beyond the guarded ones", {
  rFiles <- list.files(test_path("..", "..", "R"), pattern = "[.]R$", full.names = TRUE)
  skip_if(length(rFiles) == 0L, "R sources not available")

  used <- unlist(lapply(rFiles, function(f) {
    lines <- readLines(f, warn = FALSE)
    hits <- regmatches(lines, gregexpr("MALDIcellassay:::([.[:alnum:]_]+)", lines))
    unlist(hits)
  }))

  used <- sub("MALDIcellassay:::", "", used)
  guarded <- c(".repairMetaData", ".aggregateSpectra", "calculatePeakStatistics",
               ".detectPeaks", ".preprocess")

  expect_length(setdiff(used, guarded), 0L)
})
