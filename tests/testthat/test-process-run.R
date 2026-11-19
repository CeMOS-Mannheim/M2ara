test_that("processRun processes a dataset into a MALDIassay object", {
  spec <- MALDIcellassay::Blank2022spec

  settings <- list(
    SNR = 3,
    SinglePointRecal = TRUE,
    normMz = 760.585,
    normTol = 0.1,
    normMeth = "mz",
    alignTol = 0.1,
    halfWindowSize = 20,
    peakMethod = "SuperSmoother",
    avgMethod = "mean",
    binTol = 100,
    VarFilterMethod = "mean"
  )
  preprocessing <- list(
    sqrtTrans = FALSE,
    smooth = TRUE,
    rmBl = TRUE,
    monoisotopicFilter = FALSE
  )

  steps <- character()
  out <- processRun(
    spec_all = spec,
    spec_idx = rep(TRUE, length(spec)),
    centroided = FALSE,
    preprocessing = preprocessing,
    settings = settings,
    dir = NULL,
    progress = function(step) steps <<- c(steps, step)
  )

  expect_true(out$ok)
  expect_s4_class(out$res, "MALDIassay")
  expect_equal(steps, c("start", "preprocess", "aggregate", "intensity",
                        "fit", "statistics", "done"))

  stats <- getStatistics(out$res)
  expect_gt(nrow(stats), 0)
  expect_true(all(c("mz", "mzIdx", "pEC50", "R2", "log2FC", "CRS") %in% names(stats)))
})

test_that("processRun reports unusable settings instead of throwing", {
  spec <- MALDIcellassay::Blank2022spec
  settings <- list(
    SNR = 3,
    SinglePointRecal = TRUE,
    normMz = 1,        # nothing matches this mass
    normTol = 0.01,
    normMeth = "mz",
    alignTol = 0.1,
    halfWindowSize = 20,
    peakMethod = "SuperSmoother",
    avgMethod = "mean",
    binTol = 100,
    VarFilterMethod = "mean"
  )

  out <- suppressWarnings(
    processRun(spec, rep(TRUE, length(spec)), centroided = FALSE,
               preprocessing = list(sqrtTrans = FALSE, smooth = TRUE,
                                    rmBl = TRUE, monoisotopicFilter = FALSE),
               settings = settings)
  )

  expect_false(out$ok)
  expect_equal(out$error, "recal")
})

test_that("the queue: processCacheKey is stable and depends on settings and files", {
  dir <- file.path(tempdir(), "m2ara_cache_source")
  unlink(dir, recursive = TRUE)
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  writeLines("a", file.path(dir, "file_a.txt"))
  writeLines("b", file.path(dir, "file_b.txt"))

  settings <- list(SNR = 3, normMz = 760.585)
  preprocessing <- list(smooth = TRUE)

  key <- processCacheKey(dir, settings, preprocessing)
  expect_type(key, "character")
  expect_equal(key, processCacheKey(dir, settings, preprocessing))
  expect_false(identical(key, processCacheKey(dir, list(SNR = 4, normMz = 760.585),
                                             preprocessing)))

  # a changed file invalidates the key
  Sys.setFileTime(file.path(dir, "file_a.txt"), Sys.time() + 5)
  expect_false(identical(key, processCacheKey(dir, settings, preprocessing)))
})

test_that("the queue: cached results can be stored and read", {
  cache_dir <- file.path(tempdir(), "m2ara_cache_store")
  unlink(cache_dir, recursive = TRUE)
  dir.create(cache_dir, recursive = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  withr::local_envvar(M2ARA_CACHE_DIR = cache_dir)

  # a dataset keeps its last result; another key invalidates it
  data_dir <- file.path(tempdir(), "m2ara_cache_data")
  unlink(data_dir, recursive = TRUE)
  dir.create(data_dir, recursive = TRUE)
  writeLines("a", file.path(data_dir, "1.mzML"))

  expect_null(processCacheGet("missing", data_dir))
  expect_true(processCachePut("key-1", list(value = 42), data_dir))
  expect_equal(processCacheGet("key-1", data_dir), list(value = 42))

  # a different key (other settings) does not return the old result ...
  expect_null(processCacheGet("key-2", data_dir))
  # ... and storing it replaces the previous entry
  processCachePut("key-2", list(value = 43), data_dir)
  expect_equal(processCacheGet("key-2", data_dir), list(value = 43))
  expect_null(processCacheGet("key-1", data_dir))

  # one entry per dataset, not per run
  expect_length(list.files(cache_dir, pattern = "\\.rds$"), 1L)
  expect_match(processCacheSummary(), "1 cached dataset")
})
test_that("processRunAsync runs the pipeline in a background process", {
  skip_on_cran()

  spec <- MALDIcellassay::Blank2022spec
  settings <- list(
    SNR = 3, SinglePointRecal = TRUE, normMz = 760.585, normTol = 0.1,
    normMeth = "mz", alignTol = 0.1, halfWindowSize = 20,
    peakMethod = "SuperSmoother", avgMethod = "mean", binTol = 100,
    VarFilterMethod = "mean"
  )
  preprocessing <- list(sqrtTrans = FALSE, smooth = TRUE, rmBl = TRUE,
                        monoisotopicFilter = FALSE)

  job <- processRunAsync(spec, rep(TRUE, length(spec)), centroided = FALSE,
                         preprocessing = preprocessing, settings = settings)
  on.exit(processJobCleanup(job), add = TRUE)

  for (i in seq_len(300)) {
    if (processJobDone(job)) break
    Sys.sleep(0.2)
  }

  expect_true(processJobDone(job))
  expect_equal(processJobProgress(job), "done")

  out <- processJobResult(job)
  expect_true(isTRUE(out$ok))
  expect_s4_class(out$res, "MALDIassay")
})
test_that("the cache key depends on the pipeline version", {
  dir <- withr::local_tempdir()
  writeLines("a", file.path(dir, "1.mzML"))

  settings <- list(SNR = 3, concUnits = "uM")
  preprocessing <- list(smooth = TRUE)

  key <- processCacheKey(dir, settings, preprocessing)
  expect_true(nzchar(key))
  expect_true(grepl(utils::packageVersion("M2ara"), processPipelineVersion(),
                    fixed = TRUE))
})