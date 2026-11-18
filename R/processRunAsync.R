# Asynchronous processing ------------------------------------------------------
#
# processRun() in a background R process: the spectra are written to a temporary
# file (faster and more robust than passing them through the callr interface),
# the child reports every stage to a file and the result is written to an RDS
# file. The server polls processJobProgress()/processJobDone() and can kill the
# child with processJobCancel().

processRunAsync <- function(spec_all, spec_idx, centroided, preprocessing,
                            settings, dir = NULL) {
  data_file <- tempfile(pattern = "m2ara_process_", fileext = ".rds")
  result_file <- tempfile(pattern = "m2ara_result_", fileext = ".rds")
  progress_file <- tempfile(pattern = "m2ara_progress_", fileext = ".txt")
  stdout_file <- tempfile(pattern = "m2ara_stdout_", fileext = ".log")
  stderr_file <- tempfile(pattern = "m2ara_stderr_", fileext = ".log")

  saveRDS(
    list(
      spec_all = spec_all,
      spec_idx = spec_idx,
      centroided = centroided,
      preprocessing = preprocessing,
      settings = settings,
      dir = dir
    ),
    data_file
  )
  writeLines("queued", progress_file)

  job <- callr::r_bg(
    function(data_file, progress_file, result_file, dev_path) {
      # In development the app runs from a source checkout (pkgload) while the
      # installed package may be missing or outdated: load the sources then.
      if (!is.null(dev_path) && nzchar(dev_path)) {
        if (!requireNamespace("pkgload", quietly = TRUE)) {
          stop("M2ara is not installed and pkgload is not available")
        }
        pkgload::load_all(dev_path, quiet = TRUE)
      }

      args <- readRDS(data_file)

      process <- utils::getFromNamespace("processRun", "M2ara")
      out <- process(
        spec_all = args$spec_all,
        spec_idx = args$spec_idx,
        centroided = args$centroided,
        preprocessing = args$preprocessing,
        settings = args$settings,
        dir = args$dir,
        progress = function(step) {
          cat(step, "\n", sep = "", file = progress_file, append = TRUE)
        }
      )

      saveRDS(out, result_file)
      invisible(NULL)
    },
    args = list(
      data_file = data_file,
      progress_file = progress_file,
      result_file = result_file,
      dev_path = processDevelopmentPath()
    ),
    # R CMD check exports R_TESTS; a child process that inherits it would try to
    # run the test suite again (callr::rcmd_safe_env() unsets it)
    env = callr::rcmd_safe_env(),
    supervise = TRUE,
    stdout = stdout_file,
    stderr = stderr_file
  )

  structure(
    list(
      job = job,
      data_file = data_file,
      result_file = result_file,
      progress_file = progress_file,
      stdout_file = stdout_file,
      stderr_file = stderr_file
    ),
    class = "m2ara_process_job"
  )
}

# Path of the sources when M2ara is loaded from a checkout (development), NULL
# for an installed package: the child then uses the installed one.
processDevelopmentPath <- function() {
  ns <- tryCatch(asNamespace("M2ara"), error = function(e) NULL)
  if (is.null(ns)) {
    return(NULL)
  }

  path <- tryCatch(getNamespaceInfo(ns, "path"), error = function(e) NULL)
  if (is.null(path) || !dir.exists(path)) {
    return(NULL)
  }

  # an installed package has R/M2ara.rdb, a checkout has R/*.R sources
  r_dir <- file.path(path, "R")
  is_source <- dir.exists(r_dir) && length(list.files(r_dir, pattern = "\\.R$")) > 0L
  if (!is_source) {
    return(NULL)
  }

  normalizePath(path, winslash = "/")
}

# output of the child process (for error messages)
processJobLog <- function(job) {
  read_log <- function(file) {
    if (!file.exists(file)) {
      return(character())
    }
    tryCatch(readLines(file, warn = FALSE), error = function(e) character())
  }

  lines <- c(read_log(job$stdout_file), read_log(job$stderr_file))
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0L) {
    return("")
  }
  paste(utils::tail(lines, 8), collapse = "\n")
}

# last reported stage ("queued", "preprocess", ..., "done")
processJobProgress <- function(job) {
  stages <- tryCatch(readLines(job$progress_file, warn = FALSE), error = function(e) character())
  if (length(stages) == 0L) {
    return("queued")
  }
  trimws(stages[[length(stages)]])
}

processJobDone <- function(job) {
  !isTRUE(job$job$is_alive())
}

# result of a finished job: list(ok = TRUE, res = ...) / list(ok = FALSE, error = ...)
# or NULL when the child died without writing a result (crash, killed)
processJobResult <- function(job) {
  if (!file.exists(job$result_file)) {
    return(NULL)
  }
  tryCatch(readRDS(job$result_file), error = function(e) NULL)
}

processJobCancel <- function(job) {
  if (isTRUE(job$job$is_alive())) {
    job$job$kill()
  }
  invisible(NULL)
}

processJobCleanup <- function(job) {
  unlink(c(job$data_file, job$result_file, job$progress_file,
           job$stdout_file, job$stderr_file))
  invisible(NULL)
}

# human readable stage text for the UI
processStageLabel <- function(stage) {
  switch(
    stage,
    queued = "Starting ...",
    start = "Preparing data ...",
    preprocess = "Preprocessing spectra ...",
    aggregate = "Calculating average spectra ...",
    intensity = "Extracting intensities ...",
    fit = "Fitting curves ...",
    statistics = "Calculating peak statistics ...",
    done = "Done",
    stage
  )
}
# percentage range of the overall progress for each stage
processStageRange <- function(stage) {
  switch(
    stage,
    queued = c(0, 2),
    start = c(2, 5),
    preprocess = c(5, 20),
    aggregate = c(20, 45),
    intensity = c(45, 50),
    fit = c(50, 90),
    statistics = c(90, 100),
    done = c(100, 100),
    c(0, 100)
  )
}

# counters of the running child ("fitting <mz> (12/377)" on stdout)
processJobCounters <- function(job) {
  lines <- tryCatch(readLines(job$stdout_file, warn = FALSE), error = function(e) character())
  hits <- regmatches(lines, regexec("\\(([0-9]+)/([0-9]+)\\)", lines))
  hits <- hits[lengths(hits) == 3L]
  if (length(hits) == 0L) {
    return(list(done = NA_integer_, total = NA_integer_))
  }
  last <- hits[[length(hits)]]
  list(done = as.integer(last[2]), total = as.integer(last[3]))
}

# current progress: stage, counters and the overall percentage
processJobStatus <- function(job) {
  stage <- processJobProgress(job)
  counters <- processJobCounters(job)
  range <- processStageRange(stage)

  fraction <- 0
  if (identical(stage, "fit") && !is.na(counters$total) && counters$total > 0) {
    fraction <- min(1, counters$done / counters$total)
  }

  list(
    stage = stage,
    done = counters$done,
    total = counters$total,
    percent = round(range[1] + (range[2] - range[1]) * fraction)
  )
}