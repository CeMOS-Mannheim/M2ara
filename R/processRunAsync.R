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
    function(data_file, progress_file, result_file) {
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
      result_file = result_file
    ),
    supervise = TRUE,
    stdout = tempfile(pattern = "m2ara_stdout_", fileext = ".log"),
    stderr = tempfile(pattern = "m2ara_stderr_", fileext = ".log")
  )

  structure(
    list(
      job = job,
      data_file = data_file,
      result_file = result_file,
      progress_file = progress_file
    ),
    class = "m2ara_process_job"
  )
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
  unlink(c(job$data_file, job$result_file, job$progress_file))
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
