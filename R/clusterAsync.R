# Asynchronous clustering ------------------------------------------------------
#
# The clustering of the fitted curves (k = 2..10 over all features) takes a while
# for larger datasets and would freeze the app. It therefore runs in a background
# R process, like the processing: the curve data (small) is passed to the child,
# the child reports its stage to a file and the models are written to an RDS file.
# The server polls the stage and can kill the child.

clusterCurvesAsync <- function(curves, nClusters = 10, centroidMethod = "mean") {
  data_file <- tempfile(pattern = "m2ara_clust_", fileext = ".rds")
  result_file <- tempfile(pattern = "m2ara_clust_result_", fileext = ".rds")
  progress_file <- tempfile(pattern = "m2ara_clust_progress_", fileext = ".txt")
  stdout_file <- tempfile(pattern = "m2ara_clust_stdout_", fileext = ".log")
  stderr_file <- tempfile(pattern = "m2ara_clust_stderr_", fileext = ".log")

  saveRDS(list(curves = curves, nClusters = nClusters,
               centroidMethod = centroidMethod), data_file)
  writeLines("queued", progress_file)

  job <- callr::r_bg(
    function(data_file, progress_file, result_file, dev_path) {
      if (!is.null(dev_path) && nzchar(dev_path)) {
        if (!requireNamespace("pkgload", quietly = TRUE)) {
          stop("M2ara is not installed and pkgload is not available")
        }
        pkgload::load_all(dev_path, quiet = TRUE)
      }

      args <- readRDS(data_file)
      run <- utils::getFromNamespace("clusterCurvesFromData", "M2ara")

      models <- run(
        curves = args$curves,
        nClusters = args$nClusters,
        centroidMethod = args$centroidMethod,
        progress = function(step) {
          cat(step, "\n", sep = "", file = progress_file, append = TRUE)
        }
      )

      saveRDS(models, result_file)
      invisible(NULL)
    },
    args = list(
      data_file = data_file,
      progress_file = progress_file,
      result_file = result_file,
      dev_path = processDevelopmentPath()
    ),
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
    class = "m2ara_cluster_job"
  )
}

clusterJobLog <- function(job) {
  read_log <- function(file) {
    if (!file.exists(file)) {
      return(character())
    }
    tryCatch(readLines(file, warn = FALSE), error = function(e) character())
  }

  lines <- trimws(c(read_log(job$stdout_file), read_log(job$stderr_file)))
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0L) {
    return("")
  }
  paste(utils::tail(lines, 8), collapse = "\n")
}

clusterJobProgress <- function(job) {
  stages <- tryCatch(readLines(job$progress_file, warn = FALSE),
                     error = function(e) character())
  if (length(stages) == 0L) {
    return("queued")
  }
  trimws(stages[[length(stages)]])
}

clusterJobDone <- function(job) {
  !isTRUE(job$job$is_alive())
}

clusterJobResult <- function(job) {
  if (!file.exists(job$result_file)) {
    return(NULL)
  }
  tryCatch(readRDS(job$result_file), error = function(e) NULL)
}

clusterJobCancel <- function(job) {
  if (isTRUE(job$job$is_alive())) {
    job$job$kill()
  }
  invisible(NULL)
}

clusterJobCleanup <- function(job) {
  unlink(c(job$data_file, job$result_file, job$progress_file,
           job$stdout_file, job$stderr_file))
  invisible(NULL)
}

clusterStageLabel <- function(stage) {
  switch(
    as.character(stage),
    "queued" = "Preparing curves ...",
    "curves" = "Preparing curves ...",
    "distances" = "Computing the distance matrix ...",
    "clustering" = "Clustering the curves (k = 2..10) ...",
    "done" = "Preparing the plots ...",
    "Clustering ..."
  )
}
