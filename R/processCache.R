# Result cache -----------------------------------------------------------------
#
# Processing a dataset takes a minute or more, but the result only depends on
# the spectra on disk and on the settings. The MALDIassay object is therefore
# cached per user, keyed by a fingerprint of the dataset files and the settings.

# M2ARA_CACHE_DIR overrides the location (used by the tests and for shared
# caches on a network drive)
processCacheDir <- function() {
  dir <- Sys.getenv("M2ARA_CACHE_DIR", unset = "")
  if (!nzchar(dir)) {
    dir <- file.path(tools::R_user_dir("M2ara", which = "cache"), "results")
  }
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  dir
}

# name, size and modification time of every file in the dataset directory
datasetFingerprint <- function(dir) {
  if (is.null(dir) || length(dir) == 0L || !dir.exists(dir)) {
    return(NULL)
  }

  files <- list.files(dir, recursive = TRUE, full.names = TRUE, all.files = FALSE)
  if (length(files) == 0L) {
    return(NULL)
  }

  info <- file.info(files)
  entries <- sprintf(
    "%s|%s|%s",
    sub(paste0(normalizePath(dir, winslash = "/"), "/"), "", normalizePath(files, winslash = "/")),
    format(info$size),
    format(info$mtime, "%Y%m%d%H%M%OS3")
  )

  paste(sort(entries), collapse = "\n")
}

# key of the cache entry, NULL when the dataset cannot be fingerprinted
processCacheKey <- function(dir, settings, preprocessing, centroided) {
  fingerprint <- datasetFingerprint(dir)
  if (is.null(fingerprint)) {
    return(NULL)
  }

  payload <- jsonlite::toJSON(
    list(
      dir = normalizePath(dir, winslash = "/"),
      files = fingerprint,
      settings = settings,
      preprocessing = preprocessing,
      centroided = isTRUE(centroided)
    ),
    auto_unbox = TRUE,
    null = "null"
  )

  hash_file <- tempfile()
  on.exit(unlink(hash_file), add = TRUE)
  writeLines(as.character(payload), hash_file)

  unname(tools::md5sum(hash_file))
}

processCacheGet <- function(key) {
  if (is.null(key) || is.na(key)) {
    return(NULL)
  }

  path <- file.path(processCacheDir(), paste0(key, ".rds"))
  if (!file.exists(path)) {
    return(NULL)
  }

  tryCatch(readRDS(path), error = function(e) NULL)
}

processCachePut <- function(key, value) {
  if (is.null(key) || is.na(key)) {
    return(invisible(FALSE))
  }

  path <- file.path(processCacheDir(), paste0(key, ".rds"))
  tryCatch(
    {
      saveRDS(value, path)
      invisible(TRUE)
    },
    error = function(e) {
      message("could not write the result cache: ", conditionMessage(e))
      invisible(FALSE)
    }
  )
}

# remove all cached results (Settings tab)
clearProcessCache <- function() {
  dir <- processCacheDir()
  files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  unlink(files)
  length(files)
}

processCacheSize <- function() {
  files <- list.files(processCacheDir(), pattern = "\\.rds$", full.names = TRUE)
  if (length(files) == 0L) {
    return(0)
  }
  sum(file.size(files), na.rm = TRUE)
}
