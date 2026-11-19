# Result cache -----------------------------------------------------------------
#
# Processing a dataset takes a minute or more, but the result only depends on
# the spectra on disk and on the settings. One result is cached per dataset: the
# file name is derived from the dataset, the entry remembers the key (data,
# settings, versions) it belongs to. A dataset therefore keeps its last result
# and old entries cannot pile up.

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

# md5 of an arbitrary string
.hashString <- function(text) {
  hash_file <- tempfile()
  on.exit(unlink(hash_file), add = TRUE)
  writeLines(text, hash_file)
  unname(tools::md5sum(hash_file))
}

# identifier of a dataset, used as the file name of its cache entry
datasetCacheId <- function(dir) {
  fingerprint <- datasetFingerprint(dir)
  if (is.null(fingerprint)) {
    return(NULL)
  }

  .hashString(paste(normalizePath(dir, winslash = "/"), fingerprint, sep = "\n"))
}

# Bump this when the structure of a cached result changes; it is part of the
# cache key, so old entries are ignored after an update of the app.
processCacheSchema <- 3L

# versions of the packages that produce a result
processPipelineVersion <- function() {
  versions <- tryCatch(
    c(as.character(utils::packageVersion("M2ara")),
      as.character(utils::packageVersion("MALDIcellassay"))),
    error = function(e) "unknown"
  )
  paste(c(processCacheSchema, versions), collapse = "-")
}

# stable identifier of the concentration mapping (it changes the result)
mappingHash <- function(mapping) {
  if (is.null(mapping) || length(mapping) == 0L) {
    return("none")
  }
  .hashString(paste(as.integer(serialize(mapping, NULL)), collapse = ","))
}

# key of the cache entry, NULL when the dataset cannot be fingerprinted
processCacheKey <- function(dir, settings, preprocessing, mapping = NULL) {
  fingerprint <- datasetFingerprint(dir)
  if (is.null(fingerprint)) {
    return(NULL)
  }

  .hashString(jsonlite::toJSON(
    list(
      dir = normalizePath(dir, winslash = "/"),
      files = fingerprint,
      settings = settings,
      preprocessing = preprocessing,
      mapping = mappingHash(mapping),
      pipeline = processPipelineVersion()
    ),
    auto_unbox = TRUE,
    null = "null"
  ))
}

processCachePath <- function(dir) {
  id <- datasetCacheId(dir)
  if (is.null(id)) {
    return(NULL)
  }
  file.path(processCacheDir(), paste0(id, ".rds"))
}

# the cached result of this dataset, but only when it belongs to the same key
processCacheGet <- function(key, dir) {
  if (is.null(key) || is.na(key)) {
    return(NULL)
  }

  path <- processCachePath(dir)
  if (is.null(path) || !file.exists(path)) {
    return(NULL)
  }

  entry <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(entry) || !identical(entry[["key"]], key)) {
    return(NULL)
  }

  entry[["value"]]
}

processCachePut <- function(key, value, dir) {
  if (is.null(key) || is.na(key)) {
    return(invisible(FALSE))
  }

  path <- processCachePath(dir)
  if (is.null(path)) {
    return(invisible(FALSE))
  }

  # write next to the target and rename, so a crash cannot leave a broken entry
  tmp <- paste0(path, ".tmp")
  ok <- tryCatch(
    {
      saveRDS(list(key = key, value = value, created = Sys.time()), tmp)
      file.rename(tmp, path)
    },
    error = function(e) {
      message("could not write the result cache: ", conditionMessage(e))
      FALSE
    }
  )
  if (!isTRUE(ok)) {
    unlink(tmp)
  }

  invisible(isTRUE(ok))
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

# short description for the settings tab
processCacheSummary <- function() {
  files <- list.files(processCacheDir(), pattern = "\\.rds$", full.names = TRUE)
  size <- if (length(files) == 0L) 0 else sum(file.size(files), na.rm = TRUE)
  sprintf("%d cached dataset(s), %.1f MB", length(files), size / 1024^2)
}
