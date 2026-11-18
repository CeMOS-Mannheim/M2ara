# Drive letters that can be browsed in the "Select folder" dialog.
#
# WMIC was used for this in the past, but it is deprecated and no longer part of
# Windows 11 24H2+ (and it printed a confusing "report this to shinyFiles"
# message when missing). Probing the drive letters with dir.exists() is not an
# option either: a disconnected network drive can block for many seconds, so the
# list is queried from Windows instead.
windowsDriveLetters <- function() {
  powershell <- file.path(
    Sys.getenv("SystemRoot"), "System32", "WindowsPowerShell", "v1.0", "powershell.exe"
  )

  if (file.exists(powershell)) {
    # GetLogicalDrives() only returns the drive letters (no volume access), so
    # this is instant even when mapped network drives are offline. WMIC and
    # Get-CimInstance were both too slow/unreliable here.
    query <- "[System.IO.Directory]::GetLogicalDrives()"
    drives <- try(
      system2(
        powershell,
        args = c("-NoProfile", "-NonInteractive", "-Command", shQuote(query)),
        stdout = TRUE,
        stderr = FALSE,
        timeout = 10
      ),
      silent = TRUE
    )

    if (!inherits(drives, "try-error") && length(drives) > 0L) {
      drives <- toupper(trimws(drives))
      drives <- drives[grepl("^[A-Z]:", drives)]
      drives <- unique(paste0(substr(drives, 1L, 1L), ":"))
      if (length(drives) > 0L) {
        return(sort(drives))
      }
    }
  }

  system_drive <- Sys.getenv("SystemDrive")
  if (!nzchar(system_drive)) system_drive <- "C:"
  system_drive
}

getVolumes <- function(exclude = NULL) {
  osSystem <- Sys.info()["sysname"]
  if (osSystem == "Darwin") {
    volumes <- dir_ls("/Volumes")
    names(volumes) <- basename(volumes)
  }
  else if (osSystem == "Linux") {
    volumes <- c(Computer = "/")
    if (isTRUE(dir_exists("/media"))) {
      media <- dir_ls("/media")
      names(media) <- basename(media)
      volumes <- c(volumes, media)
    }
  }
  else if (osSystem == "Windows") {
    volumes <- windowsDriveLetters()
  }
  else {
    stop("unsupported OS")
  }
  if (!is.null(exclude)) {
    volumes <- volumes[!names(volumes) %in% exclude]
  }
  volumes <- tolower(volumes)
  names(volumes) <- gsub(x = volumes, pattern = ":", replacement = "")

  return(volumes)
}
