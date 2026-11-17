# M2ara desktop launcher ------------------------------------------------------
#
# Started by M2ara.bat from the installation directory. Runs the Shiny app in
# ./app with the bundled R runtime and opens it in an application window.
#
# Only base R is used here, so the launcher also works when something is wrong
# with the bundled packages (it then prints what is missing).

root_dir <- local({
  arguments <- commandArgs(trailingOnly = FALSE)
  file_argument <- grep("^--file=", arguments, value = TRUE)
  if (length(file_argument) > 0L) {
    dirname(normalizePath(sub("^--file=", "", file_argument[1L]), winslash = "/"))
  } else {
    normalizePath(getwd(), winslash = "/")
  }
})

app_dir <- file.path(root_dir, "app")
lib_dir <- file.path(root_dir, "R", "library")
browser_profile_dir <- file.path(root_dir, "browser-profile")

if (!dir.exists(lib_dir)) {
  stop("the bundled R library is missing: ", lib_dir, call. = FALSE)
}

# Make sure the bundled packages win over anything the user may have installed.
.libPaths(c(lib_dir, .libPaths()))

read_app_version <- function() {
  version_file <- file.path(root_dir, "VERSION.txt")
  if (!file.exists(version_file)) {
    return("unknown")
  }
  version <- readLines(version_file, n = 1L, warn = FALSE)
  if (length(version) == 0L || !nzchar(trimws(version))) {
    return("unknown")
  }
  trimws(version)
}

check_packages <- function() {
  installed <- installed.packages(lib.loc = lib_dir)
  if (!"M2ara" %in% rownames(installed)) {
    stop(
      "this installation of M2ara is incomplete: the M2ara package is missing.\n",
      "Please reinstall M2ara from https://github.com/CeMOS-Mannheim/M2ara/releases",
      call. = FALSE
    )
  }

  required <- tools::package_dependencies(
    "M2ara",
    db = installed,
    which = c("Depends", "Imports", "LinkingTo"),
    recursive = TRUE
  )[[1L]]
  missing <- setdiff(required, rownames(installed))

  if (length(missing) > 0L) {
    stop(
      "this installation of M2ara is incomplete.\n",
      "Missing R package(s): ", paste(missing, collapse = ", "), "\n",
      "Please reinstall M2ara from https://github.com/CeMOS-Mannheim/M2ara/releases",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

find_bundled_browser <- function() {
  candidates <- c(
    file.path(root_dir, "chrome", "chrome.exe"),
    file.path(root_dir, "chrome", "GoogleChromePortable.exe")
  )
  candidates <- candidates[file.exists(candidates)]
  if (length(candidates) == 0L) NULL else candidates[[1L]]
}

find_edge <- function() {
  candidates <- c(
    file.path(Sys.getenv("ProgramFiles(x86)"), "Microsoft", "Edge", "Application", "msedge.exe"),
    file.path(Sys.getenv("ProgramFiles"), "Microsoft", "Edge", "Application", "msedge.exe"),
    file.path(Sys.getenv("LOCALAPPDATA"), "Microsoft", "Edge", "Application", "msedge.exe")
  )
  candidates <- candidates[nzchar(candidates) & !is.na(candidates)]
  candidates <- candidates[file.exists(candidates)]
  if (length(candidates) == 0L) NULL else candidates[[1L]]
}

# Starts the browser asynchronously: open-browser.ps1 waits for a visible
# window, brings it to the front and falls back to Edge and the default browser
# if no window appears. run.R must not wait for that - the Shiny server only
# answers requests once this function has returned.
launch_browser_window <- function(url, primary, secondary = NULL, user_data_dir = NULL) {
  script <- file.path(root_dir, "open-browser.ps1")
  powershell <- file.path(
    Sys.getenv("SystemRoot"), "System32", "WindowsPowerShell", "v1.0", "powershell.exe"
  )

  if (file.exists(script) && file.exists(powershell)) {
    parts <- c(
      shQuote(powershell),
      "-NoProfile -ExecutionPolicy Bypass -File", shQuote(script),
      "-Browser", shQuote(primary),
      "-Url", shQuote(url),
      "-OpenDefault"
    )
    if (!is.null(user_data_dir)) {
      parts <- c(parts, "-UserDataDir", shQuote(user_data_dir))
    }
    if (!is.null(secondary)) {
      parts <- c(parts, "-FallbackBrowser", shQuote(secondary))
    }
    started <- try(
      system(paste(parts, collapse = " "), wait = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE),
      silent = TRUE
    )
    if (!inherits(started, "try-error")) {
      return(invisible(TRUE))
    }
  }

  # Last resort: start the browser directly.
  arguments <- c(
    paste0("--app=", url),
    "--window-size=1440,900",
    "--disable-infobars"
  )
  system2(primary, args = shQuote(arguments), wait = FALSE)
  invisible(TRUE)
}

open_app_window <- function(url) {
  cat("\n  M2ara is running at ", url, "\n", sep = "")
  cat("  A browser window should open now. If you do not see it, look for\n")
  cat("  'M2ara' in the taskbar or open the address above in your browser.\n\n")

  browser <- find_bundled_browser()
  edge <- find_edge()

  if (!is.null(browser)) {
    message("Opening M2ara in the bundled browser ...")
    launch_browser_window(
      url,
      primary = browser,
      secondary = edge,
      user_data_dir = browser_profile_dir
    )
  } else if (!is.null(edge)) {
    message("Opening M2ara in the Microsoft Edge app window ...")
    launch_browser_window(url, primary = edge)
  } else {
    message("Opening M2ara in your default browser ...")
    utils::browseURL(url)
  }

  invisible(NULL)
}

cat("\n")
cat("  M2ara", read_app_version(), "\n")
cat("  https://github.com/CeMOS-Mannheim/M2ara\n\n")
cat("  Starting the application, this takes 10-20 seconds ...\n")

check_packages()

cat("  Ready. Close the application window to stop M2ara.\n\n")

# settings.csv lives in the installation directory
setwd(root_dir)

# Attach shiny before runApp loads the app: Shiny itself requires it while
# starting up, which would print "Loading required package: shiny".
suppressWarnings(suppressPackageStartupMessages(library(shiny)))

M2ara::runApp(launch.browser = open_app_window)
