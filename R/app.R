#' M2ara application object
#'
#' Builds the Shiny application from the settings in the working directory
#' (`settings.csv`, optional). Use [runApp()] to start it.
#'
#' @param defaults settings as returned by `defaultsSettingsHandler()`. When
#'   `NULL`, they are read from `settings.csv` in the working directory.
#'
#' @return A `shiny.appobj`.
#' @export
app <- function(defaults = NULL) {
  if (is.null(defaults)) {
    defaults <- defaultsSettingsHandler()
  }

  # figures referenced by the manual are served from the package
  figures <- system.file("figures", package = "M2ara")
  if (nzchar(figures)) {
    shiny::addResourcePath("figures", figures)
  }

  # javascript of the app (selection, shortcuts, share links)
  www <- system.file("www", package = "M2ara")
  if (nzchar(www)) {
    shiny::addResourcePath("m2ara", www)
  }

  shiny::shinyApp(
    # the settings of a share link are read per request
    ui = function(request) {
      appUI(appDefaultsFromUrl(defaults, request$QUERY_STRING)$defaults)
    },
    server = function(input, output, session) {
      appServer(input, output, session, defaults = defaults)
    }
  )
}

#' Start M2ara
#'
#' Starts the M2ara Shiny app in the working directory. The settings are read
#' from `settings.csv` in that directory and written back by "Save settings", so
#' the working directory has to be writable.
#'
#' @param ... passed on to [shiny::runApp()], e.g. `launch.browser`, `port`.
#'
#' @return `NULL`; the function blocks while the app runs.
#' @export
runApp <- function(...) {
  migrate_old_settings()
  shiny::runApp(app(), ...)
}

# Settings used to live in the app directory (`<install>/app/settings.csv`) and
# now live in the working directory, i.e. the installation root. Copy the old
# file once so an upgrade does not lose the user's settings.
migrate_old_settings <- function() {
  new <- file.path(getwd(), "settings.csv")
  old <- file.path(getwd(), "app", "settings.csv")
  if (!file.exists(new) && file.exists(old)) {
    file.copy(old, new)
    message("M2ara: settings migrated from app/settings.csv to settings.csv")
  }
  invisible(NULL)
}
