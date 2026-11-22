# States of the app -------------------------------------------------------------
#
# The app walks through a small number of states while data is selected, read and
# processed. Every change goes through setInfoState(), so an unexpected jump is
# reported in the log instead of silently producing an inconsistent UI.

infoStates <- c("initial", "dir_set", "loaded", "processed",
                "loadErrorNum", "loadErrorFormat", "loadErrorUniqueMz",
                "loadErrorMapping", "errorMapping",
                "fitErrorRecal", "fitErrorNorm", "fitErrorOther")

# allowed sources for every state (a state may always be set again)
infoStateRules <- list(
  initial = character(0),
  dir_set = c("initial", "dir_set", "loaded", "processed",
              "loadErrorNum", "loadErrorFormat", "loadErrorUniqueMz",
              "loadErrorMapping", "errorMapping",
              "fitErrorRecal", "fitErrorNorm", "fitErrorOther"),
  loaded = c("dir_set", "loaded"),
  processed = c("loaded", "processed"),
  loadErrorNum = c("dir_set", "loadErrorNum"),
  loadErrorFormat = c("dir_set", "loadErrorFormat"),
  loadErrorUniqueMz = c("dir_set", "loadErrorUniqueMz"),
  loadErrorMapping = c("dir_set", "loadErrorMapping"),
  errorMapping = c("initial", "dir_set", "loaded", "processed", "errorMapping"),
  fitErrorRecal = c("loaded", "fitErrorRecal"),
  fitErrorNorm = c("loaded", "fitErrorNorm"),
  fitErrorOther = c("loaded", "processed", "fitErrorOther")
)

# does the state exist?
isInfoState <- function(state) {
  length(state) == 1L && !is.na(state) && state %in% infoStates
}

# can `to` follow `from`?
isAllowedTransition <- function(from, to) {
  if (!isInfoState(to)) {
    return(FALSE)
  }
  if (identical(from, to)) {
    return(TRUE)
  }
  from %in% infoStateRules[[to]]
}

# set the state of the app (the single place that changes it)
setInfoState <- function(appData, state) {
  if (!isInfoState(state)) {
    stop("unknown state: ", paste(state, collapse = ", "), call. = FALSE)
  }

  # during start up the state is set outside of a reactive context, where a
  # reactiveValue cannot be read - the check is skipped then
  current <- tryCatch(appData$info_state, error = function(e) NULL)

  if (!is.null(current) && !isAllowedTransition(current, state)) {
    warning("unexpected state change: ", current, " -> ", state,
            call. = FALSE)
  }

  appData$info_state <- state
  invisible(state)
}
