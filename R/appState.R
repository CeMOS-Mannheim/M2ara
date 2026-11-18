# State of the app that is encoded into a shareable link -----------------------

# everything that is needed to describe the current view: the sidebar settings,
# the selected folder and the compared features
appStateFromInput <- function(input, dir = NULL, features = integer(0)) {
  list(
    settings = as.list(settingsDataFrame(input)),
    dir = if (is.null(dir)) "" else as.character(dir),
    features = as.integer(features)
  )
}

# compact, URL safe representation (base64 of JSON)
encodeAppState <- function(state) {
  json <- jsonlite::toJSON(state, auto_unbox = TRUE, digits = NA, null = "null")
  encoded <- jsonlite::base64_enc(charToRaw(as.character(json)))
  gsub("[[:space:]]", "", encoded)
}

decodeAppState <- function(txt) {
  raw <- jsonlite::base64_dec(txt)
  jsonlite::fromJSON(rawToChar(raw), simplifyVector = FALSE)
}

# merge the state of an URL into the defaults the app starts with
appDefaultsFromUrl <- function(defaults, query_string) {
  result <- list(defaults = defaults, features = integer(0), active = FALSE)

  if (is.null(query_string) || !nzchar(query_string)) {
    return(result)
  }

  query <- try(shiny::parseQueryString(query_string), silent = TRUE)
  if (inherits(query, "try-error") || is.null(query[["state"]])) {
    return(result)
  }

  state <- try(decodeAppState(query[["state"]]), silent = TRUE)
  if (inherits(state, "try-error") || is.null(state[["settings"]])) {
    return(result)
  }

  for (nm in names(state[["settings"]])) {
    value <- state[["settings"]][[nm]]
    if (is.list(value) && length(value) == 1L) {
      value <- value[[1]]
    }
    if (length(value) <= 1L) {
      defaults[[nm]] <- value
    }
  }

  if (!is.null(state[["dir"]]) && nzchar(state[["dir"]])) {
    defaults[["dir"]] <- state[["dir"]]
  }

  if (!is.null(state[["features"]])) {
    result$features <- as.integer(unlist(state[["features"]]))
  }

  result$defaults <- defaults
  result$active <- TRUE
  result
}

# the URL of the current view
appShareUrl <- function(session, state) {
  client <- session$clientData
  port <- client$url_port
  port <- if (is.null(port) || !nzchar(port)) "" else paste0(":", port)

  paste0(
    client$url_protocol, "//", client$url_hostname, port, client$url_pathname,
    "?state=", utils::URLencode(encodeAppState(state), reserved = TRUE)
  )
}
