# Session bookkeeping ----------------------------------------------------------
#
# The app shuts down when the browser goes away, but a page reload ends the old
# session and starts a new one within milliseconds. Shutting down immediately
# would kill the app on every reload (the shared link feature reloads the page),
# so the shutdown waits until no session is left.

.m2araSessions <- new.env(parent = emptyenv())
.m2araSessions$count <- 0L

registerSession <- function() {
  .m2araSessions$count <- .m2araSessions$count + 1L
  invisible(NULL)
}

unregisterSession <- function(onLast = NULL, delay = 2) {
  .m2araSessions$count <- max(0L, .m2araSessions$count - 1L)

  if (.m2araSessions$count == 0L && !is.null(onLast)) {
    later::later(function() {
      if (.m2araSessions$count == 0L) {
        onLast()
      }
    }, delay = delay)
  }

  invisible(NULL)
}
