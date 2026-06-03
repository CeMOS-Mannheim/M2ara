#' Find the project root by locating req.txt
#' Walks up from the current directory until req.txt is found
find_req_txt <- function(start_dir = getwd()) {
  d <- normalizePath(start_dir, winslash = "/", mustWork = FALSE)
  while (d != dirname(d)) {
    if (file.exists(file.path(d, "req.txt"))) {
      return(file.path(d, "req.txt"))
    }
    d <- dirname(d)
  }
  stop("Could not find req.txt from ", start_dir)
}
