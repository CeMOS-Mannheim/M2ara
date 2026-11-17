# Entry point for `shiny::runApp(".")` and RStudio's "Run App" button.
#
# The app itself lives in the M2ara package. Install it once with
#   pak::pkg_install(".")          (or: source("install_packages.R"))
# and start it with `shiny::runApp(".")` or `M2ara::runApp()`.
if (!requireNamespace("M2ara", quietly = TRUE)) {
  stop(
    "M2ara is not installed. Install it with pak::pkg_install(\".\") ",
    "or run install_packages.R first.",
    call. = FALSE
  )
}

M2ara::runApp()
