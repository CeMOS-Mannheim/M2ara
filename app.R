# Entry point for `shiny::runApp()` and RStudio's "Run App" button.
#
# Shiny treats a directory that contains DESCRIPTION and R/ as a *package app*:
# it sources the files in R/ and then evaluates this file. A plain
# `library(M2ara)` would silently run the installed version instead of this
# checkout, so a checkout is loaded with pkgload::load_all().
is_checkout <- file.exists("DESCRIPTION") && dir.exists("R")

if (is_checkout && requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", quiet = TRUE)
} else if (!requireNamespace("M2ara", quietly = TRUE)) {
  stop(
    "M2ara is not installed. Install it with pak::pkg_install(\".\") ",
    "or run install_packages.R first.",
    call. = FALSE
  )
}

M2ara::app()
