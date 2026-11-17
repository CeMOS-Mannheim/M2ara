#!/usr/bin/env Rscript
#
# Informational lint run (the app is not an R package yet, so the linters are
# not blocking). Prints a summary of the most frequent lints and the first
# findings, so they show up in the CI log.
#
# Run from the repository root: Rscript tools/lint.R

if (!requireNamespace("lintr", quietly = TRUE)) {
  if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
  pak::pkg_install("lintr")
}

targets <- c(list.files("functions", pattern = "\\.R$", full.names = TRUE),
             list.files("components", pattern = "\\.R$", full.names = TRUE),
             "app.R")
targets <- targets[file.exists(targets)]

lints <- unlist(lapply(targets, lintr::lint), recursive = FALSE)

cat("files linted:", length(targets), "\n")

if (length(lints) == 0L) {
  cat("no lints\n")
} else {
  linters <- vapply(lints, function(x) class(x)[1L], character(1))
  cat("total lints:", length(lints), "\n\n")
  print(sort(table(linters), decreasing = TRUE))
  cat("\nfirst findings:\n")
  print(utils::head(lints, 25))
}
