#!/usr/bin/env Rscript
#
# Informational lint run. The style linters report a long backlog of cosmetic
# findings in the app code, so the step prints a summary instead of failing CI
# (the syntax/object checks are part of R CMD check).
#
# Run from the repository root: Rscript tools/lint.R

if (!requireNamespace("lintr", quietly = TRUE)) {
  if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
  pak::pkg_install("lintr")
}

targets <- c(list.files("R", pattern = "\\.R$", full.names = TRUE),
             list.files("tests/testthat", pattern = "\\.R$", full.names = TRUE),
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
