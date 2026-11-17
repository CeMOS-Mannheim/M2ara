#!/usr/bin/env Rscript
#
# Renders inst/manual.Rmd to inst/manual.md. The app ships the rendered
# markdown (it is shown in the Manual tab), so no pandoc/knitr is needed at
# runtime. Re-run this after editing the manual.
#
# Run from the repository root: Rscript tools/render-manual.R

# the manual uses shiny::icon() in inline R code
suppressPackageStartupMessages(library(shiny))

manual_dir <- file.path("inst")
rmd <- file.path(manual_dir, "manual.Rmd")

if (!file.exists(rmd)) {
  stop("inst/manual.Rmd not found (run from the repository root)", call. = FALSE)
}

old_wd <- setwd(manual_dir)
on.exit(setwd(old_wd), add = TRUE)

knitr::knit(basename(rmd), output = "manual.md", quiet = TRUE)

cat("rendered inst/manual.md (",
    file.size("manual.md"), " bytes)\n", sep = "")
