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

# The headings carry pandoc style anchors ("## Sample sheet {#sample-sheet}").
# The app shows the markdown with markdown::markdownToHTML(), which does not
# understand them - the anchors would appear as text and the links in the table of
# contents would go nowhere. Turn them into real ids.
md <- readLines("manual.md", warn = FALSE)
md <- sub("^(#+ .*) \\{#([^}]+)\\}$", "\\1 {#\\2}", md)
# markdown::markdownToHTML keeps raw HTML, so the id is written as an anchor tag
md <- sub("^(#+) (.*) \\{#([^}]+)\\}$", "\\1 \\2 <a id=\"\\3\"></a>", md)
writeLines(md, "manual.md", useBytes = TRUE)

cat("rendered inst/manual.md (",
    file.size("manual.md"), " bytes)\n", sep = "")
