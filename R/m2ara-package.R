#' M2ara
#'
#' A Shiny GUI to explore concentration-response data from MALDI cell assays.
#'
#' @keywords internal
#' @import shiny
#' @import shinyFiles
#' @import shinycssloaders
#' @import shinyhelper
#' @import shinybusy
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @import forcats
#' @import purrr
#' @import readr
#' @import MALDIquant
#' @import MALDIcellassay
#' @import sparsepca
#' @import fs
#' @import patchwork
#' @importFrom plotly ggplotly plotlyOutput renderPlotly
#' @importFrom shinyjs useShinyjs disable enable hidden show hide
#' @importFrom methods new is
#' @importFrom stats var median sd na.omit setNames
#' @importFrom utils head tail read.csv write.csv
#' @importFrom grDevices dev.off
"_PACKAGE"

# variables used inside dplyr/ggplot2 non-standard evaluation
utils::globalVariables(c(
  "mzIdx", "conc", "y", "x", "pIC50", "R2", "fc", "pEC50", "log2FC", "CRS",
  "val", "xax", "yax", "spot", "cheuv", "value", "direction", "Z", "V", "SSMD",
  "spectrum", "V'", "Z'"
))
# Side effects have to be set when the namespace is loaded: top level code in R/
# runs at install time only.
.onLoad <- function(libname, pkgname) {
  ggplot2::theme_set(m2araTheme())
  invisible(NULL)
}