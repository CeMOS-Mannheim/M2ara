checkInstalledPackages <- function() {
  #### package check on start up ####
  # If a package is installed, it will be loaded. If any
  # are not, the missing package(s) will be installed
  # from CRAN and then loaded.

  ## First specify the packages of interest
  packages = c("tidyverse", "shiny", "shinyFiles",
               "MALDIquant", "MALDIquantForeign", "DT", "plotly",
               "shinycssloaders", "shinyhelper", "knitr", "shinybusy",
               "shinythemes", "shinyWidgets", "devtools",
               "proxy", "sparsepca", "platetools", "zoo",
               "fs", "cluster", "shinyjs", "latrend", "dtwclust")

  ## Now load or install&load all
  package.check <- lapply(
    packages,
    FUN = function(x) {
      if (
        suppressWarnings(
          suppressPackageStartupMessages(
            !require(x,
                     character.only = TRUE,
                     quietly = TRUE,
                     warn.conflicts = FALSE)
          )
        )
      ) {
        install.packages(x, dependencies = TRUE)
        library(x, character.only = TRUE)
      }
    }
  )

  # options
  options(dplyr.summarise.inform = FALSE)

  # special case for packages not on CRAN
  if (!require("MALDIcellassay", character.only = TRUE)) {
    devtools::install_github("CeMOS-Mannheim/MALDIcellassay", dependencies = TRUE)
    library(MALDIcellassay, character.only = TRUE)
  }
}
