checkInstalledPackages <- function() {

  #### package check on start up ####
  # If a package is installed, it will be loaded. If any
  # are not, the missing package(s) will be installed
  # from CRAN and then loaded.

  ## First specify the packages of interest
  packages = c("tidyverse", "tidymodels", "shiny",  "vip", "shinyFiles",
               "MALDIquant", "MALDIquantForeign", "DT", "plotly",
               "shinycssloaders", "shinyhelper", "knitr", "shinybusy",
               "shinythemes", "shinyWidgets", "devtools", "ggpubr", "dendextend",
               "glmnet", "proxy", "sparsepca", "platetools", "ggdendro", "zoo",
               "fs", "cluster", "shinyjs")

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
