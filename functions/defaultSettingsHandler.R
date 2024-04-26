generateDefaults <- function() {
  defaults <- tibble(concUnits = "M",
                     smooth = TRUE,
                     rmBl = TRUE,
                     sqrtTrans = FALSE,
                     monoisotopicFilter = FALSE,
                     avgMethod = "mean",
                     SNR = 3,
                     normMeth = "mz",
                     VarFilterMethod = "none",
                     SinglePointRecal = TRUE,
                     normTol = 0.1,
                     normMz = 349.1,
                     alignTol = 0,
                     binTol = 100,
                     errorbars = "none",
                     zoom = 4,
                     plateStat = "Recal-shift",
                     plateScale = FALSE,
                     pcaAlpha = -3,
                     pcaBeta = -3,
                     pcaX = "PC1",
                     pcaY = "PC2",
                     pcaEllipse = 0.67,
                     simpleLoadings = FALSE,
                     sigmoidModel = FALSE,
                     elasticNet = FALSE,
                     corFilter = FALSE,
                     penalty = -5,
                     num_cluster = 4,
                     hcDist = "Euclidean",
                     hcMethod = "average",
                     fileFomat = "bruker",
                     halfWindowSize = 3,
                     checkEmpty = TRUE)
  return(defaults)
}

defaultsSettingsHandler <- function(userSavedSettings = "settings.csv") {
  stopifnot(is.character(userSavedSettings))
  source("functions/handlePreprocSettings.R")

  #### check for defaults ####
  if(file.exists(userSavedSettings)) {
    # if user saved settings exist use them
    defaults <- read.csv(userSavedSettings) %>%
      tibble()
    cat(userSavedSettings, "loaded.\n")
  } else {
    defaults <- generateDefaults()
    cat("defaults loaded")
  }
  defaults$preproc_settings <-  handleDefaultPreprocSetting(
    smooth_lgl = defaults$smooth,
    rmBl_lgl = defaults$rmBl,
    sqrtTrans_lgl = defaults$sqrtTrans,
    monoisotopicFilter_lgl = defaults$monoisotopicFilter)

  return(defaults)
}


