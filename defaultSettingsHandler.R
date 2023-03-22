generateDefaults <- function() {
  defaults <- data.frame(concUnits = "M",
                         smooth = TRUE,
                         rmBl = TRUE,
                         sqrtTrans = FALSE,
                         monoisotopicFilter = FALSE,
                         SNR = 3,
                         normMeth = "mz",
                         VarFilterMethod = "mean",
                         SinglePointRecal = TRUE,
                         normTol = 0.1,
                         normMz = 760.585,
                         alignTol = 0.01,
                         binTol = 200,
                         errorbars = FALSE,
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
                         hcMethod = "average")
  return(defaults)
}

defaultsSettingsHandler <- function(userSavedSettings = "settings.conf") {
  stopifnot(is.character(userSavedSettings))

  #### check for defaults ####
  if(file.exists(userSavedSettings)) {
    # if user saved settings exist use them
    defaults <- read.csv(userSavedSettings)
    cat(userSavedSettings, "loaded.\n")
  } else {
    defaults <- generateDefaults()
    cat("defaults loaded")
  }
  return(defaults)
}


