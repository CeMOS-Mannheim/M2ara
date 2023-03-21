writeDefaults <- function(defaultsFile) {
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

  write.csv(defaults, file = defaultsFile, row.names = FALSE)
  cat(defaultsFile, "written.\n")
}

defaultsSettingsHandler <- function(userSavedSettings = "settings.conf", defaultsFile = "defaults.conf") {
  stopifnot(is.character(userSavedSettings))
  stopifnot(is.character(defaultsFile))

  #### check for defaults ####
  if(file.exists(userSavedSettings)) {
    # if user saved settings exit, use them
    defaults <- read.csv(userSavedSettings)
    cat(userSavedSettings, "loaded.\n")
  } else {
    if(!file.exists(defaultsFile)) {
      # check if there is a defaults file
      # create it if not
      writeDefaults(defaultsFile)
    }

    if(!file.exists(defaultsFile)) {
      # this check should be redundant but we need to make sure we have defaults
      # to load
      stop(paste(defaultsFile, "not found and unable to create it.\nPlease check your permissions.\n"))
    }
    defaults <- read.csv(defaultsFile)
  }
  return(defaults)
}


