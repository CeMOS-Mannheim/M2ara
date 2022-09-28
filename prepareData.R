library(MALDIcellassay)

dir <- "Z:/17-Thomas Enzlein/@Thomas/Von Alex/2022FEB14 16/"

spec <- loadSpectra(dir)

res <- fitCurve(spec = spec,
                dir = dir,
                conc = names(spec),
                normMeth = "mz",
                SNR = 3,
                saveIntensityMatrix = FALSE,
                plot = FALSE,
                SinglePointRecal = TRUE,
                varFilterMethod = "none")
