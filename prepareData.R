library(MALDIcellassay)
library(MALDIquant)

dir <- "C:/0100/"

spec <- loadSpectra(dir)

conc <- as.numeric(names(spec))
spec <- spec[order(conc)]

cat("check for empty spectra...\n")
conc <- names(spec)
peaks <- detectPeaks(spec, SNR = 3, method = "SuperSmoother")
filPeaks <- vapply(peaks,
                   function(x) {
                     ifelse(length(mz(x)) > 0, TRUE, FALSE)
                   },
                   FUN.VALUE = TRUE)
cat(sum(filPeaks), "/", length(spec), "spectra retained.\n")
peaks_fil <- peaks[filPeaks]

spec <- spec[filPeaks]
names(spec) <- conc[filPeaks]


res <- fitCurve(spec = spec,
                dir = dir,
                conc = NA,
                normMeth = "mz",
                normMz = 523.021,
                normTol = 0.1,
                SNR = 3,
                saveIntensityMatrix = FALSE,
                plot = FALSE,
                SinglePointRecal = TRUE,
                varFilterMethod = "none")
