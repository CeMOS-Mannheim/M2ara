writeDefaults <- function() {
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
                         binTol = 200)

  write.csv(defaults, file = "defaults.conf", row.names = FALSE)
  cat("defaults written.\n")
}


