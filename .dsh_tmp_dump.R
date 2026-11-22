suppressPackageStartupMessages(library(MALDIcellassay))
ns <- asNamespace("MALDIcellassay")
cat("== exported ==\n")
print(sort(getNamespaceExports("MALDIcellassay")))
cat("\n== all ==\n")
print(sort(ls(ns, all.names = TRUE)))
for (n in c("calculateZPrime", "calculateVPrime", "calculateSSMD", "calculateCurveFit")) {
  cat("\n=====", n, "=====\n")
  print(get(n, envir = ns))
}
