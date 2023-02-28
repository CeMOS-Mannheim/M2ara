library(MALDIquant)
library(MALDIquantForeign)
library(tidyverse)
library(MALDIcellassay)
library(ggdendro)
library(proxy)
library(patchwork)
library(zoo)

optimalNumClusters <- function(dend) {
  opt <- dendextend::find_k(dend, krange = 1:25)

  return(opt)
}

optimalNumClustersPlot <- function(opt, sel_k) {
  df <- tibble(k = 1:25,
               crit = opt$crit)

  p <-
    ggplot(df, aes(x = k, y = crit)) +
    geom_point() +
    geom_vline(aes(xintercept = opt$nc),
               col = "red",
               linetype = "dashed",
               alpha = 0.75) +
    geom_vline(aes(xintercept = sel_k),
               col = "green") +
    geom_line() +
    labs(x = "Number of clusters",
         y = "Average silhouette width") +
    theme_minimal(base_size = 16)
  return(p)
}


doHClust <- function(res, cut = 5, dist = "correlation", clustMethod = "ward.D2") {
  avg <- getAvgSpectra(res)

  intmat <- intensityMatrix(getAvgPeaks(res), spectra = avg)

  # select only those m/z values that were fitted to curves
  all_mz <- round(as.numeric(colnames(intmat)), digits = 3)

  intmat <- intmat[,which(all_mz %in% getAllMz(res))]
  colnames(intmat) <- round(as.numeric(colnames(intmat)), digits = 3)

  tintmat <- t(scale(intmat))
  colnames(tintmat) <- names(avg)

  d <- proxy::dist(tintmat, method = dist)
  attr(d, "Labels") <- round(as.numeric(colnames(intmat)), digits = 2)
  hc <- hclust(d, method = clustMethod)


  dend <- as.dendrogram(hc)
  dend <- color_labels(dend, k = cut)
  dend <- color_branches(dend, k =  cut)

  opt <- optimalNumClusters(dend)

  return(list(tintmat = tintmat,
              dend = dend,
              opt = opt))
}

extractClusters <- function(dend) {
  clusters <- tibble(mz = as.numeric(labels(dend)),
                     cluster = as.numeric(factor(get_leaves_branches_col(dend)))) %>%
    mutate(cluster = factor(cluster)) # turn back to factors to better sort table

  return(clusters)
}

plotDendro <- function(dend) {
  cluster <- as.numeric(factor(get_leaves_branches_col(dend)))
  n_cluster <- length(unique(cluster))

  p <-
    as.ggdend(dend) %>%
    ggplot(horiz = TRUE) +
    scale_colour_manual(values = c("grey", rainbow(n_cluster))) +
    theme(legend.position = "none")
  return(p)
}


plotClusterCurves <- function(dend, tintmat) {
  cluster <- as.numeric(factor(get_leaves_branches_col(dend)))
  n_cluster <- length(unique(cluster))

  df <-
    tintmat %>%
    as_tibble(rownames = NA) %>%
    rownames_to_column("label") %>%
    mutate(cluster = cluster) %>%
    gather(conc, int, -cluster, -label) %>%
    mutate(conc = log10(as.numeric(conc))) %>%
    group_by(conc, cluster) %>%
    summarise(meanInt = mean(int),
              n_mz = n()) %>%
    ungroup()

  p_curves <-
    df %>%
    ggplot(aes(x = conc, y = meanInt, col = factor(cluster), label = n_mz)) +
    geom_point(alpha = 0.75) +
    stat_smooth(method = "loess", se = FALSE, alpha = 0.6) +
    scale_colour_manual(values = c("grey", rainbow(n_cluster))) +
    theme_minimal(base_size = 16) +
    #theme(legend.position = "none") +
    labs(x = "Log10(Conc.)",
         y = "Z-Scored mean intensity\nof clusters",
         col = "Cluster")

  return(p_curves)
}



