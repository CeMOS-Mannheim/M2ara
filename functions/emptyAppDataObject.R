emptyAppDataObject <- function(){
  reactiveValues(selected_dir = NULL,
                 res = NULL, # MALDIcellassay object
                 spec_all = NULL, # all spectra
                 spec_idx = NULL, # indices of non-empty spectra
                 preprocessing = list(smooth = NULL,
                                      rmBl = NULL,
                                      sqrtTrans = NULL,
                                      monoisotopicFilter = NULL), # preprocessing
                 stats_original = NULL, # original stats
                 stats = NULL,
                 pca = NULL,
                 model = NULL,
                 clust = NULL,
                 opt = NULL,
                 info_state = "inital",
                 show_plot = FALSE)
}
