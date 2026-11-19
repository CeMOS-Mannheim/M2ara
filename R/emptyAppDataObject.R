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
                 info_state = "initial",
                 show_plot = FALSE,
                 mapping = NULL,
                 centroided = NULL,
                 settings_snapshot = NULL, # settings of the last run (export)
                 pending_selection = integer(0), # features from a share link
                 force_reprocess = FALSE, # next run ignores the cache
                 samples = NULL, # effective concentration sheet
                 load_problem = NULL, # reason when loading failed
                 skipped = character(0), # entries that are no dataset
                 mapping_source = NULL) # 'file names', 'uploaded' or the sheet file
}
