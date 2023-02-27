fitGLM <- function(res, sigmoid = FALSE) {

  intmat <- intensityMatrix(getSinglePeaks(res))

  all_mz <- round(as.numeric(colnames(intmat)), digits = 3)

  # filter out normalization mz
  normMz <- getNormMz(res)

  if(!is.null(normMz)) {
    normMzIdx <- match.closest(normMz, all_mz)
    intmat <- intmat[,-normMzIdx]
    all_mz <- round(as.numeric(colnames(intmat)), digits = 3)
  }

  # select only those m/z values that were fitted to curves
  intmat <- intmat[,which(all_mz %in% getAllMz(res))]
  colnames(intmat) <- round(as.numeric(colnames(intmat)), digits = 3)

  df <-intmat %>%
    as_tibble(intmat) %>%
    mutate(conc = getConc(res))

  rec <- recipe(df, conc ~.) %>%
    step_log(all_outcomes(), base = 10) %>% # x-axis has to be log10(conc)!
    step_nzv(all_predictors()) %>%
    step_normalize(all_predictors()) %>%
    step_corr(all_predictors(), threshold = 0.9)


  df_rdy <- prep(rec) %>%
    bake(new_data = NULL)

  glm <- linear_reg(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet") %>%
    set_mode("regression")

  if(sigmoid) {
    # set linkfun and family so that a sigmoid fit is done
    cat("fitting sigmoid model...\n")
    glm <-
      glm %>%
      set_args(linkfun = function(x) 1 / (1 + exp(-x)))
  }

  tune_rs <- tune_grid(object = glm,
                       preprocessor = rec,
                       resamples = bootstraps(df,
                                              times = 5,
                                              strata = "conc",
                                              pool = 0.4),
                       grid = grid_regular(penalty(range = c(-5,0)),
                                           levels = 10))

  best_penalty <- select_by_one_std_err(tune_rs, desc(penalty), metric = "rsq")

  return(list(model = glm,
              prepData = df_rdy,
              penalty = pull(best_penalty, penalty)))
}
