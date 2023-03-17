fitGLM <- function(res, sigmoid = FALSE, elasticNet = FALSE) {

  intmat <- getIntensityMatrix(res)

  df <-intmat %>%
    as_tibble(intmat) %>%
    mutate(conc = getConc(res))

  rec <- recipe(df, conc ~.) %>%
    step_mutate_at(all_outcomes(), fn = transformConc2Log) %>% # x-axis has to be log10(conc)!
    #step_nzv(all_predictors()) %>%
    step_normalize(all_predictors()) #%>%
  #step_corr(all_predictors(), threshold = 0.95)


  df_rdy <- prep(rec) %>%
    bake(new_data = NULL)

  if(elasticNet) {
    glm <- linear_reg(penalty = tune(), mixture = tune()) %>%
      set_engine("glmnet") %>%
      set_mode("regression")

    grid <- grid_max_entropy(penalty(range = c(-5,0)),
                             mixture(),
                             size = 20)
  } else {
    glm <- linear_reg(penalty = tune(), mixture = 1) %>%
      set_engine("glmnet") %>%
      set_mode("regression")

    grid <- grid_regular(penalty(range = c(-5,0)),
                         levels = 10)
    mixture <- 1
  }



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
                       grid = grid)

  best <- select_by_one_std_err(tune_rs, desc(penalty), metric = "rsq")
  penalty <- pull(best, penalty)
  mixture <- pull(best, mixture)

  cat("mixture =", mixture, "\n")

  return(list(model = glm,
              prepData = df_rdy,
              penalty = penalty,
              mixture = mixture))
}

getVi <- function(model, penalty, mixture, elesticNet = FALSE) {
  if(elesticNet) {
    vi <- model$model %>%
      finalize_model(tibble(penalty = 10^penalty,
                            mixture = mixture)) %>%
      fit(data = model$prepData, formula = conc ~.) %>%
      vip::vi_model(lambda = 10^penalty)
  } else  {
    vi <- model$model %>%
      finalize_model(tibble(penalty = 10^penalty)) %>%
      fit(data = model$prepData, formula = conc ~.) %>%
      vip::vi_model(lambda = 10^penalty)
  }

  return(vi)
}
