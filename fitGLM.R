fitGLM <- function(res, sigmoid = FALSE) {

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

getVi <- function(model, penalty) {
  vi <- model$model %>%
    finalize_model(tibble(penalty = 10^penalty)) %>%
    fit(data = model$prepData, formula = conc ~.) %>%
    vip::vi_model(lambda = 10^penalty)

  return(vi)
}
