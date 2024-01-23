fitGLM <- function(res, sigmoid = FALSE, elasticNet = FALSE, corFilter = TRUE) {

  intmat <- getIntensityMatrix(res, avg = FALSE, excludeNormMz = TRUE)
  cat("Number of features before preprocessing:", dim(intmat)[2], "\n")

  df <-intmat %>%
    as_tibble(intmat) %>%
    mutate(conc = getConc(res))

  rec <- recipe(df, conc ~.) %>%
    step_mutate_at(all_outcomes(), fn = transformConc2Log) %>% # x-axis has to be log10(conc)!
    step_nzv(all_predictors()) %>%
    step_normalize(all_predictors())

  if(corFilter) {
    rec <- rec %>%
      step_corr(all_predictors(), threshold = 0.975)
  }



  df_rdy <- prep(rec) %>%
    bake(new_data = NULL)

  cat("Number of features after preprocessing:", dim(df_rdy)[2]-1, "\n")

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

  if(elasticNet) {
    mixture <- pull(best, mixture)
  }

  cat("mixture =", mixture, "\n")

  return(list(model = glm,
              tune_rs = tune_rs,
              prepData = df_rdy,
              penalty = penalty,
              mixture = mixture))
}

getModelFit <- function(model, penalty) {
  fit <-
  model$model %>%
    finalize_model(tibble(penalty = 10^penalty)) %>%
    parsnip::fit(data = model$prepData, formula = conc ~.)
  return(fit)
}

getInformationCirteria <- function(fit) {
    #tLL <- fit$null.deviance - deviance(fit)
    tLL <- -deviance(fit) # 2*log-likelihood
    k <- fit$df
    n <- nobs(fit)
    AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
    AIC_ <- -tLL+2*k
    r2 <- fit$dev.ratio

    BIC<-log(n)*k - tLL
    res <- data.frame(penalty = fit$lambda,
                      nonZeroVar = fit$df,
                      r2 = r2,
                      AIC = AIC_,
                      AICc = AICc,
                      BIC = BIC)

    return(res)
}

getVi <- function(model, penalty, mixture, elesticNet = FALSE) {
  if(elesticNet) {
    vi <- model$model %>%
      finalize_model(tibble(penalty = 10^penalty,
                            mixture = mixture)) %>%
      parsnip::fit(data = model$prepData, formula = conc ~.) %>%
      vip::vi_model(lambda = 10^penalty)
  } else  {
    vi <- model$model %>%
      finalize_model(tibble(penalty = 10^penalty)) %>%
      parsnip::fit(data = model$prepData, formula = conc ~.) %>%
      vip::vi_model(lambda = 10^penalty)
  }

  return(vi)
}
