library(tidymodels)
library(tidyverse)


intmat <- intensityMatrix(getSinglePeaks(appData$res))

df <- as_tibble(intmat) %>%
  mutate(conc = getConc(res))

bs <- bootstraps(df,
                 times = 4,
                 strata = "conc",
                 pool = 0.25)

rec <- recipe(df, conc ~.) %>%
  step_log(all_outcomes(), base = 10) %>%
  step_sqrt() %>%
  step_normalize(all_predictors()) %>%
  step_YeoJohnson(all_predictors()) %>%
  step_corr(all_predictors())


df_rdy <- prep(rec) %>%
  bake(new_data = NULL)

grid <- grid_regular(penalty(range = c(-6,0)),  levels = 5)

glm <- linear_reg(penalty = tune()) %>%
  set_engine("glmnet")

tune_rs <- tune_grid(glm, preprocessor = rec, resamples = bs, grid = grid)
collect_metrics(tune_rs)
best_penalty <- select_by_one_std_err(tune_rs, desc(penalty), metric = "rsq")

model <- finalize_model(glm, best_penalty) %>%
  fit(data =df_rdy, formula = conc ~.)

model %>%
  predict(df_rdy) %>%
  mutate(truth = df_rdy %>%
           pull(conc)) %>%
  ggplot(aes(x = .pred, y = truth)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(aes(label = ..adj.rr.label..)) +
  coord_obs_pred() +
  labs(x = "Predicted concentration",
       y = "True concentration") +
  theme_minimal(base_size = 14)

model %>%
  vip::vi_model(lambda = best_penalty %>% pull(penalty)) %>%
  arrange(desc(Importance)) %>%
  slice_head(n = 20) %>%
  mutate(Variable = round(readr::parse_number(Variable), 3)) %>%
  mutate(imp = ifelse(Sign == "POS", Importance, -Importance)) %>%
  mutate(Variable = fct_reorder(as.factor(Variable), imp)) %>%
  ggplot(aes(x = Variable, y = imp, fill = Sign)) +
  geom_col() +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(x = "m/z",
       y = "Variable importance")


model %>%
  vip::vi_model(lambda = best_penalty %>% pull(penalty)) %>%
  arrange(desc(Importance)) %>%
  mutate(Variable = round(readr::parse_number(Variable), 3)) %>%
  mutate(imp = ifelse(Sign == "POS", Importance, -Importance)) %>%
  ggplot(aes(x = Variable, ymax = imp, ymin = 0, col = Sign)) +
  geom_hline(yintercept = 0, alpha = 0.25) +
  geom_linerange() +
  theme_minimal(base_size = 14) +
  labs(x = "m/z",
       y = "Variable importance")
