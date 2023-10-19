perpareVi <- function(vi) {
  res <-
  vi %>%
    mutate(Variable = readr::parse_number(Variable)) %>%
    mutate(`Lasso importance` = ifelse(Sign == "POS",
                                       Importance,
                                       -Importance)) %>%
    mutate(mz = as.numeric(Variable),
           `Lasso importance` = round(`Lasso importance`, digits = 4)) %>%
    select(mz, `Lasso importance`) %>%
    mutate(mzIdx = match.closest(x = mz,
                                 table = getAllMz(appData$res),
                                 tolerance = 0.1)) %>%
    filter(!`Lasso importance` == 0) %>%
    select(-mz)

  return(res)
}
