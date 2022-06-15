######################
# FUNCTIONS related to cross validation of hierarchical GAM
######################
# ------------------------------------------------------------------------------------------------------------------
# Functions for cross validation comparison to get RMSE for each model (takes about 10 minutes, so comment out when necessary)
# Generate random partitions (default = 100) holding out fraction of observations (default = 20%) 
# for calculating RMSE
# ------------------------------------------------------------------------------------------------------------------
save_RData <- function(object,fileName){
  #loads an RData file, and returns it
  save(object, fileName)
  get(ls()[ls() != "fileName"])
}

cv_mod <- function(data, stoich, npartition = 100, fractest = 0.2, file) {
  if (stoich == "CP") {
    modG <- b1_4vars_nutlim_modG_CP(data)
    modGS <- b1_4vars_nutlim_modGS_CP(data)
    modGI <- b1_4vars_nutlim_modGI_CP(data)
    modS <- b1_4vars_nutlim_modS_CP(data)
    modI <- b1_4vars_nutlim_modI_CP(data)
    modC <- b1_4vars_nutlim_modC_CP(data)
  } else if (stoich == "NP") {
    modG <- b1_4vars_nutlim_modG_NP(data)
    modGS <- b1_4vars_nutlim_modGS_NP(data)
    modGI <- b1_4vars_nutlim_modGI_NP(data)
    modS <- b1_4vars_nutlim_modS_NP(data)
    modI <- b1_4vars_nutlim_modI_NP(data)
    modC <- b1_4vars_nutlim_modC_NP(data)
  } else if (stoich == "CN") {
    modG <- b1_4vars_nutlim_modG_CN(data)
    modGS <- b1_4vars_nutlim_modGS_CN(data)
    modGI <- b1_4vars_nutlim_modGI_CN(data)
    modS <- b1_4vars_nutlim_modS_CN(data)
    modI <- b1_4vars_nutlim_modI_CN(data)
    modC <- b1_4vars_nutlim_modC_CN(data)
  }
  set.seed(1)  # Run to replicate
  cv_df =
      crossv_mc(data, npartition, fractest) %>%
      mutate(
          train = map(train, as_tibble),
          test = map(test, as_tibble))
  cv_df =
    cv_df %>%
    mutate(
      CNP_Nutcline_modG  = map(train,~gam(formula(modG$formula),method = "REML", family = "gaussian", data=.x)),
      CNP_Nutcline_modGS  = map(train,~gam(formula(modGS$formula),method = "REML", family = "gaussian", data=.x)),
      CNP_Nutcline_modGI = map(train,~gam(formula(modGI$formula),method = "REML", family = "gaussian", data=.x)),
      CNP_Nutcline_modS  = map(train,~gam(formula(modS$formula),method = "REML", family = "gaussian", data=.x)),
      CNP_Nutcline_modI  = map(train,~gam(formula(modI$formula),method = "REML", family = "gaussian", data=.x)),
      CNP_Nutcline_modC  = map(train,~gam(formula(modC$formula),method = "REML", family = "gaussian", data=.x))
    ) %>%
    mutate(
      rmse_CNP_Nutcline_modG = map2_dbl(CNP_Nutcline_modG, test, ~rmse(model = .x, data = .y)),
      rmse_CNP_Nutcline_modGS = map2_dbl(CNP_Nutcline_modGS, test, ~rmse(model = .x, data = .y)),
      rmse_CNP_Nutcline_modGI = map2_dbl(CNP_Nutcline_modGI, test, ~rmse(model = .x, data = .y)),
      rmse_CNP_Nutcline_modS = map2_dbl(CNP_Nutcline_modS, test, ~rmse(model = .x, data = .y)),
      rmse_CNP_Nutcline_modI = map2_dbl(CNP_Nutcline_modI, test, ~rmse(model = .x, data = .y)),
      rmse_CNP_Nutcline_modC = map2_dbl(CNP_Nutcline_modC, test, ~rmse(model = .x, data = .y)),
      rsq_train_CNP_Nutcline_modG = map2_dbl(CNP_Nutcline_modG, train, ~rsquare(model = .x, data = .y)),
      rsq_train_CNP_Nutcline_modGS = map2_dbl(CNP_Nutcline_modGS, train, ~rsquare(model = .x, data = .y)),
      rsq_train_CNP_Nutcline_modGI = map2_dbl(CNP_Nutcline_modGI, train, ~rsquare(model = .x, data = .y)),
      rsq_train_CNP_Nutcline_modS = map2_dbl(CNP_Nutcline_modS, train, ~rsquare(model = .x, data = .y)),
      rsq_train_CNP_Nutcline_modI = map2_dbl(CNP_Nutcline_modI, train, ~rsquare(model = .x, data = .y)),
      rsq_train_CNP_Nutcline_modC = map2_dbl(CNP_Nutcline_modC, train, ~rsquare(model = .x, data = .y)),
      rsq_test_CNP_Nutcline_modG = map2_dbl(CNP_Nutcline_modG, test, ~rsquare(model = .x, data = .y)),
      rsq_test_CNP_Nutcline_modGS = map2_dbl(CNP_Nutcline_modGS, test, ~rsquare(model = .x, data = .y)),
      rsq_test_CNP_Nutcline_modGI = map2_dbl(CNP_Nutcline_modGI, test, ~rsquare(model = .x, data = .y)),
      rsq_test_CNP_Nutcline_modS = map2_dbl(CNP_Nutcline_modS, test, ~rsquare(model = .x, data = .y)),
      rsq_test_CNP_Nutcline_modI = map2_dbl(CNP_Nutcline_modI, test, ~rsquare(model = .x, data = .y)),
      rsq_test_CNP_Nutcline_modC = map2_dbl(CNP_Nutcline_modC, test, ~rsquare(model = .x, data = .y))
    )
  cv_df <- cv_df %>%
    dplyr :: select(starts_with("rmse_CNP"), 
                    starts_with("rsq_train_CNP"),
                    starts_with("rsq_test_CNP")) %>%
    setNames(c("x1","x2","x3","x4","x5","x6",
               "y1","y2","y3","y4","y5","y6",
               "z1","z2","z3","z4","z5","z6")) %>%
    pivot_longer(
      everything(),
      names_to = c(".value","model"),
      names_pattern = "(.)(.)"
    )
  cv_df <- setNames(cv_df, c("model","rmse","rsq_train","rsq_test"))
  cv_df$model <- factor(cv_df$model,levels = c("1","2","3","4","5","6"))
  levels(cv_df$model) <-c("modG","modGS","modGI","modS","modI","modC")
  saveRDS(cv_df, file)
  return(cv_df)
}










