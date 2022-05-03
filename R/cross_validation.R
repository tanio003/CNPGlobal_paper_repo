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
      rmse_CNP_Nutcline_modC = map2_dbl(CNP_Nutcline_modC, test, ~rmse(model = .x, data = .y))
      )
  
  cv_df <- cv_df %>%
    dplyr :: select(starts_with("rmse_CNP")) %>%
    pivot_longer(
      everything(),
      names_to = "model",
      values_to = "rmse",
      names_prefix = "rmse_CNP_")

  cv_df$model <- factor(cv_df$model,levels = c("Nutcline_modG","Nutcline_modGS","Nutcline_modGI","Nutcline_modS","Nutcline_modI","Nutcline_modC"))

  levels(cv_df$model) <-c("modG","modGS","modGI","modS","modI","modC")
  saveRDS(cv_df, file)
  return(cv_df)
}


# POM_genomes_selected_gam_w_highlat <- POM_genomes_selected_gam_w_highlat %>%
#   mutate_at(c("Nutlim"),
#             replace_factor_na,
#             factorname = "Fe-lim")
# 
# data <- POM_genomes_selected_gam
# modG_CP <- b1_4vars_nutlim_modG_CP(data)
# modGS_CP <- b1_4vars_nutlim_modGS_CP(data)
# modGI_CP <- b1_4vars_nutlim_modGI_CP(data)
# modS_CP <- b1_4vars_nutlim_modS_CP(data)
# modI_CP <- b1_4vars_nutlim_modI_CP(data)
# modC_CP <- b1_4vars_nutlim_modC_CP(data)
# 
# set.seed(1)  # Run to replicate
# cv_df =
#     crossv_mc(data= POM_genomes_selected_gam_w_highlat, 100, test = 0.2) %>%
#     # crossv_mc(data= POM_genomes_selected_gam_w_highlat, 10, test = 0.2) %>%
#     mutate(
#         train = map(train, as_tibble),
#         test = map(test, as_tibble))
# cv_df =
#   cv_df %>%
#   mutate(
#     CP_Nutcline_modG  = map(train,~gam(formula(modG_CP$formula),method = "REML", family = "gaussian", data=.x)),
#     CP_Nutcline_modGS  = map(train,~gam(formula(modGS_CP$formula),method = "REML", family = "gaussian", data=.x)),
#     CP_Nutcline_modGI = map(train,~gam(formula(modGI_CP$formula),method = "REML", family = "gaussian", data=.x)),
#     CP_Nutcline_modS  = map(train,~gam(formula(modS_CP$formula),method = "REML", family = "gaussian", data=.x)),
#     CP_Nutcline_modI  = map(train,~gam(formula(modI_CP$formula),method = "REML", family = "gaussian", data=.x)),
#     CP_Nutcline_modC  = map(train,~gam(formula(modC_CP$formula),method = "REML", family = "gaussian", data=.x))
# ) %>%
#   mutate(
#     rmse_CP_Nutcline_modG = map2_dbl(CP_Nutcline_modG, test, ~rmse(model = .x, data = .y)),
#     rmse_CP_Nutcline_modGS = map2_dbl(CP_Nutcline_modGS, test, ~rmse(model = .x, data = .y)),
#     rmse_CP_Nutcline_modGI = map2_dbl(CP_Nutcline_modGI, test, ~rmse(model = .x, data = .y)),
#     rmse_CP_Nutcline_modS = map2_dbl(CP_Nutcline_modS, test, ~rmse(model = .x, data = .y)),
#     rmse_CP_Nutcline_modI = map2_dbl(CP_Nutcline_modI, test, ~rmse(model = .x, data = .y)),
#     rmse_CP_Nutcline_modC = map2_dbl(CP_Nutcline_modC, test, ~rmse(model = .x, data = .y))
#     )
# 
# cv_df <- cv_df %>%
#   dplyr :: select(starts_with("rmse_CP")) %>%
#   pivot_longer(
#     everything(),
#     names_to = "model",
#     values_to = "rmse",
#     names_prefix = "rmse_CP_") 
# 
# cv_df$model <- factor(cv_df$model,levels = c("Nutcline_modG","Nutcline_modGS","Nutcline_modGI","Nutcline_modS","Nutcline_modI","Nutcline_modC"))
# 
# levels(cv_df$model) <-c("modG_CP","modGS_CP","modGI_CP","modS_CP","modI_CP","modC_CP")
# 
# figsx_cv_cp <- ggplot(cv_df,aes(x = model, y = rmse)) + geom_violin() + stat_summary(fun.data = mean_sdl, geom="point", size=4, color="red") + stat_summary(aes(label=round(..y..,3)), fun=mean, geom="text", size=6, vjust = -0.5) + ggtitle("C:P cross validation") 
# figsx_cv_cp
# 
# Summary_table_CP <- AIC(
#   modG_CP,
#   modGS_CP,
#   modGI_CP,
#   modS_CP,
#   modI_CP,
#   modC_CP) %>%  
#   rownames_to_column(var= "Model") %>% 
#   mutate(deltaAIC = AIC - min(AIC)) %>%
#   mutate(rmse = with(cv_df, tapply(rmse, model, mean)))
# Summary_table_CP$R_squared<- c(
#   summary(modG_CP)$r.sq,
#   summary(modGS_CP)$r.sq,
#   summary(modGI_CP)$r.sq,
#   summary(modS_CP)$r.sq,
#   summary(modI_CP)$r.sq,
#   summary(modC_CP)$r.sq
# )
# 
# Summary_table_CP <- Summary_table_CP %>% mutate_if(is.numeric, round, digits = 3) %>% arrange(AIC)
# Summary_table_CP









