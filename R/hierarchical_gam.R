######################
# FUNCTIONS related to hierarchical GAM
######################
# Reference for hierarchical GAM: Pedersen et al. 2019, PeerJ, Hierarchical generalized additive models in ecology: An introduction with mgcv
# Model G = A global smoother (shape) for all observations
# Model GS = single common smoother plus group-level smoothers that have the same wiggliness
# Model GI = single common smoother plus group-level smoothers that have the different wiggliness (set m = 1 for the group level smoothers following Pedersen et al. 2019)
# Model S = Group-specific smoothers (shape) without a global smoother, but with all smoothers having the same wiggliness
# Model I = Group-specific smoothers with different wiggliness
# Model C = Control (no group level smoothers)

# Set the  number of basis function k = 4 to avoid over-fitting.
# ------------------------------------------------------------------------------------------------------------------
# Functions of hierarchical GAM (SST, NO3, Nutricline, Nutricline x Nutrient limitation)
# ------------------------------------------------------------------------------------------------------------------
# model GS #
b1_4vars_nutlim_modGS_CP <- function(data){
  b1 <- gam(logCP ~ s(SST, bs = "tp", k = 4) + 
              s(logNO3, bs = "tp", k = 4) +
              s(Nutcline_1uM_interp, bs = "tp", k = 4, m = 2) +
              s(Nutcline_1uM_interp, Nutlim, bs = "fs", k = 4, m = 2),
            data = data, method = "REML", family = "gaussian", na.action = na.omit)
  b1
  #Nutlim_effect_cnp_gam_summary(b1) # For conducting Tukey HSD 
}
b1_4vars_nutlim_modGS_NP <- function(data){
  b1 <- gam(logNP ~ s(SST, bs = "tp", k = 4) + 
              s(logNO3, bs = "tp", k = 4) +
              s(Nutcline_1uM_interp, bs = "tp", k = 4, m = 2) +
              s(Nutcline_1uM_interp, Nutlim, bs = "fs", k = 4, m = 2),
            data = data, method = "REML", family = "gaussian", na.action = na.omit)
  b1
  #Nutlim_effect_cnp_gam_summary(b1) # For conducting Tukey HSD 
}
b1_4vars_nutlim_modGS_CN <- function(data){
  b1 <- gam(logCN ~ s(SST, bs = "tp", k = 4) + 
              s(logNO3, bs = "tp", k = 4) +
              s(Nutcline_1uM_interp, bs = "tp", k = 4, m = 2) +
              s(Nutcline_1uM_interp, Nutlim, bs = "fs", k = 4, m = 2),
            data = data, method = "REML", family = "gaussian", na.action = na.omit)
  b1
  #Nutlim_effect_cnp_gam_summary(b1) # For conducting Tukey HSD 
}

# model G # 
b1_4vars_nutlim_modG_CP <- function(data){
  b1 <- gam(logCP ~ s(SST, bs = "tp", k = 4) + 
                  s(logNO3, bs = "tp", k = 4) + 
                  s(Nutcline_1uM_interp, bs = "tp", k = 4) +
                  s(Nutlim, k  = 4, bs = "re"),
                data, method = "REML", family = "gaussian", na.action = na.omit)
  b1
}
b1_4vars_nutlim_modG_NP <- function(data){
  b1 <- gam(logNP ~ s(SST, bs = "tp", k = 4) + 
              s(logNO3, bs = "tp", k = 4) + 
              s(Nutcline_1uM_interp, bs = "tp", k = 4) +
              s(Nutlim, k  = 4, bs = "re"),
            data, method = "REML", family = "gaussian", na.action = na.omit)
  b1
}
b1_4vars_nutlim_modG_CN <- function(data){
  b1 <- gam(logCN ~ s(SST, bs = "tp", k = 4) + 
              s(logNO3, bs = "tp", k = 4) + 
              s(Nutcline_1uM_interp, bs = "tp", k = 4) +
              s(Nutlim, k  = 4, bs = "re"),
            data, method = "REML", family = "gaussian", na.action = na.omit)
  b1
}

# model GI #
b1_4vars_nutlim_modGI_CP <- function(data){
  b1 <- gam(logCP ~ s(SST, bs = "tp", k = 4) + 
                   s(logNO3, bs = "tp", k = 4) +                               
                   s(Nutcline_1uM_interp, bs = "tp", k = 4, m = 2) +
                   s(Nutcline_1uM_interp, by = Nutlim, bs = "tp", m = 1) + 
                   s(Nutlim, bs = "re", k = 4),
                 data, method = "REML", family = "gaussian", na.action = na.omit)
  b1
}
b1_4vars_nutlim_modGI_NP <- function(data){
  b1 <- gam(logNP ~ s(SST, bs = "tp", k = 4) + 
              s(logNO3, bs = "tp", k = 4) +                               
              s(Nutcline_1uM_interp, bs = "tp", k = 4, m = 2) +
              s(Nutcline_1uM_interp, by = Nutlim, bs = "tp", m = 1) + 
              s(Nutlim, bs = "re", k = 4),
            data, method = "REML", family = "gaussian", na.action = na.omit)
  b1
}
b1_4vars_nutlim_modGI_CN <- function(data){
  b1 <- gam(logCN ~ s(SST, bs = "tp", k = 4) + 
              s(logNO3, bs = "tp", k = 4) +                               
              s(Nutcline_1uM_interp, bs = "tp", k = 4, m = 2) +
              s(Nutcline_1uM_interp, by = Nutlim, bs = "tp", m = 1) + 
              s(Nutlim, bs = "re", k = 4),
            data, method = "REML", family = "gaussian", na.action = na.omit)
  b1
}
# model S #
b1_4vars_nutlim_modS_CP <- function(data){
  b1 <- gam(logCP ~ s(Nutcline_1uM_interp, Nutlim, bs = "fs", k = 4, m = 2) +
                  s(logNO3, bs = "tp", k = 4) +
                  s(SST, bs = "tp", k = 4) ,
                data, method = "REML", family = "gaussian")
  b1
}
b1_4vars_nutlim_modS_NP <- function(data){
  b1 <- gam(logNP ~ s(Nutcline_1uM_interp, Nutlim, bs = "fs", k = 4, m = 2) +
              s(logNO3, bs = "tp", k = 4) +
              s(SST, bs = "tp", k = 4) ,
            data, method = "REML", family = "gaussian")
  b1
}
b1_4vars_nutlim_modS_CN <- function(data){
  b1 <- gam(logCN ~ s(Nutcline_1uM_interp, Nutlim, bs = "fs", k = 4, m = 2) +
              s(logNO3, bs = "tp", k = 4) +
              s(SST, bs = "tp", k = 4) ,
            data, method = "REML", family = "gaussian")
  b1
}
# model I #
b1_4vars_nutlim_modI_CP <- function(data){
  b1 <- gam(logCP ~ s(Nutcline_1uM_interp, by = Nutlim, bs = "tp", k = 4, m = 2) + 
                  s(logNO3, bs = "tp", k = 4) + 
                  s(SST, bs = "tp", k = 4) +
                  s(Nutlim, bs = "re", k = 4),
                data, method = "REML", family = "gaussian")
  b1
}
b1_4vars_nutlim_modI_NP <- function(data){
  b1 <- gam(logNP ~ s(Nutcline_1uM_interp, by = Nutlim, bs = "tp", k = 4, m = 2) + 
              s(logNO3, bs = "tp", k = 4) + 
              s(SST, bs = "tp", k = 4) +
              s(Nutlim, bs = "re", k = 4),
            data, method = "REML", family = "gaussian")
  b1
}
b1_4vars_nutlim_modI_CN <- function(data){
  b1 <- gam(logCN ~ s(Nutcline_1uM_interp, by = Nutlim, bs = "tp", k = 4, m = 2) + 
              s(logNO3, bs = "tp", k = 4) + 
              s(SST, bs = "tp", k = 4) +
              s(Nutlim, bs = "re", k = 4),
            data, method = "REML", family = "gaussian")
  b1
}
# model C (Control) #
b1_4vars_nutlim_modC_CP <- function(data){
  b1 <- gam(logCP ~ s(SST, bs = "tp", k = 4) +
                  s(logNO3, bs = "tp", k = 4) + 
                  s(Nutcline_1uM_interp, bs = "tp", k = 4),
                data, method = "REML", family = "gaussian")
  b1
}
b1_4vars_nutlim_modC_NP <- function(data){
  b1 <- gam(logNP ~ s(SST, bs = "tp", k = 4) +
              s(logNO3, bs = "tp", k = 4) + 
              s(Nutcline_1uM_interp, bs = "tp", k = 4),
            data, method = "REML", family = "gaussian")
  b1
}
b1_4vars_nutlim_modC_CN <- function(data){
  b1 <- gam(logCN ~ s(SST, bs = "tp", k = 4) +
              s(logNO3, bs = "tp", k = 4) + 
              s(Nutcline_1uM_interp, bs = "tp", k = 4),
            data, method = "REML", family = "gaussian")
  b1
}

# Temporary
# Function to predict C:P under different nutrient limitation under varying SST for a given model and prediction data

# mod_CP_pred_Nutcline_nutlim <- function(model,pred_data) {
#   mod_CP_pred <- pred_data
#   mod_CP_pred <- cbind(mod_CP_pred,
#                        predict(model,
#                                mod_CP_pred,
#                                se.fit=TRUE,
#                                type="response"))
# }
# 
# # Function to predict N:P under different nutrient limitation under varying SST for a given model and prediction data
# 
# mod_NP_pred_Nutcline_nutlim <- function(model,pred_data) {
#   mod_NP_pred <- pred_data
#   mod_NP_pred <- cbind(mod_NP_pred,
#                        predict(model,
#                                mod_NP_pred,
#                                se.fit=TRUE,
#                                type="response"))
# }
# 
# # Function to predict C:N under different nutrient limitation under varying SST for a given model and prediction data
# 
# mod_CN_pred_Nutcline_nutlim <- function(model,pred_data) {
#   mod_CN_pred <- pred_data
#   mod_CN_pred <- cbind(mod_CN_pred,
#                        predict(model,
#                                mod_CN_pred,
#                                se.fit=TRUE,
#                                type="response"))
# }
# 
# # Function to plot C:P under different nutrient limitation under varying Nutricline depth for a given model and prediction data with observation
# 
# plot_CP_pred_Nutcline_nutlim <- function(mod_CP_pred, data, model_name) {
#   {ggplot(data, aes(x=Nutcline_1uM_interp, y=exp(logCP), group=Nutlim)) +
#       facet_wrap(~Nutlim) +
#       geom_ribbon(aes(ymin=exp(fit - 2*se.fit), ymax=exp(fit + 2*se.fit), x=Nutcline_1uM_interp),
#                   data=mod_CP_pred,
#                   alpha=0.3,
#                   inherit.aes=FALSE) +
#       xlim(0,210) +
#       ylim(60, 255) +
#       coord_cartesian(xlim = c(0,210), ylim = c(60, 255)) +
#       scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
#       scale_y_continuous(breaks = c(75, 100, 125, 150, 175, 200, 225),
#                          labels = c(75, 100, 125, 150, 175, 200, 225)) +
#       geom_jitter(aes(color = Nutlim), size = 0.8) +
#       geom_line(aes(y=exp(fit)), data=mod_CP_pred) +
#       ggtitle(model_name) +
#       labs(x = 'Nutricline (m)', y = 'C:P')} +
#     theme_bw() +
#     theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.position = "none")
# }
# 
# # Function to plot N:P under different nutrient limitation under varying Nutricline for a given model and prediction data with observation
# 
# plot_NP_pred_Nutcline_nutlim <- function(mod_NP_pred, data, model_name) {
#   {ggplot(data, aes(x=Nutcline_1uM_interp, y=exp(logNP), group=Nutlim)) +
#       facet_wrap(~Nutlim) +
#       geom_ribbon(aes(ymin=exp(fit - 2*se.fit), ymax=exp(fit + 2*se.fit), x=Nutcline_1uM_interp),
#                   data=mod_NP_pred,
#                   alpha=0.3,
#                   inherit.aes=FALSE) +
#       xlim(0,210) +
#       ylim(10, 36) +
#       coord_cartesian(xlim = c(0,210), ylim = c(10, 36)) +
#       scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
#       scale_y_continuous(breaks = c(12, 16, 20, 24, 28,32),
#                          labels = c(12, 16, 20, 24, 28,32)) +
#       geom_jitter(aes(color = Nutlim), size = 0.8) +
#       geom_line(aes(y=exp(fit)), data=mod_NP_pred) +
#       ggtitle(model_name) +
#       labs(x = 'Nutricline (m)', y = 'N:P')} +
#     theme_bw() +
#     theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.position = "none")
# }
# 
# # Function to plot C:N under different nutrient limitation under varying Nutricline for a given model and prediction data with observation
# 
# plot_CN_pred_Nutcline_nutlim <- function(mod_CN_pred, data, model_name) {
#   {ggplot(data, aes(x=Nutcline_1uM_interp, y=exp(logCN), group=Nutlim)) +
#       facet_wrap(~Nutlim) +
#       geom_ribbon(aes(ymin=exp(fit - 2*se.fit), ymax=exp(fit + 2*se.fit), x=Nutcline_1uM_interp),
#                   data=mod_CN_pred,
#                   alpha=0.3,
#                   inherit.aes=FALSE) +
#       xlim(0,210) +
#       ylim(5, 9) +
#       coord_cartesian(xlim = c(0,210), ylim = c(5, 9)) +
#       geom_jitter(aes(color = Nutlim), size = 0.8) +
#       geom_line(aes(y=exp(fit)), data=mod_CN_pred) +
#       ggtitle(model_name) +
#       labs(x = 'Nutricline (m)', y = 'C:N')} +
#     theme_bw() +
#     theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.position = "none")
# }
# 
# # Test plotting
# # setup prediction data
# data <- POM_genomes_selected_gam
# mod_CP_pred <- with(data,
#                     expand.grid(Nutcline_1uM_interp =seq(min(0), max(300), length=300),
#                                 SST= mean(data$SST),
#                                 logNO3 = mean(data$logNO3),
#                                 Nutlim=levels(Nutlim)))
# # SST= 18))
# mod_NP_pred <- mod_CP_pred
# mod_CN_pred <- mod_CP_pred
# 
# modGI_CP_pred <- mod_CP_pred_Nutcline_nutlim(b1_4vars_nutlim_modGI_CP(data),mod_CP_pred)
# plot_CP_pred_Nutcline_nutlim(modGI_CP_pred, data, "model GI")

# ------------------------------------------------------------------------------------------------------------------
# Functions for cross validation comparison to get RMSE for each model (takes about 10 minutes, so comment out when necessary)
# Generate 100 random partitions holding out 20% of observations to calculate AIC
# ------------------------------------------------------------------------------------------------------------------
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









