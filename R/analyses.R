###################
# GENERAL FUNCTIONS
###################

# Custom function to calculate mode
getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Function to inserting a row in data frame
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

# Function to replace NA with factor value
replace_factor_na <- function(x, factorname){
  x <- as.character(x)
  x <- if_else(is.na(x), factorname, x)
  x <- as.factor(x)
}

# Function to calculate 95% confidence interval for weighted mean Following Eqn 6 of Hedges et al. 1999, Ecology
weighted.ttest.ci <- function(x, weights, conf.level = 0.95) {
    require(Hmisc)
    nx <- length(x)
    df <- nx - 1
    vx <- wtd.var(x, weights, normwt = TRUE) ## From Hmisc
    mx <- weighted.mean(x, weights)
    stderr <- sqrt(vx/nx)
    tstat <- mx/stderr ## not mx - mu
    alpha <- 1 - conf.level
    cint <- qt(1 - alpha/2, df)
    cint <- tstat + c(-cint, cint)
    cint * stderr
}

# Function for conducting Tukey HSD test on Nutrient limitation as a categorial variable with full gam model
Nutlim_effect_cnp_gam_summary <- function(model) {
  contr <- matrix(0, nrow = 6, ncol = length(coef(model)))
  colnames(contr) <- names(coef(model))
  rownames(contr) <- c("PN - P", "N - P", "Fe - P", "N - PN", "Fe - PN","Fe - N")
  contr[, 2:4] <- rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1),c(-1, 1, 0), c(-1, 0, 1), c(0, -1, 1))
  g2 <- glht(model, linfct = contr)
  summary(g2)
}

# Function for computing mean and SE for the intercept for different nutrient limitation
Nutlim_effect_cnp_gam_intercept <- function(model,data) {
  Nutlim_names <- c("P-lim","PN-colim", "N-lim","Fe-lim")
  out <- summary(model)
  mean_intercept<- (c(out$p.coeff[1], 
                   out$p.coeff[1]+out$p.coeff[2], 
                   out$p.coeff[1]+out$p.coeff[3],
                   out$p.coeff[1]+out$p.coeff[4]))
  se_intercept <- (c(out$se[1], 
                   out$se[2], 
                   out$se[3],
                   out$se[4]))
  n <- data %>% group_by(sp_Nutlim_CESM2) %>% summarise(n = n())
  nsamples <- n[,2]
  intercept.frame <- data.frame(Nutlim_names, 
                                mean = exp(mean_intercept), 
                                SE = exp(mean_intercept)*se_intercept,
                                nsamples = nsamples)
  colnames(intercept.frame) <- c("Nutlim", "mean","SE","n")
  intercept.frame
}

# Function to bin data by 1 by 1 grid
bin_data_1by1 <- function(data) {
  lat_grid <- seq(from = -89.5, to = 89.5, by = 1.0)
  lon_grid <- seq(from = -179.5, to = 179.5, by = 1.0)
  if(!"Nutlim" %in% colnames(data))
    {
  data$Nutlim = NA
  }
  data_binned <- data %>% 
  mutate(binlon = cut(Longitude, seq(from = -180.0, to = 180.0, by = 1.0), include.lowest = T, right = F), binlat = cut(Latitude, seq(from = -90.0, to = 90.0, by = 1.0), include.lowest = T, right = F)
         )  %>% group_by(binlat, binlon) %>%  summarise(POCavg = mean(POCavg_uM, na.rm = TRUE),
                                                        PONavg = mean(PONavg_uM, na.rm = TRUE),
                                                        POPavg = mean(POPavg_nM, na.rm = TRUE),
                                                        logCP = mean(logCP, na.rm = TRUE), 
                                                        logNP = mean(logNP, na.rm = TRUE), 
                                                        logCN = mean(logCN, na.rm = TRUE),
                                                        absLatitude = mean(absLatitude, na.rm = TRUE),
                                                        SST = mean(Temperature, na.rm = TRUE),
                                                        PO4 = mean(Phosphate, na.rm = TRUE),
                                                        NO3 = mean(DIN, na.rm = TRUE),
                                                        Nutcline_1uM_interp = mean(Nutricline_1uM_Interp, na.rm = TRUE),
                                                        MLD_Holte17 = mean(MLD_Holte17, na.rm = TRUE),
                                                        MLPAR = mean(MLPAR_surfmean_umols, na.rm = TRUE),
                                                        CHLOR_a_MODIS = mean(CHLOR_a_MODIS, na.rm = TRUE),
                                                        frac_dia_NOBM = mean(frac_dia_NOBM, na.rm = TRUE),
                                                        frac_coc_NOBM = mean(frac_coc_NOBM, na.rm = TRUE),
                                                        frac_cya_NOBM = mean(frac_cya_NOBM, na.rm = TRUE),
                                                        frac_chloro_NOBM = mean(frac_chloro_NOBM, na.rm = TRUE),
                                                        region =  ifelse(absLatitude < 15, "Tropical",
                                                                         ifelse(absLatitude >= 15 & absLatitude < 45, "Subtropical",
                                                                                ifelse(absLatitude >= 45 & absLatitude < 65, "Subpolar",
                                                                                       "Polar"))),
                                                        sp_Nutlim_CESM2 = getmode(sp_Nutlim_CESM2),
                                                        Nutlim = getmode(Nutlim)
            )  %>% replace_with_na_all(condition = ~.x == -Inf) %>% mutate(Latitude = lat_grid[as.integer(binlat)],Longitude = lon_grid[as.integer(binlon)])
data_binned$region <- factor(data_binned$region,
                                      levels = c("Polar", "Subpolar", "Subtropical","Tropical"))
data_binned$area_weights = cos(deg2rad(data_binned$Latitude))
data_binned$Nutlim = factor(data_binned$Nutlim,
                                     levels = c("P-lim","PN-colim", "N-lim","Fe-lim"))
data_binned$sp_Nutlim_CESM2 = factor(data_binned$sp_Nutlim_CESM2,
                                     levels = c("P-lim","PN-colim", "N-lim","Fe-lim"))
data_binned
}

# Function to prepare dataset for GAM analyses
clean_data_for_gam <- function(data) {
  data_for_gam <- data %>% dplyr::mutate(NO3 = ifelse(DIN < 0.1, 0.1, DIN),
                                         PO4 = ifelse(Phosphate < 0.01, 0.01, Phosphate),
                                         logNO3 = log(NO3),
                                         logPO4 = log(PO4),
                                         SST = Temperature,
                                         Nutcline_1uM_interp = Nutricline_1uM_Interp) %>% dplyr::select(logCP, logNP, logCN,
                                                                              SST, logNO3, logPO4,
                                                                              Nutcline_1uM_interp, sp_Nutlim_CESM2,
                                                                              absLatitude,
                                                                              contains("Nutlim")
                                          ) %>% drop_na(logCP, logNP, logCN) %>% replace_with_na_all(condition = ~.x == -Inf) %>% tidyr::drop_na(SST, logNO3, logPO4, Nutcline_1uM_interp) 

  if("Nutlim" %in% colnames(data_for_gam))
    {
  data_for_gam$Nutlim = factor(data_for_gam$Nutlim,
                                     levels = c("P-lim","PN-colim", "N-lim","Fe-lim"))
  }
  data_for_gam
}

# Function to combine POM_genomes_selected_gam with POM_all_gam with max abslatitude1
combine_data_global_gam <- function(POM_all_gam,POM_genomes_selected_gam) {
  POM_all_gam_selected <- POM_all_gam
  POM_all_gam_selected$Nutlim <- rep(NA,length(POM_all_gam_selected$sp_Nutlim_CESM2))
  POM_all_gam_selected <- POM_all_gam_selected[POM_all_gam_selected$absLatitude > max(POM_genomes_selected_gam$absLatitude),]
  POM_genomes_selected_gam_w_highlat <- rbind(POM_genomes_selected_gam,POM_all_gam_selected)
  # Reorder factor level in nutrient limitation so P-limiation is plotted last.
  POM_genomes_selected_gam_w_highlat$Nutlim <- factor(POM_genomes_selected_gam_w_highlat$Nutlim,levels = rev(levels(POM_genomes_selected_gam_w_highlat$Nutlim)))
  # data_all_for_boxplot <- drop_na(data_all, Nutlim)
  POM_genomes_selected_gam_w_highlat
}

# Function to calculate area-weighted global mean C:N:P
calc_cnp_global_mean <- function(POM_all) {
  cnp_global <- summarise(POM_all, 
          meancp_global = exp(weighted.mean(logCP, area_weights)),
          meancp_ci_lb = exp(weighted.ttest.ci(logCP, area_weights)[1]),
          meancp_ci_ub = exp(weighted.ttest.ci(logCP, area_weights)[2]),
          meannp_global = exp(weighted.mean(logNP, area_weights)),
          meannp_ci_lb = exp(weighted.ttest.ci(logNP, area_weights)[1]),
          meannp_ci_ub = exp(weighted.ttest.ci(logNP, area_weights)[2]),          
          meancn_global = exp(weighted.mean(logCN, area_weights)),
          meancn_ci_lb = exp(weighted.ttest.ci(logCN, area_weights)[1]),
          meancn_ci_ub = exp(weighted.ttest.ci(logCN, area_weights)[2]),          
          nsamples = n())
  cnp_global
}

# Function to separate high latitude with absolute latitude of 45 degrees
sep_data_highlat <- function(data,latitude = 45) {
  data_highlat <- dplyr::filter(data, absLatitude >= latitude)  
  data_highlat
}

# Function to separate low latitude data with absolute latitude of 45 degrees
sep_data_lowlat <- function(data,latitude = 45) {
  data_lowlat <- dplyr::filter(data, absLatitude < latitude)
  data_lowlat
}

# Function to select variables for Correlation analyses and standardize data
clean_data_for_corr <- function(POM_all) {
  POM_all_corr <- POM_all %>% dplyr::mutate(NO3 = ifelse(DIN < 0.1, 0.1, DIN),
                                            PO4 = ifelse(Phosphate < 0.01, 0.01, Phosphate),
                                            logNO3 = log(NO3),
                                            logPO4 = log(PO4),
                                            logFeT = log(FeT_CESM),
                                            SST = Temperature,
                                            MLPAR = MLPAR_surfmean_umols,
                                            Nutcline_1uM_interp = Nutricline_1uM_Interp) %>% dplyr::select(logCP, logNP, logCN,
                  absLatitude, SST, logNO3, logPO4,
                  logFeT, Nutcline_1uM_interp, MLD_Holte17,
                  MLPAR, CHLOR_a_MODIS,
                  frac_dia_NOBM, frac_coc_NOBM, frac_chloro_NOBM, frac_cya_NOBM) %>% drop_na(logCP, logNP, logCN) %>% replace_with_na_all(condition = ~.x == -Inf)  %>% drop_na(absLatitude, 
                                                                                                                                                                               SST, 
                                                                                                                                                                               logNO3, 
                                                                                                                                                                               logPO4,
                                                                                                                                                                               logFeT, 
                                                                                                                                                                               Nutcline_1uM_interp, 
                                                                                                                                                                               MLD_Holte17,
                                                                                                                                                                               MLPAR, 
                                                                                                                                                                               CHLOR_a_MODIS,
                                                                                                                                                                               frac_dia_NOBM, 
                                                                                                                                                                               frac_coc_NOBM, 
                                                                                                                                                                               frac_cya_NOBM, 
                                                                                                                                                                               frac_chloro_NOBM)
  scaled.POM_all_corr <- data.frame(scale(POM_all_corr[,1:ncol(POM_all_corr)]))
}

# Function to get correlation matrix with column and rownames
M.POM_corr <- function(scaled.POM_all_corr, highlat = FALSE, colnames = FALSE) {
  # if (highlat) {
  # For nutricline manually assign value of 0
  # scaled.POM_all_corr$Nutcline_1uM_interp <- rep(0,length(scaled.POM_all_corr$Nutcline_1uM_interp))
  # }
  M.scaled.POM_corr <- cor(scaled.POM_all_corr,
                                  method = c("pearson"), 
                                  use = "pairwise.complete.obs")
  M.POM_corr_selected <- M.scaled.POM_corr[1:3,4:ncol(M.scaled.POM_corr)] 
  if (colnames) {
  colnames(M.POM_corr_selected) <- paste(c("Abs.Latitude",
                                           "SST","Nitrate","Phosphate", "FeT", 
                                           "Nutricline", 
                                           "MLD", "MLPAR", "Chl-a",
                                           "% Diatoms", "% Cocco","% Chloro","% Cya"
                                           ))
  } else {
  colnames(M.POM_corr_selected) <- paste(c("",
                                           "","","","",
                                           "", 
                                           "", "", "",
                                           "", "","",""
                                           ))    
  }
  rownames(M.POM_corr_selected) <- paste(c("C:P", "N:P", "C:N"))
  M.POM_corr_selected
}

# Function to get pvalue for correlation matrix
testRes.POM_corr <- function(scaled.POM_all_corr, highlat = FALSE) {
  # if (highlat) {
  # # For nutricline manually assign value of 0
  # scaled.POM_all_corr$Nutcline_1uM_interp <- rep(0,length(scaled.POM_all_corr$Nutcline_1uM_interp))
  # } 
  M.scaled.POM_corr <- cor(scaled.POM_all_corr,
                                  method = c("pearson"), 
                                  use = "pairwise.complete.obs")
  M.POM_corr_selected <- M.scaled.POM_corr[1:3,4:ncol(M.scaled.POM_corr)] 
  testRes = cor.mtest(scaled.POM_all_corr,method = c("pearson"), use = "pairwise.complete.obs", conf.level = 0.95)
  testRes_selected = testRes$p[1:3,4:ncol(M.scaled.POM_corr)]
  testRes_selected
}

# Function to make Nutlim Data to overlay on top of CESM model nutrient limitation output
make_obsNutlim_overlay_cesm <- function(POM_all_binned, POM_genomes_selected_binned) {
  POM_all_binned_selected <- POM_all_binned
  POM_all_binned_selected$Nutlim <- rep(NA,length(POM_all_binned_selected$sp_Nutlim_CESM2))
  
  POM_all_binned_selected <- POM_all_binned_selected[POM_all_binned_selected$absLatitude > max(POM_genomes_selected_binned$absLatitude),]
  POM_genomes_selected_binned_w_highlat <- rbind(POM_genomes_selected_binned,POM_all_binned_selected)
  return(POM_genomes_selected_binned_w_highlat)
}


####################################################
# FUNCTIONS RELATED TO MAIN FIG.2
####################################################

# Function to conduct GAM with 2 variables with no interactions
b1_2vars <- function(xvar1, xvar2, yvar, data) {
  b1 <- gam(as.formula(paste(yvar, "~", "s(",xvar1,")","+", "s(",xvar2,")")),data = data, method ="REML",na.action = na.omit)
}

# Function to conduct GAM with 3 variables with no interactions
b1_3vars <- function(xvar1, xvar2, xvar3, yvar, data) {
  b1 <- gam(as.formula(paste(yvar, "~", "s(",xvar1,")","+", "s(",xvar2,")","+", "s(",xvar3,")")),data = data, method ="REML",na.action = na.omit)
}

# Functions to conduct GAM with 4 predetermined variables (SST, NO3, Nutricline, Nutlim) with interactions between Nutricline and Nutlim under model GS
# 1. C:P
b1_4vars_nutlim_modGS_CP <- function(data){
  b1 <- gam(logCP ~ s(SST, bs = "tp", k = 4) + 
              s(logNO3, bs = "tp", k = 4) +
              s(Nutcline_1uM_interp, bs = "tp", k = 4, m = 2) +
              s(Nutcline_1uM_interp, Nutlim, bs = "fs", k = 4, m = 2),
            data = data, method = "REML", family = "gaussian", na.action = na.omit)
  b1
  #Nutlim_effect_cnp_gam_summary(b1) # For conducting Tukey HSD 
}
# 2. N:P
b1_4vars_nutlim_modGS_NP <- function(data){
  b1 <- gam(logNP ~ s(SST, bs = "tp", k = 4) + 
              s(logNO3, bs = "tp", k = 4) +
              s(Nutcline_1uM_interp, bs = "tp", k = 4, m = 2) +
              s(Nutcline_1uM_interp, Nutlim, bs = "fs", k = 4, m = 2),
            data = data, method = "REML", family = "gaussian", na.action = na.omit)
  b1
  #Nutlim_effect_cnp_gam_summary(b1) # For conducting Tukey HSD 
}
# 3. C:N
b1_4vars_nutlim_modGS_CN <- function(data){
  b1 <- gam(logCN ~ s(SST, bs = "tp", k = 4) + 
              s(logNO3, bs = "tp", k = 4) +
              s(Nutcline_1uM_interp, bs = "tp", k = 4, m = 2) +
              s(Nutcline_1uM_interp, Nutlim, bs = "fs", k = 4, m = 2),
            data = data, method = "REML", family = "gaussian", na.action = na.omit)
  b1
  #Nutlim_effect_cnp_gam_summary(b1) # For conducting Tukey HSD 
}

# Function to extract p-values from GAM and convert to significance symbols
get_pval_gam <- function(b1) {
  pvalue_gam <- p.adjust(summary(b1)$s.table[,4])
  pvalue_gam <- ifelse(pvalue_gam < 0.001, "***", 
                        ifelse(pvalue_gam < 0.01, "**",
                               ifelse(pvalue_gam < 0.05, "*",
                                      "n.s.")))
  pvalue_gam
}

# Function to make CNP gam p-value summary table for high latitude
make_CNP_pval_highlat <- function(data = POM_highlat_gam) {
  xvar1 <- "SST"
  xvar2 <- "logNO3"  
  xvar3 <- "Nutcline_1uM_interp" 
  cp_pvalue_highlat <- get_pval_gam(b1_3vars(xvar1, xvar2, xvar3, "logCP", data))
  cp_pvalue_highlat<- R.utils::insert(cp_pvalue_highlat,ats=4,values=c("n.s.",""))  
  np_pvalue_highlat <- get_pval_gam(b1_3vars(xvar1, xvar2, xvar3, "logNP", data))
  np_pvalue_highlat<- R.utils::insert(np_pvalue_highlat,ats=4,values=c("n.s.",""))  
  cn_pvalue_highlat <- get_pval_gam(b1_3vars(xvar1, xvar2, xvar3, "logCN", data))
  cn_pvalue_highlat<- R.utils::insert(cn_pvalue_highlat,ats=4,values=c("n.s.","")) 
  CNP_highlat_pval <- as.data.frame(cbind(cp_pvalue_highlat,np_pvalue_highlat,cn_pvalue_highlat))
  rownames(CNP_highlat_pval) <- c("SST", "Nitrate","Nutricline", "Nutricline x Nutlim","Total")
  colnames(CNP_highlat_pval) <- c('C:P','N:P','C:N')  
  CNP_highlat_pval
}

# Function to make CNP gam p-value summary table for low latitude
make_CNP_pval_lowlat <- function(data = POM_lowlat_gam) {
  cp_pvalue_lowlat <- get_pval_gam(b1_4vars_nutlim_modGS_CP(data))
  cp_pvalue_lowlat<- R.utils::insert(cp_pvalue_lowlat,ats=5,values=c(""))  
  np_pvalue_lowlat <- get_pval_gam(b1_4vars_nutlim_modGS_NP(data))
  np_pvalue_lowlat<- R.utils::insert(np_pvalue_lowlat,ats=5,values=c(""))  
  cn_pvalue_lowlat <- get_pval_gam(b1_4vars_nutlim_modGS_CN(data))
  cn_pvalue_lowlat<- R.utils::insert(cn_pvalue_lowlat,ats=5,values=c(""))  
  CNP_lowlat_pval <- as.data.frame(cbind(cp_pvalue_lowlat,np_pvalue_lowlat,cn_pvalue_lowlat))
  rownames(CNP_lowlat_pval) <- c("SST", "Nitrate","Nutricline", "Nutricline x Nutlim","Total")
  colnames(CNP_lowlat_pval) <- c('C:P','N:P','C:N')  
  CNP_lowlat_pval
}

# Function to calculate deviance explained in high latitudes with 3 variables (SST, NO3, Nutricline)
calc_devexpl_highlat <- function(xvar1, xvar2, xvar3, yvar, data) {
  result_highlat <- deviance3variables(xvar1, xvar2, xvar3, yvar, data)
  result_highlat<- R.utils::insert(result_highlat,ats=4,values=c(0))  
  result_highlat
}

# Function to make CNP deviance explained summary table for high latitude
make_CNP_devexpl_highlat <- function(data = POM_highlat_gam) {
  xvar1 <- "SST"
  xvar2 <- "logNO3" 
  xvar3 <- "Nutcline_1uM_interp"
  result_cp_highlat <- calc_devexpl_highlat(xvar1, xvar2, xvar3, "logCP", data)
  result_np_highlat <- calc_devexpl_highlat(xvar1, xvar2, xvar3, "logNP", data)
  result_cn_highlat <- calc_devexpl_highlat(xvar1, xvar2, xvar3, "logCN", data)
  CNP_highlat_devexpl <- as.data.frame(cbind(result_cp_highlat[1:5],result_np_highlat[1:5],result_cn_highlat[1:5]))
  rownames(CNP_highlat_devexpl) <- c("SST", "Nitrate","Nutricline", "Nutricline x Nutlim",'Total')
  colnames(CNP_highlat_devexpl) <- c('C:P','N:P','C:N')  
  round(CNP_highlat_devexpl, digits = 3)
}

# Function to make CNP deviance explained summary table for low latitude
make_CNP_devexpl_lowlat <- function(data = POM_lowlat_gam) {
  result_cp_lowlat <- deviance4variables_nutlim_modGS_CP(data)
  result_np_lowlat <- deviance4variables_nutlim_modGS_NP(data)
  result_cn_lowlat <- deviance4variables_nutlim_modGS_CN(data)
  CNP_lowlat_devexpl <- as.data.frame(cbind(result_cp_lowlat[1:5],result_np_lowlat[1:5],result_cn_lowlat[1:5]))
  rownames(CNP_lowlat_devexpl) <- c("SST", "Nitrate","Nutricline", "Nutricline x Nutlim",'Total')
  colnames(CNP_lowlat_devexpl) <- c('C:P','N:P','C:N')  
  round(CNP_lowlat_devexpl, digits = 3)
}

####################################################
# FUNCTIONS RELATED TO MAIN FIG.3
####################################################
# Function to make GAM models with default parameter settings with no nutrient limitation effect
# Models are saved as global functions 
make_mod_CNP_no_Nutlim <- function(data_all) {
  mod_CP <-  gam(logCP ~ s(logNO3) + s(SST) + s(Nutcline_1uM_interp),
                       data  = data_all, method = "REML", family = "gaussian")
  mod_NP <-  gam(logNP ~ s(logNO3) + s(SST) + s(Nutcline_1uM_interp),
                       data  = data_all, method = "REML", family = "gaussian")
  mod_CN <-  gam(logCN ~ s(logNO3) + s(SST) + s(Nutcline_1uM_interp),
                       data  = data_all, method = "REML", family = "gaussian")
  mod_CNP <- list("mod_CP"= mod_CP, "mod_NP" = mod_NP, "mod_CN" = mod_CN)
  return(mod_CNP)
}

# Function to make SST based gam prediction of CNP (Nutricline and Nitrate kept constant)
make_mod_CNP_SST_pred <- function(data_all, mod_CNP) {
  mod_CNP_SST_pred <- with(data_all,
                      expand.grid(SST=seq(min(SST), max(35), length=100),
                                  Nutcline_1uM_interp = mean(data_all$Nutcline_1uM_interp),
                                  logNO3 = mean(data_all$logNO3)))
  mod_CNP_SST_pred <- cbind(mod_CNP_SST_pred,
                       predict(mod_CNP, 
                               mod_CNP_SST_pred, 
                               se.fit=TRUE, 
                               type="response"))
  mod_CNP_SST_pred
}

# Function to make Nitrate based gam prediction of CNP (Nutricline and SST kept constant)
make_mod_CNP_Nitrate_pred <- function(data_all, mod_CNP) {
  mod_CNP_Nitrate_pred <- with(data_all,
                      expand.grid(logNO3=seq(min(-6), max(4), length=100),
                                  Nutcline_1uM_interp = mean(data_all$Nutcline_1uM_interp),
                                  SST= mean(data_all$SST)))
  mod_CNP_Nitrate_pred <- cbind(mod_CNP_Nitrate_pred,
                       predict(mod_CNP, 
                               mod_CNP_Nitrate_pred, 
                               se.fit=TRUE, 
                               type="response"))
  mod_CNP_Nitrate_pred
} 

# These are same gam functions used in Figure 2, so will recycle them for consistency
make_mod_CNP_Nutcline_Nutlim_modGS <- function(data_all) {
  mod_CP_Nutcline_Nutlim_modGS <- b1_4vars_nutlim_modGS_CP(data_all)
  mod_NP_Nutcline_Nutlim_modGS <- b1_4vars_nutlim_modGS_NP(data_all)
  mod_CN_Nutcline_Nutlim_modGS <- b1_4vars_nutlim_modGS_CN(data_all)
  mod_CNP_Nutcline_Nutlim_modGS <- list("mod_CP_Nutcline_Nutlim_modGS"= mod_CP_Nutcline_Nutlim_modGS, 
                                        "mod_NP_Nutcline_Nutlim_modGS" = mod_NP_Nutcline_Nutlim_modGS, 
                                        "mod_CN_Nutcline_Nutlim_modGS" = mod_CN_Nutcline_Nutlim_modGS)
  return(mod_CNP_Nutcline_Nutlim_modGS)
}

# Function to predict CNP under different nutrient limitation under varying SST for a given model and prediction data
pred_CNP_Nutcline_nutlim <- function(model,pred_data) {
  mod_CNP_pred <- pred_data
  mod_CNP_pred <- cbind(mod_CNP_pred,
                       predict(model, 
                               mod_CNP_pred, 
                               se.fit=TRUE, 
                               type="response"))
}

# Function to make Nutricline x Nutlim based gam prediction of CNP (Nitrate and SST kept constant)
make_mod_CNP_Nutcline_Nutlim_pred <- function(data_all, mod_CNP_Nutcline_Nutlim_modGS) {
  mod_CNP_Nutcline_Nutlim_pred <- with(data_all,
                      expand.grid(Nutcline_1uM_interp =seq(min(0), max(300), length=300),
                                  SST= mean(data_all$SST),
                                  logNO3 = mean(data_all$logNO3),
                                  Nutlim=levels(Nutlim)))
  # mod_CNP_Nutcline_Nutlim_pred <- cbind(mod_CNP_Nutcline_Nutlim_pred,
  #                     predict(mod_CNP_Nutcline_Nutlim_modGS, 
  #                             mod_CNP_Nutcline_Nutlim_pred, 
  #                             se.fit=TRUE, 
  #                             type="response"))
  mod_CNP_Nutcline_Nutlim_pred <- pred_CNP_Nutcline_nutlim(mod_CNP_Nutcline_Nutlim_modGS, mod_CNP_Nutcline_Nutlim_pred)
  mod_CNP_Nutcline_Nutlim_pred  
}

####################################################
# FUNCTIONS RELATED TO ANALYSES of CESM2-LENS OUTPUT
####################################################
# Function to read CESM SST Output
read_sst_cesm <- function(cesm_filepath, filename) {
  sst_cesm <- nc_open(file.path(cesm_filepath, paste(filename,  sep="")))
  sst_surf <- ncvar_get(sst_cesm, varid = "TEMP")
  return(sst_surf)
}

# Function to read CESM Nitrate Output and set threshold minimum (default = 0.1 uM)
read_nitrate_cesm <- function(cesm_filepath, filename, threshold = 0.1) {
  nitrate_cesm <- nc_open(file.path(cesm_filepath, paste(filename,  sep="")))
  nitrate_surf <- ncvar_get(nitrate_cesm, varid = "NO3")
  nitrate_surf <- ifelse(nitrate_surf < threshold, threshold, nitrate_surf)
  return(nitrate_surf)
}

# Function to read CESM Nutricline and multoply by correction factor (default = 1.54)
read_nutcline_cesm <- function(cesm_filepath, filename, nutcline_correction = 1.54) {
  nutcline_cesm <- nc_open(file.path(cesm_filepath, paste(filename,  sep="")))
  nutcline <- ncvar_get(nutcline_cesm, varid = "Nutcline")
  nutcline_corrected <- nutcline*nutcline_correction
  return(nutcline_corrected)
}

# Function to read CESM SP Nutrient Limitation
read_nutlim_cesm <- function(cesm_filepath, filename, varid) {
  nutlim_cesm <- nc_open(file.path(cesm_filepath, paste(filename,  sep="")))
  nutlim <- ncvar_get(nutlim_cesm, varid = varid)
  return(nutlim)
}

# Function to extract longitude-latitude info for CESM output and make masks
get_cesm_lonlat <- function(cesm_filepath, filename = 'TEMP_regrid_historic.nc') {
  sst_cesm <- nc_open(file.path(cesm_filepath, paste(filename,  sep="")))
  lon <- ncvar_get(sst_cesm, varid = "lon")
  lat <- ncvar_get(sst_cesm, varid = "lat")
  lat_mask <- which(abs(lat) > 55)
  lonlat_tropical_mask <- outer(rep(1,length(lon)), ifelse(abs(lat) < 15, 1, 0))
  lonlat_Subtropical_mask <- outer(rep(1,length(lon)), ifelse(abs(lat) >= 15 & abs(lat) < 45, 1, 0))
  lonlat_Subpolar_mask <- outer(rep(1,length(lon)), ifelse(abs(lat) >= 45 & abs(lat) < 65, 1, 0))
  lonlat_polar_mask <- outer(rep(1,length(lon)),  ifelse(abs(lat) >= 65, 1, 0))
  lonlat_vector_mask <- lonlat_tropical_mask*1 + lonlat_Subtropical_mask*2 + 
  lonlat_Subpolar_mask*3 + lonlat_polar_mask*4
  lonlat_regions <- lonlat_vector_mask
  lonlat_regions[lonlat_regions == 1] <- "Tropical"
  lonlat_regions[lonlat_regions == 2] <- "Subtropical"
  lonlat_regions[lonlat_regions == 3] <- "Subpolar"
  lonlat_regions[lonlat_regions == 4] <- "Polar"
  lonlat_large_regions <- lonlat_regions
  lonlat_large_regions[lonlat_large_regions == "Subpolar" | lonlat_large_regions == "Polar"] <- "Highlat"
  lonlat_large_regions[lonlat_large_regions == "Tropical" | lonlat_large_regions == "Subtropical"] <- "Midlowlat"
  lonlat_grid <- outer(lon, lat, FUN = "paste")
  lonlat_grid_lat <- outer(rep(1,length(lon)), lat)
  lonlat_grid_lon <- outer(lon, rep(1, length(lat)))
  area_weights = cos(deg2rad(lonlat_grid_lat))
  
  cesm_lonlat_data <- list("lon"= lon, 
                           "lat" = lat, 
                           "lat_mask" = lat_mask,
                           "lonlat_regions" = lonlat_regions,
                           "lonlat_large_regions" = lonlat_large_regions,
                           "lonlat_grid" = lonlat_grid,
                           "lonlat_grid_lat" = lonlat_grid_lat,
                           "lonlat_grid_lon" = lonlat_grid_lon,
                           "area_weights" = area_weights)
  return(cesm_lonlat_data)
  
}

# Function to rename Nutrient limitation (from number to characters) for later analyses
rename_nutlim_cesm <- function(nutlim_cesm) {
  nutlim_renamed <- nutlim_cesm
  nutlim_renamed[nutlim_renamed == 1] <- "P-lim"
  nutlim_renamed[nutlim_renamed == 2] <- "PN-colim"
  nutlim_renamed[nutlim_renamed == 3] <- "N-lim"
  nutlim_renamed[nutlim_renamed == 4] <- "Fe-lim"
  return(nutlim_renamed)
}

# Function to create cesm variable array for historic and for future
make_cesm_var_array_for_gam <- function(sst_surf_historic,
                                        sst_surf_SSP370,
                                        nitrate_surf_historic,
                                        nitrate_surf_SSP370,
                                        nutcline_historic,
                                        nutcline_SSP370,
                                        nutlim_historic,
                                        nutlim_SSP370) {
  
  # Nutrient Limitation in character from number
  nutlim_historic <- rename_nutlim_cesm(nutlim_historic)
  nutlim_SSP370 <- rename_nutlim_cesm(nutlim_SSP370)
  
  # Make dataset tables for C:N:P prediction
  ## "_full" = change in SST, NO3, Nutricline and Nutrient limitation
  ## "_Tonly" = change in SST only from the historic
  ## "_Nonly" = change in NO3 only from the historic
  ## "_Nutlimonly" = change in Nutrient limitation only from the historic
  
  newd_historic_full = data.frame(SST = as.vector(sst_surf_historic),
                  Nutcline_1uM_interp = as.vector(nutcline_historic),
                  logNO3 = as.vector(log(nitrate_surf_historic)),
                  Nutlim = factor(as.vector(nutlim_historic), levels = c("P-lim","PN-colim", "N-lim","Fe-lim"))
                  )
  newd_SSP370_full = data.frame(SST = as.vector(sst_surf_SSP370),
                  Nutcline_1uM_interp = as.vector(nutcline_SSP370),
                  logNO3 = as.vector(log(nitrate_surf_SSP370)),
                  Nutlim = factor(as.vector(nutlim_SSP370), 
                                  levels = c("P-lim","PN-colim", "N-lim","Fe-lim")))
  newd_SSP370_Tonly = data.frame(SST = as.vector(sst_surf_SSP370),
                  Nutcline_1uM_interp = as.vector(nutcline_historic),        
                  logNO3 = as.vector(log(nitrate_surf_historic)),
                  Nutlim = factor(as.vector(nutlim_historic), 
                                  levels = c("P-lim","PN-colim", "N-lim","Fe-lim")))
  newd_SSP370_Nonly = data.frame(SST = as.vector(sst_surf_historic), 
                  Nutcline_1uM_interp = as.vector(nutcline_historic),                  
                  logNO3 = as.vector(log(nitrate_surf_SSP370)),
                  Nutlim = factor(as.vector(nutlim_historic), 
                                  levels = c("P-lim","PN-colim", "N-lim","Fe-lim")))
  newd_SSP370_Nutclineonly = data.frame(SST = as.vector(sst_surf_historic), 
                  Nutcline_1uM_interp = as.vector(nutcline_SSP370),                  
                  logNO3 = as.vector(log(nitrate_surf_historic)),
                  Nutlim = factor(as.vector(nutlim_historic), 
                                  levels = c("P-lim","PN-colim", "N-lim","Fe-lim")))
  newd_SSP370_Nutlimonly = data.frame(SST = as.vector(sst_surf_historic),
                  Nutcline_1uM_interp = as.vector(nutcline_historic), 
                  logNO3 = as.vector(log(nitrate_surf_historic)),
                  Nutlim = factor(as.vector(nutlim_SSP370), 
                                  levels = c("P-lim","PN-colim", "N-lim","Fe-lim")))
  
  cesm_var_array_for_gam <- list("newd_historic_full" = newd_historic_full,
                                 "newd_SSP370_full" = newd_SSP370_full,
                                 "newd_SSP370_Tonly" = newd_SSP370_Tonly,
                                 "newd_SSP370_Nonly" = newd_SSP370_Nonly,
                                 "newd_SSP370_Nutclineonly" = newd_SSP370_Nutclineonly,
                                 "newd_SSP370_Nutlimonly" = newd_SSP370_Nutlimonly
                                   )
  return(cesm_var_array_for_gam)
}

# Function to predict CNP with GAM and CESM variables
predict_cnp_gam_cesm <- function(mod_CP, mod_NP, cesm_var_array_for_gam) {
  newd_historic_full <- cesm_var_array_for_gam$newd_historic_full
  newd_SSP370_full <- cesm_var_array_for_gam$newd_SSP370_full
  newd_SSP370_Tonly <- cesm_var_array_for_gam$newd_SSP370_Tonly
  newd_SSP370_Nonly <- cesm_var_array_for_gam$newd_SSP370_Nonly
  newd_SSP370_Nutlimonly <- cesm_var_array_for_gam$newd_SSP370_Nutlimonly
  newd_SSP370_Nutclineonly <- cesm_var_array_for_gam$newd_SSP370_Nutclineonly
  # Predict C:P
  pred_cp_historic_full <- array(exp(predict(mod_CP, newdata = newd_historic_full)), c(360,180))
  pred_cp_SSP370_full <- array(exp(predict(mod_CP, newdata = newd_SSP370_full)), c(360,180))
  pred_cp_SSP370_Tonly <- array(exp(predict(mod_CP, newdata = newd_SSP370_Tonly)), c(360,180))
  pred_cp_SSP370_Nonly <- array(exp(predict(mod_CP, newdata = newd_SSP370_Nonly)), c(360,180))
  pred_cp_SSP370_Nutlimonly <- array(exp(predict(mod_CP, newdata = newd_SSP370_Nutlimonly)), c(360,180))
  pred_cp_SSP370_Nutclineonly <- array(exp(predict(mod_CP, newdata = newd_SSP370_Nutclineonly)), c(360,180))
  # Predict N:P
  pred_np_historic_full <- array(exp(predict(mod_NP, newdata = newd_historic_full)), c(360,180))
  pred_np_SSP370_full <- array(exp(predict(mod_NP, newdata = newd_SSP370_full)), c(360,180))
  pred_np_SSP370_Tonly <- array(exp(predict(mod_NP, newdata = newd_SSP370_Tonly)), c(360,180))
  pred_np_SSP370_Nonly <- array(exp(predict(mod_NP, newdata = newd_SSP370_Nonly)), c(360,180))
  pred_np_SSP370_Nutlimonly <- array(exp(predict(mod_NP, newdata = newd_SSP370_Nutlimonly)), c(360,180))
  pred_np_SSP370_Nutclineonly <- array(exp(predict(mod_NP, newdata = newd_SSP370_Nutclineonly)), c(360,180))

  pred_cnp_gam <- list("pred_cp_historic_full" = pred_cp_historic_full,
                       "pred_cp_SSP370_full" = pred_cp_SSP370_full,
                       "pred_cp_SSP370_Tonly" = pred_cp_SSP370_Tonly,
                       "pred_cp_SSP370_Nonly" =  pred_cp_SSP370_Nonly,
                       "pred_cp_SSP370_Nutlimonly" =  pred_cp_SSP370_Nutlimonly,
                       "pred_cp_SSP370_Nutclineonly" = pred_cp_SSP370_Nutclineonly,
                       "pred_np_historic_full" = pred_np_historic_full,
                       "pred_np_SSP370_full" = pred_np_SSP370_full,
                       "pred_np_SSP370_Tonly" = pred_np_SSP370_Tonly,
                       "pred_np_SSP370_Nonly" = pred_np_SSP370_Nonly,
                       "pred_np_SSP370_Nutlimonly" = pred_np_SSP370_Nutlimonly,
                       "pred_np_SSP370_Nutclineonly" = pred_np_SSP370_Nutclineonly
                       )
  return(pred_cnp_gam)
}  

# Function to take m models from posterior distribution and n models with replacement
sim_random_posterior <- function(model,m = 1000,n = 2000, data) {
  sim_model <- suppressWarnings(exp(simulate(model,
                                     nsim = m,
                                    seed = 101,
                                         newdata = dplyr::select(data,c('SST','Nutcline_1uM_interp','logNO3','Nutlim')))))
   # Selecting 2000 models with replacement for each grid point
   sim_model<- sim_model[, sample(ncol(sim_model), n, replace = TRUE)]
}

# Function to count how many of the n models build from posterior distribution of GAM predict positive change
calc_diff_cnp_PosCount_grid <- function(mod_CNP, m, n, cesm_var_array_for_gam) {

  newd_historic_full <- cesm_var_array_for_gam$newd_historic_full
  newd_SSP370_full <- cesm_var_array_for_gam$newd_SSP370_full

  sim_cnp_full_historic <- sim_random_posterior(mod_CNP, m, n, newd_historic_full)
  sim_cnp_full_SSP370 <- sim_random_posterior(mod_CNP, m, n, newd_SSP370_full)

  diffcnp_full <- sim_cnp_full_SSP370 - sim_cnp_full_historic
  diffcnp_full_Pos_Count <- rowSums(diffcnp_full[,] > 0)
  diffcnp_full_PosCount_grid <- array(diffcnp_full_Pos_Count, dim = c(360, 180))

  return(diffcnp_full_PosCount_grid)
}

# Function to calculate regional means from CNP with CESM2-lens GAM
cnp_regional <- function(cnp_data, region_grid, area_weights){
  data_vec <- reshape2::melt(cnp_data)
  data_vec$regions <- as.vector(region_grid)
  data_vec$area_weights <- as.vector(area_weights)
  data_vec_regional <- data_vec %>% group_by(regions) %>%
  summarise(mean = exp(weighted.mean(log(value), area_weights, na.rm = TRUE)))
}

####################################################
# FUNCTIONS RELATED TO ANALYSES of GLODAP OUTPUT
####################################################
# Function to read GLODAP Nutricline
read_nutcline_glodap <- function(glodap_filepath, filename) {
  nutcline_glodap<- nc_open(file.path(glodap_filepath, paste(filename,  sep="")))
  nutcline <- ncvar_get(nutcline_glodap, varid = "Nutcline")
  return(nutcline)
}

# Function to read GLODAP lon, lat, surf depth info
get_glodap_lonlat_surfdepth <- function(glodap_filepath, 
                                        filename_temp = 'GLODAPv2.2016b.temperature_regrid.nc',
                                        filename_nutcline = 'GLODAPv2.2016b.nutcline_regrid.nc',
                                        surfdepth = 30.0) {
  temp_glodap <- nc_open(file.path(glodap_filepath, paste(filename_temp,  sep="")))
  nutcline_glodap <- nc_open(file.path(glodap_filepath, paste(filename_nutcline,  sep="")))
  lon <- ncvar_get(nutcline_glodap, varid = "lon")
  lat <- ncvar_get(nutcline_glodap, varid = "lat") 
  depth <- ncvar_get(temp_glodap, varid = "DEPTH")
  surf_depth_index <- which.min(abs(depth - surfdepth))
  
  glodap_lonlat_surfdepth <- list("lon"= lon, 
                                   "lat" = lat, 
                                   "surf_depth_index" = surf_depth_index
                                   )
  return(glodap_lonlat_surfdepth)
}


