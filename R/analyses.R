###################
# GENERAL FUNCTIONS
###################

# Custom function to calculate mode
getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
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

# Function for conducting Tukey HSD test on Nutrient limitation as ac categorial variable with full gam model
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
  data_binned <- data %>% 
  mutate(binlon = cut(Longitude, seq(from = -180.0, to = 180.0, by = 1.0), include.lowest = T, right = F), binlat = cut(Latitude, seq(from = -90.0, to = 90.0, by = 1.0), include.lowest = T, right = F)
         )  %>% group_by(binlat, binlon) %>%  summarise(POCavg = mean(POCavg),
                                                        PONavg = mean(PONavg), 
            POPavg = mean(POPavg),
            logCP = mean(logCP), logNP = mean(logNP), logCN = mean(logCN),
            absLatitude = mean(absLatitude), 
            SST = mean(SST), 
            logPO4_fill = mean(logPO4_fill),
            logNO3_fill = mean(logNO3_fill),
            Nutcline_GLODAP_1um = mean(Nutcline_GLODAP_1um),
            Nstar_200_GLODAP = mean(Nstar_200_GLODAP),
            MLD_Holte17 = mean(MLD_Holte17),
            MLPAR = mean(MLPAR),
            CHLOR_a_MODIS = mean(CHLOR_a_MODIS),
            SPP_Rich_Righetti19 = mean(SPP_Rich_Righetti19),
            frac_dia_NOBM = mean(frac_dia_NOBM),
            frac_cya_NOBM = mean(frac_cya_NOBM),
            region =  ifelse(absLatitude < 15, "Tropical",
                             ifelse(absLatitude >= 15 & absLatitude < 45, "Subtropical",
                                    ifelse(absLatitude >= 45 & absLatitude < 65, "Subpolar",
                                           "Polar"))),            
            # area_weights = cos(deg2rad(Latitude)),
            sp_Nutlim_CESM2 = getmode(sp_Nutlim_CESM2)
            )  %>% replace_with_na_all(condition = ~.x == -Inf) %>% mutate(Latitude = lat_grid[as.integer(binlat)],Longitude = lon_grid[as.integer(binlon)])
data_binned$region <- factor(data_binned$region,
                                      levels = c("Polar", "Subpolar", "Subtropical","Tropical"))
data_binned$area_weights = cos(deg2rad(data_binned$Latitude))
data_binned
}
# Function to prepare dataset for GAM analyses
clean_data_for_gam <- function(data) {
  data_for_gam <- data %>% dplyr::select(logCP, logNP, logCN,
                                          SST, 
                                          logNO3_fill,
                                          logPO4_fill,
                                          Nutcline_GLODAP_1um,
                                          Nstar_200_GLODAP,
                                          MLPAR,
                                         sp_Nutlim_CESM2,
                                         absLatitude,
					 contains("Nutlim")
                                          ) %>% drop_na(logCP, logNP, logCN) %>% replace_with_na_all(condition = ~.x == -Inf) %>% tidyr::drop_na(SST, MLPAR, logNO3_fill, logPO4_fill, Nstar_200_GLODAP, Nutcline_GLODAP_1um) 
  data_for_gam
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
  POM_all_corr <- POM_all %>% dplyr::select(logCP, logNP, logCN,
                                          absLatitude,
                                          SST,
                                          logNO3_fill,
                                          logPO4_fill,
                                          logFeT,
                                          Nutcline_GLODAP_1um,
                                          # Nstar_200_GLODAP,
                                          MLD_Holte17,
                                          MLPAR,
                                          CHLOR_a_MODIS,
                                          frac_dia_NOBM,
                                          frac_cya_NOBM,
                                          sp_Nutlim_CESM2
                                          ) %>% drop_na(logCP, logNP, logCN) %>% replace_with_na_all(condition = ~.x == -Inf) %>% drop_na(SST, MLPAR, logNO3_fill, logPO4_fill,sp_Nutlim_CESM2)
  scaled.POM_all_corr <- data.frame(scale(POM_all_corr[,1:ncol(POM_all_corr)-1]))
}

# Function to get correlation matrix with column and rownames
M.POM_corr <- function(scaled.POM_all_corr, highlat = FALSE, colnames = FALSE) {
  if (highlat) {
  # For nutricline manually assign value of 0
  scaled.POM_all_corr$Nutcline_GLODAP_1um <- rep(0,length(scaled.POM_all_corr$Nutcline_GLODAP_1um))
  }
  M.scaled.POM_corr <- cor(scaled.POM_all_corr,
                                  method = c("pearson"), 
                                  use = "pairwise.complete.obs")
  M.POM_corr_selected <- M.scaled.POM_corr[1:3,4:ncol(M.scaled.POM_corr)] 
  if (colnames) {
  colnames(M.POM_corr_selected) <- paste(c("Abs.Latitude",
                                           "SST","Nitrate","Phosphate", "FeT", 
                                           "Nutricline", 
                                           "MLD", "MLPAR", "Chl-a",
                                           "% Diatoms", "% Cyano"
                                           ))
  } else {
  colnames(M.POM_corr_selected) <- paste(c("",
                                           "","","","",
                                           "", 
                                           "", "", "",
                                           "", ""
                                           ))    
  }
  rownames(M.POM_corr_selected) <- paste(c("C:P", "N:P", "C:N"))
  M.POM_corr_selected
}

# Function to get pvalue for correlation matrix
testRes.POM_corr <- function(scaled.POM_all_corr, highlat = FALSE) {
  if (highlat) {
  # For nutricline manually assign value of 0
  scaled.POM_all_corr$Nutcline_GLODAP_1um <- rep(0,length(scaled.POM_all_corr$Nutcline_GLODAP_1um))
  } 
  M.scaled.POM_corr <- cor(scaled.POM_all_corr,
                                  method = c("pearson"), 
                                  use = "pairwise.complete.obs")
  M.POM_corr_selected <- M.scaled.POM_corr[1:3,4:ncol(M.scaled.POM_corr)] 
  testRes = cor.mtest(scaled.POM_all_corr,method = c("pearson"), use = "pairwise.complete.obs", conf.level = 0.95)
  testRes_selected = testRes$p[1:3,4:ncol(M.scaled.POM_corr)]
  testRes_selected
}











