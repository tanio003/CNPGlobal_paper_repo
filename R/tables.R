######################
# AUXILLIARY FUNCTIONS
######################

export_csv <- function(object, tab_out_folder, out_file, verbose = TRUE, rownames = FALSE) {
  out_path <- file.path(tab_out_folder, out_file)
  if (verbose) {
    message("Saving ", out_path, " to file")
  }
  write.csv(object, out_path, row.names = rownames)
  object
}

# Function to make summary plot from a box plot
make_region_cnp_summary <- function(x) {
  region <- c("Polar", "Subpolar", "Subtropical","Tropical")
  mean <- round((ggplot_build(x)$data[[3]]$y), digits = 1)
  median <- round(rev(ggplot_build(x)$data[[1]]$middle),digits = 1)
  ci.lb <- round(rev(ggplot_build(x)$data[[1]]$ymin), digits = 1)
  ci.ub <- round(rev(ggplot_build(x)$data[[1]]$ymax), digits = 1)
  n <- x$data %>% group_by(region) %>% summarise(n = n()) %>% select(n)
  region_summary <- data.frame(region,
                               mean,
                               median,
                               ci.lb,
                               ci.ub,
                               n)
  region_summary
}

# Function to combine CNP region_summary and export to csv
make_region_cnp_summary_combined <- function(POM_all, ...) {
  cp_summary <- make_region_cnp_summary(cp_boxplot(POM_all))
  np_summary <- make_region_cnp_summary(np_boxplot(POM_all))
  cn_summary <- make_region_cnp_summary(cn_boxplot(POM_all))
  cnp_summary <- rbind(cp_summary,np_summary,cn_summary)
  cnp_summary <- insertRow(cnp_summary, c("C:P"), 1) %>% insertRow(c("N:P"),6) %>% insertRow(c("C:N"),11)
  cnp_summary %>% export_csv(...)
}

# Function to Merge Correlation Matrix and p-value matrix by alternating columns
merge_corr_pval <- function(odd_data, even_data, digits = 3) {
  odd_data <- round(odd_data, digits)
  neworder <- order(c(2*(seq_along(odd_data) - 1) + 1,
                    2*seq_along(even_data)))
  newtable <- cbind(odd_data, even_data)[,..neworder]
  colnames(newtable) <- paste(c("Abs.Latitude","pval",
                                           "SST","pval",
                                "Nitrate", "pval",
                                "Phosphate","pval", 
                                "FeT", "pval",
                                           "Nutricline", "pval",
                                           "MLD", "pval",
                                "MLPAR", "pval",
                                "Chl-a", "pval",
                                           "% Diatoms", "pval",
                                "% Cyano","pval"
                                           ))
  newtable <- as.data.frame(newtable) 
  rownames(newtable) <- paste(c("C:P", "N:P", "C:N"))
  newtable
}

# Function to make CNP global mean summary table
make_cnp_table <- function(CNP_global_mean, sgnf = 1, ...) {
  CP_summary <- cbind(CNP_global_mean$meancp_global,
                    CNP_global_mean$meancp_ci_lb,
                    CNP_global_mean$meancp_ci_ub,
                    CNP_global_mean$nsamples)
  NP_summary <- cbind(CNP_global_mean$meannp_global,
                    CNP_global_mean$meannp_ci_lb,
                    CNP_global_mean$meannp_ci_ub,
                    CNP_global_mean$nsamples)
  CN_summary <- cbind(CNP_global_mean$meancn_global,
                    CNP_global_mean$meancn_ci_lb,
                    CNP_global_mean$meancn_ci_ub,
                    CNP_global_mean$nsamples)
  CNP_summary <- as.data.frame(rbind(round(CP_summary,sgnf),
                                   round(NP_summary,sgnf),
                                   round(CN_summary,sgnf+1)))
  rownames(CNP_summary) <- c('C:P','N:P','C:N')
  colnames(CNP_summary) <- c('Mean','l-95% CI','u-95%CI', 'n')
  CNP_summary %>% export_csv(...)
}

# Function to export corr-pval matrix to csv file
make_corr_pval_table <- function(M.POM_corr_selected, testRes_selected, ...) {
  testRes_selected  <- ifelse(testRes_selected < 0.001, "***",
                        ifelse(testRes_selected < 0.01, "**",
                               ifelse(testRes_selected < 0.05, "*",
                                      "n.s.")))
  merge_corr_pval_table <- merge_corr_pval(data.table(M.POM_corr_selected),
                     data.table(testRes_selected)) %>% export_csv(...)
}

# Function to Merge Deviance explained and p-value matrix by alternating columns and export to csv
make_devexpl_pval_table <- function(odd_data, even_data, digits = 3, ...) {
  odd_data <- data.table(round(odd_data, digits))
  odd_data <- data.table::transpose(odd_data)
  even_data <- data.table(data.table::transpose(even_data))
  neworder <- order(c(2*(seq_along(odd_data) - 1) + 1,
                    2*seq_along(even_data)))
  newtable <- cbind(odd_data, even_data)[,..neworder]
  colnames(newtable) <- paste(c("SST","pval",
                                "Nitrate", "pval",
                                "Nutricline","pval",
                                "Nutricline x Nutlim", "pval",
                                           "Total", "pval"
                                           ))
  newtable <- as.data.frame(newtable)
  rownames(newtable) <- paste(c("C:P", "N:P", "C:N"))
  newtable %>% export_csv(...)
}

# Function to make area-weighted regional mean table from CESM-GAM outputs
make_cnp_gam_cesm_summary_table <- function(cesm_lonlat_info, CNP_gam_cesm, ...) {
  region <- c("Polar", "Subpolar", "Subtropical","Tropical")
  area_weights <- cesm_lonlat_info$area_weights
  lonlat_regions <- cesm_lonlat_info$lonlat_regions
  lonlat_large_regions <- cesm_lonlat_info$lonlat_large_regions
  pred_cp_historic_full <- CNP_gam_cesm$pred_cp_historic_full
  pred_np_historic_full <- CNP_gam_cesm$pred_np_historic_full
  pred_cp_SSP370_full <- CNP_gam_cesm$pred_cp_SSP370_full
  pred_np_SSP370_full <- CNP_gam_cesm$pred_np_SSP370_full
  
  # Global
  cp_global_historic <- weighted.mean(log(pred_cp_historic_full), area_weights, na.rm = TRUE)
  np_global_historic <- weighted.mean(log(pred_np_historic_full), area_weights, na.rm = TRUE)
  cp_global_SSP370 <- weighted.mean(log(pred_cp_SSP370_full), area_weights, na.rm = TRUE)
  np_global_SSP370 <- weighted.mean(log(pred_np_SSP370_full), area_weights, na.rm = TRUE)

  # Regional
  # C:P
  cp_historic_full_vec_regional <- cnp_regional(pred_cp_historic_full, lonlat_regions, area_weights) 
  cp_historic_full_vec_largeregional <- cnp_regional(pred_cp_historic_full, lonlat_large_regions, area_weights)
  cp_SSP370_full_vec_regional <- cnp_regional(pred_cp_SSP370_full, lonlat_regions, area_weights) 
  cp_SSP370_full_vec_largeregional <- cnp_regional(pred_cp_SSP370_full, lonlat_large_regions, area_weights)
  # N:P
  np_historic_full_vec_regional <- cnp_regional(pred_np_historic_full, lonlat_regions, area_weights) 
  np_historic_full_vec_largeregional <- cnp_regional(pred_np_historic_full, lonlat_large_regions, area_weights)
  np_SSP370_full_vec_regional <- cnp_regional(pred_np_SSP370_full, lonlat_regions, area_weights) 
  np_SSP370_full_vec_largeregional <- cnp_regional(pred_np_SSP370_full, lonlat_large_regions, area_weights)

  # Summarize in a table
  regions <- c(region,"High latitude (|Lat| >= 45)", "Low latitude (|Lat| < 45)", "Global")

  cp_hist <- c(cp_historic_full_vec_regional$mean, cp_historic_full_vec_largeregional$mean, exp(cp_global_historic))
  cp_SSP370 <- c(cp_SSP370_full_vec_regional$mean, cp_SSP370_full_vec_largeregional$mean, exp(cp_global_SSP370))
  np_hist <- c(np_historic_full_vec_regional$mean, np_historic_full_vec_largeregional$mean, exp(np_global_historic))
  np_SSP370 <- c(np_SSP370_full_vec_regional$mean, np_SSP370_full_vec_largeregional$mean, exp(np_global_SSP370))
  summary_cesm_pred_regional <- data.frame(regions, 
                                           cp_hist, 
                                           cp_SSP370, 
                                           np_hist,
                                          np_SSP370)
  colnames(summary_cesm_pred_regional) <- c('Region',
                                            'C:P Historical (2010s)',
                                            'C:P SSP370 (2090s)',
                                            'N:P Historical (2010s)',
                                            'N:P SSP 370 (2090s)')
  summary_cesm_pred_regional %>% mutate_if(is.numeric, round, digits = 1) %>% export_csv(...)
}

# Function to create summary table for cross validation of hierarchical GAM models
make_cnp_cv_summary_table <- function(data, cv_df, stoich, tabletitle, ...) {
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
  Summary_table <- AIC(
    modG,
    modGS,
    modGI,
    modS,
    modI,
    modC) %>%  
    rownames_to_column(var= "Model") %>% 
    mutate(deltaAIC = AIC - min(AIC)) %>%
    mutate(rmse = with(cv_df, tapply(rmse, model, mean)))
  Summary_table$R_squared<- c(
    summary(modG)$r.sq,
    summary(modGS)$r.sq,
    summary(modGI)$r.sq,
    summary(modS)$r.sq,
    summary(modI)$r.sq,
    summary(modC)$r.sq
  )
  Summary_table <- Summary_table %>% 
    mutate_if(is.numeric, round, digits = 3) %>% arrange(AIC) %>%
    gt() %>%
    tab_header(
      title = md(tabletitle)
    ) %>% export_csv(...)
}


















