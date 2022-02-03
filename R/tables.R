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
  odd_data <- transpose(odd_data)
  even_data <- data.table(transpose(even_data))
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
