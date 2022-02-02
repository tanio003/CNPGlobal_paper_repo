######################
# AUXILLIARY FUNCTIONS
######################

export_csv <- function(object, tab_out_folder, out_file, verbose = TRUE) {
  out_path <- file.path(tab_out_folder, out_file)
  if (verbose) {
    message("Saving ", out_path, " to file")
  }
  write.csv(object, out_path, row.names = TRUE)
  object
}

# Function to make summary plot from a box plot
region_cnp_summmary <- function(x) {
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
