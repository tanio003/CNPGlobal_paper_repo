######################
# AUXILLIARY FUNCTIONS
######################

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

