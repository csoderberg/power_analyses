#loading libraries
library(MASS)
library(tidyverse)
library(metafor)

# function to generate population correlations assuming RE model
gen_corr_distribution <- function(corrs, sd_corrs, n_dvs){
  
  correlations <- c()  
  
    for (i in 1:length(corrs)) {
      measure_correlation <- c(rnorm(n = n_dvs, mean = corrs[i], sd = sd_corrs), i)
      correlations <- rbind(correlations, measure_correlation)
    }
  
  correlations <- as_tibble(correlations) %>%
                    rename(conserv_measure = n_dvs + 1) %>%
                    pivot_longer(cols = 1:n_dvs, names_to = 'dv', values_to = 'dv_corr')
  
  return(correlations)
}

# function to generate sample correlations from RE population correlation
gen_corr_data <- function(corrs, n_per_dv) {
  data <- mvrnorm(n = n_per_dv, mu = c(0, 0), Sigma = matrix(c(1, corrs, corrs, 1), nrow = 2))
  sample_corr <- cor(data[,1], data[,2])
  return(sample_corr)
}

# function to generate simulation inputs
gen_sim_inputs <- function(all_corrs, all_corr_sds, n_per_dv, n_dvs, n_sims) {
  sim_inputs <- crossing(pop_corrs = all_corrs,
                         pop_corr_sds = all_corr_sds,
                         n_per_dv = n_per_dv,
                         n_dvs = n_dvs, 
                         n_sims = n_sims)
  return(sim_inputs)
}
