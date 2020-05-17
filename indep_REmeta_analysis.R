#loading libraries
library(MASS)
library(tidyverse)
library(metafor)

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
