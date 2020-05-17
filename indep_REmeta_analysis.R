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
  
  pop_corrs <- as_tibble(correlations) %>%
                    rename(conserv_measure = n_dvs + 1) %>%
                    pivot_longer(cols = 1:n_dvs, names_to = 'dv', values_to = 'dv_corr')
  
  return(pop_corrs)
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

# function to run indepedent meta-analyses for specified pairwise_comps
indep_ma_results <- function(corr_data, pairwise_comps) {
  data <- escalc(measure = 'COR', ri = sample_corr, ni = n_per_dv, data = corr_data) %>%
            filter(conserv_measure == pairwise_comps[1] | conserv_measure == pairwise_comps[2])
  
  res1 <- rma(yi, vi, data=data, subset=conserv_measure==pairwise_comps[1])
  res2 <- rma(yi, vi, data=data, subset=conserv_measure==pairwise_comps[2])
  
  data_comp <- data.frame(estimate = c(coef(res1), coef(res2)), stderror = c(res1$se, res2$se),
                         meta = c(pairwise_comps[1],pairwise_comps[2]), tau2 = round(c(res1$tau2, res2$tau2),3))
  
  model <- rma(estimate, sei=stderror, mods = ~ meta, method="FE", data=data_comp, digits=3)
  
  return(model$QM, model$zval[2], model$pval[2])
}

# function to run simulations
sim_function <- function(all_corrs, all_corr_sds, n_per_dv, n_dvs, n_sims, pairwise_comps) {
  
  #generate starting dataframe
  simulation_df <- gen_sim_inputs(all_corrs, all_corr_sds, n_per_dv, n_dvs, n_sims)
  
  #for each line, generate population correlations
  simulation_df <- simulation_df %>%
                    mutate(pop_corrs = pmap(list(all_corrs, all_corr_sds, n_dvs), gen_corr_distribution))
  
}

