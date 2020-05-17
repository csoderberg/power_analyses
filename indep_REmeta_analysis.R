#loading libraries
library(MASS)
library(tidyverse)
library(metafor)
library(truncnorm)

# function to generate population correlations assuming RE model
gen_corr_distribution <- function(corrs, sd_corrs, n_dvs, n_per_dv){
  
  correlations <- c()  
  
    for (i in 1:length(corrs)) {
      measure_correlation <- c(rtruncnorm(n = n_dvs, mean = corrs[i], sd = sd_corrs, a = -1, b = 1), i)
      correlations <- rbind(correlations, measure_correlation)
    }
  
  sample_pop_corrs <- as_tibble(correlations) %>%
                    rename(conserv_measure = n_dvs + 1) %>%
                    pivot_longer(cols = 1:n_dvs, names_to = 'dv', values_to = 'dv_corr') %>%
                    mutate(n_per_dv = n_per_dv)
  
  return(sample_pop_corrs)
}

# function to generate sample correlations from RE population correlation
gen_corr_data <- function(dv_corr, n_per_dv) {
  data <- mvrnorm(n = n_per_dv, mu = c(0, 0), Sigma = matrix(c(1, dv_corr, dv_corr, 1), nrow = 2))
  sample_corr <- cor(data[,1], data[,2])
  return(sample_corr)
}

# function to generate simulation inputs
gen_sim_inputs <- function(all_corrs, all_corr_sds, n_per_dv, n_dvs, n_sims, pairwise_comps) {
  sim_inputs <- crossing(true_pop_corrs = all_corrs,
                         pop_corr_sds = all_corr_sds,
                         n_per_dv = n_per_dv,
                         n_dvs = n_dvs, 
                         n_simulations = c(1:n_sims), 
                         pairwise_comps = pairwise_comps)
  return(sim_inputs)
}

# function to run indepedent meta-analyses for specified pairwise_comps
indep_ma_results <- function(corr_data, pairwise_comps) {
  data <- escalc(measure = 'COR', ri = corr_data$sample_corr, ni = corr_data$n_per_dv, data = corr_data) %>%
            filter(corr_data$conserv_measure == pairwise_comps[[1]] | corr_data$conserv_measure == pairwise_comps[[2]])
  
  res1 <- rma(yi, vi, data=data, subset=conserv_measure==pairwise_comps[[1]])
  res2 <- rma(yi, vi, data=data, subset=conserv_measure==pairwise_comps[[2]])
  
  data_comp <- data.frame(estimate = c(coef(res1), coef(res2)), stderror = c(res1$se, res2$se),
                         meta = c(pairwise_comps[[1]],pairwise_comps[[2]]), tau2 = round(c(res1$tau2, res2$tau2),3))
  
  model <- rma(estimate, sei=stderror, mods = ~ meta, method="FE", data=data_comp, digits=3)
  
  return(set_names(c(model$QM, model$zval[2], model$pval[2]), c('qm_val', 'z_val', 'p_val')))
  
}

# function to run simulations
sim_function <- function(all_corrs, all_corr_sds, n_per_dv, n_dvs, n_sims, pairwise_comps) {
  
  #generate starting dataframe
  simulation_df <- gen_sim_inputs(all_corrs, all_corr_sds, n_per_dv, n_dvs, n_sims, pairwise_comps) %>%
                      mutate(dif_corrs = max(unlist(true_pop_corrs)) - min(unlist(true_pop_corrs)))
  
  #for each line, generate population correlations
  simulation_df <- simulation_df %>%
                    mutate(sim_pop_corrs = pmap(list(true_pop_corrs, pop_corr_sds, n_dvs, n_per_dv), gen_corr_distribution))
  
  # for each pop. correlation, generate data and calculate correlation in sample
  simulation_df <- simulation_df %>%
                     mutate(sim_pop_corrs = map(sim_pop_corrs, 
                                                ~ mutate(.x, 
                                                        sample_corr = pmap_dbl(list(dv_corr, n_per_dv), gen_corr_data))))
  
  # for each set of samples, run the indep MA and return results
  simulation_df <- simulation_df %>%
                      mutate(ma_result = pmap(list(sim_pop_corrs, pairwise_comps), indep_ma_results)) %>%
                      unnest_wider(ma_result)
  
  sim_results <- simulation_df %>%
                    group_by(dif_corrs, pop_corr_sds, n_per_dv, n_dvs) %>%
                    summarize(power = sum(p_val <= .05)/n_sims)
  
  return(sim_results)
}

