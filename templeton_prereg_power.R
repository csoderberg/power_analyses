library(tidyverse)
# set up parameters

random_stimuli <- function(n_stimuli, effect_treatment, effect_sd, n_participants, n_sims) {
  cond <- c(rep('a', n_participants/2), rep('b', n_participants/2))
  subj <- c(1:n_participants)
  p_value <- c()
  f_value <- c()
  t_value <- c()
  
  ### simulations where stimuli are drawn at random from a population ever time, and then those means used for random data draws (e.g. double random draw)
  for (y in 1:n_sims) {
    print(y)
    
    y1 <- c()
    y2 <- c()
    y1_stimuli <- c()
    y2_stimuli <- c()
    
    mean_control <- rnorm(n = n_stimuli, mean = 0, sd = effect_sd)
    mean_cond <- rnorm(n = n_stimuli, mean = effect_treatment, sd = effect_sd)
    
    for (i in 1:n_stimuli) {
      y1 <- c(y1, rnorm(n = n_participants/(2*n_stimuli), mean = mean_control[i], sd = 1))
      y2 <- c(y2, rnorm(n = n_participants/(2*n_stimuli), mean = mean_cond[i], sd = 1))
      y1_stimuli <- c(y1_stimuli, c(rep(i, n_participants/(2*n_stimuli))))
      y2_stimuli <- c(y2_stimuli, c(rep(i, n_participants/(2*n_stimuli))))
    }
    
    data <- as_tibble(cbind(subj, c(y1_stimuli, y2_stimuli), cond, c(y1, y2))) %>%
      mutate(V2 = as.factor(V2),
             cond = as.factor(cond),
             V4 = as.numeric(V4)) %>%
      rename(stimuli = V2, response = V4)
    
    if (n_stimuli > 1) {
      model <- aov(response ~ stimuli + cond, data)
      
      p_value <- c(p_value, summary(model)[[1]][2, 5])
      f_value <- c(f_value, summary(model)[[1]][2, 4])
    
    } else {
      model <- t.test(response ~ cond, data)
      
      p_value <- c(p_value, model$p.value)
      t_value <- c(t_value, model$statistic)
    }

  }
  
  power = sum(p_value <= .05)/n_sims
  
  if (n_stimuli < 2){
    cohen_d = (2*t_value)/sqrt(n_participants)
  } else {
    cohen_d = (2*sqrt(f_value))/sqrt(n_participants)
  }
  
  return(set_names(c(power, mean(cohen_d)), c('power', 'mean_d')))
}

rnorm2 <- function(n,mean,sd) { 
            mean+sd*scale(rnorm(n)) 
          }

fixed_stimuli <- function(n_stimuli, effect_treatment, effect_sd, n_participants, n_sims) {
  
  cond <- c(rep('a', n_participants/2), rep('b', n_participants/2))
  subj <- c(1:n_participants)
  p_value <- c()
  f_value <- c()
  t_value <- c()
  
  ### simulations where stimuli are drawn at random from a population ever time, and then those means used for random data draws (e.g. double random draw)
  for (y in 1:n_sims) {
    print(y)
    
    y1 <- c()
    y2 <- c()
    y1_stimuli <- c()
    y2_stimuli <- c()
    
    if (n_stimuli > 1) {
      mean_control <- c(rnorm2(n_stimuli, 0, effect_sd))
      mean_cond <- c(rnorm2(n_stimuli, 0 + effect_treatment, effect_sd))
    } else {
      mean_control <- 0
      mean_cond <- mean_control + effect_treatment
    }
    
    
    for (i in 1:n_stimuli) {
      y1 <- c(y1, rnorm(n = n_participants/(2*n_stimuli), mean = mean_control[i], sd = 1))
      y2 <- c(y2, rnorm(n = n_participants/(2*n_stimuli), mean = mean_cond[i], sd = 1))
      y1_stimuli <- c(y1_stimuli, c(rep(i, n_participants/(2*n_stimuli))))
      y2_stimuli <- c(y2_stimuli, c(rep(i, n_participants/(2*n_stimuli))))
    }
    
    data <- as_tibble(cbind(subj, c(y1_stimuli, y2_stimuli), cond, c(y1, y2))) %>%
      mutate(V2 = as.factor(V2),
             cond = as.factor(cond),
             V4 = as.numeric(V4)) %>%
      rename(stimuli = V2, response = V4)
    
    if (n_stimuli > 1) {
      model <- aov(response ~ stimuli + cond, data)
      
      p_value <- c(p_value, summary(model)[[1]][2, 5])
      f_value <- c(f_value, summary(model)[[1]][2, 4])
      
    } else {
      model <- t.test(response ~ cond, data)
      
      p_value <- c(p_value, model$p.value)
      t_value <- c(t_value, model$statistic)
    }
    
  }
  
  power = sum(p_value <= .05)/n_sims
  
  if (n_stimuli < 2){
    cohen_d = (2*t_value)/sqrt(n_participants)
  } else {
    cohen_d = (2*sqrt(f_value))/sqrt(n_participants)
  }
  
  
  return(set_names(c(power, mean(cohen_d)), c('power', 'mean_d')))
}

stimuli_as_random <- crossing(n_stimuli = c(1, 5, 10),
         effect_treatment = c(0, .25),
         effect_sd = c(.05, .1, .15, .2, .25),
         n_participants = 600,
         n_sims = 5000) %>%
  mutate(sim_results = pmap(list(n_stimuli, effect_treatment, effect_sd, n_participants, n_sims), random_stimuli)) %>%
  unnest_wider(sim_results)

stimuli_as_fixed <- crossing(n_stimuli = c(1, 5, 10),
                             effect_treatment = c(0, .25),
                             effect_sd = c(.05, .1, .15, .2, .25),
                             n_participants = 600,
                             n_sims = 5000) %>%
                      mutate(sim_results = pmap(list(n_stimuli, effect_treatment, effect_sd, n_participants, n_sims), fixed_stimuli)) %>%
                      unnest_wider(sim_results)

top_level <- function(n_sims, effect_treatment, effect_sd, n_stimuli) {
  
  mean_control <- c()
  mean_cond <- c()
    
  for (y in 1:n_sims) {
    print(y)
    
    y1 <- c()
    y2 <- c()
    y1_stimuli <- c()
    y2_stimuli <- c()
    
    control_means <- rnorm(n = n_stimuli, mean = 0, sd = effect_sd)
    cond_means <- rnorm(n = n_stimuli, mean = effect_treatment, sd = effect_sd)
    
    mean_control <- c(mean_control, mean(control_means))
    mean_cond <- c(mean_cond, mean(cond_means))
  }
  return(set_names(c(mean(mean_control), mean(mean_cond)), c('mean_control', 'mean_cond')))
}


stimuli_means <- crossing(n_stimuli = c(1, 5),
                         effect_treatment = c(0, .25),
                         effect_sd = c(.05, .1, .15, .2),
                         n_sims = 1000) %>%
                  mutate(sim_results = pmap(list(n_sims, effect_treatment, effect_sd, n_stimuli), top_level)) %>%
                  unnest_wider(sim_results)



lower_means <- function(n_stimuli, effect_treatment, effect_sd, n_participants, n_sims) {
  cond <- c(rep('a', n_participants/2), rep('b', n_participants/2))
  subj <- c(1:n_participants)
  control_mean <- c()
  cond_mean <- c()
  
  ### simulations where stimuli are drawn at random from a population ever time, and then those means used for random data draws (e.g. double random draw)
  for (y in 1:n_sims) {
    print(y)
    
    y1 <- c()
    y2 <- c()
    y1_stimuli <- c()
    y2_stimuli <- c()
    
    mean_control <- rnorm(n = n_stimuli, mean = 0, sd = effect_sd)
    mean_cond <- rnorm(n = n_stimuli, mean = effect_treatment, sd = effect_sd)
    
    for (i in 1:n_stimuli) {
      y1 <- c(y1, rnorm(n = n_participants/(2*n_stimuli), mean = mean_control[i], sd = 1))
      y2 <- c(y2, rnorm(n = n_participants/(2*n_stimuli), mean = mean_cond[i], sd = 1))
      y1_stimuli <- c(y1_stimuli, c(rep(i, n_participants/(2*n_stimuli))))
      y2_stimuli <- c(y2_stimuli, c(rep(i, n_participants/(2*n_stimuli))))
    }
    
    control_mean <- c(control_mean, mean(y1))
    cond_mean <- c(cond_mean, mean(y2))
    
  }
  
  return(set_names(c(mean(control_mean), mean(cond_mean)), c('control_mean', 'cond_mean')))
}

overall_means <- crossing(n_stimuli = c(1, 5),
                              effect_treatment = c(0, .25),
                              effect_sd = c(.05, .1, .15, .2),
                              n_participants = 600,
                              n_sims = 1000) %>%
  mutate(sim_results = pmap(list(n_stimuli, effect_treatment, effect_sd, n_participants, n_sims), lower_means)) %>%
  unnest_wider(sim_results)


all_results <- bind_rows(stimuli_as_fixed %>% mutate(data_gen = 'fixed_sample'), 
                         stimuli_as_random  %>% mutate(data_gen = 'random_sample')) %>%
               mutate(data_gen = as.factor(data_gen),
                      effect_treatment = as.factor(effect_treatment))
               

ggplot(all_results %>% filter(n_stimuli == 5 & effect_sd < .25), aes(x = effect_sd, y = power, color = effect_treatment)) +
         geom_line() +
         scale_y_continuous(name = 'percent p < .05', breaks = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9)) +
         scale_x_continuous(name = 'vignette_sd') +
         facet_grid(cols = vars(data_gen))

  