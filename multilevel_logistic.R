library(tidyverse)
library(lme4)
library(sjPlot)
library(truncnorm)


glmm_power_sim <- function(J, a_per, nsims, mean1, mean2, sd1, sd2) {
  
  p_values <- cbind(rep(NA, nsims), rep(NA, nsims), rep(NA, nsims))
  singularity <- cbind(rep(NA, nsims), rep(NA, nsims))
  
  #### set up basic sample size parameters 
  a <- rep(a_per, J) ## number of articles per journal (starting with fixed number - assuming 10 eligible per journal & 60% opt-in rate)
  
  for (i in 1:nsims) {
    print(i)
    #### generate datastructure
    baselines <- rtruncnorm(mean = mean1, sd = sd1, n = J, a = 0, b = 1)
    RRs <- rtruncnorm(mean = mean2, sd = sd2, n = J,  a = 0, b = 1)
    
    article_count <- 0
    journal_data <- NULL
    
    for (j in 1:J) {
      
      # get responses
      resp_control <- rbinom((a[j])/2, 1, baselines[j])
      resp_RR <- rbinom((a[j])/2, 1, RRs[j])
      
      resp <- c(resp_control, resp_RR)
      
      # get conditions
      cond_control <- rep(-1, (a[j])/2)
      cond_RR  <- rep(1, (a[j])/2)
      
      cond <- c(cond_control, cond_RR)
      
      journal <- rep(j, a[j])
      article <- seq(article_count + 1, article_count + a[j])
      
      article_count <- article_count + a[j]
      
      journal_data <- bind_rows(journal_data, as_tibble(cbind(resp, cond, journal, article)))
    }
    
    fe_model <- glm(resp ~ cond, family = binomial, data = journal_data)
    re_int_model <- glmer(resp ~ cond + (1|journal), family = binomial, data = journal_data, control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000))) 
    re_slope_model <- glmer(resp ~ cond + (cond|journal), family = binomial, data = journal_data, control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
    
    p_values[i, 1] <- summary(fe_model)$coefficients[2,4]
    p_values[i, 2] <- summary(re_int_model)$coefficients[2,4]
    p_values[i, 3] <- summary(re_slope_model)$coefficients[2,4]
    
    singularity[i, 1] <- isSingular(re_int_model)
    singularity[i, 2] <- isSingular(re_slope_model)
  }
  power_femodel <- sum(p_values[,1] <= .05)/nsims
  power_reint <- sum(p_values[,2] <= .05)/nsims
  power_reslope <- sum(p_values[,3] <= .05)/nsims
  
  return(cbind(p_values, singularity))
}

p_values <- glmm_power_sim(15, 8, 150, .125, .3, .05, .1)

sum(p_values[,1] <= .05)/nrow(p_values)
sum(p_values[,2] <= .05)/nrow(p_values)
sum(p_values[,3] <= .05)/nrow(p_values)





plot_model(glmer(resp ~ cond + (1|journal), family = binomial, data = journal_data), terms = 'cond', type = 're')
  

Bayes_Model_Multi_Intercept <- brm(resp ~ cond + (cond|journal),
                                   data = journal_data, 
                                   family = bernoulli(link = "logit"),
                                   warmup = 500, 
                                   iter = 2000, 
                                   chains = 2, 
                                   inits = "0", 
                                   cores = 2,
                                   seed = 123)
