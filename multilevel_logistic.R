library(tidyverse)
library(lme4)
library(sjPlot)


glmm_power_sim <- function(J, a_per, nsims, mean1, mean2, sd1, sd2) {
  
  p_values <- rep(NA, nsims)
  
  #### set up basic sample size parameters 
  a <- rep(a_per, J) ## number of articles per journal (starting with fixed number - assuming 10 eligible per journal & 60% opt-in rate)
  
  for (i in 1:nsims) {
    print(i)
    #### generate datastructure
    baselines <- rnorm(mean = mean1, sd = sd1, n = J)
    RRs <- rnorm(mean = mean2, sd = sd2, n = J)
    
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
    model_fit <- summary(glmer(resp ~ cond + (1|journal), family = binomial, data = journal_data))
    
    p_values[i] <- model_fit$coefficients[2,4]
  }
}








plot_model(glmer(resp ~ cond + (1|journal), family = binomial, data = journal_data), terms = 'cond', type = 're')
  