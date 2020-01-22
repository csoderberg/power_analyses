library(tidyverse)
library(lme4)

#### set up basic sample size parameters 
J <- 30  ## number of journals
a <- rep(30, J) ## number of articles per journal (starting with fixed number - assuming 10 eligible per journal & 60% opt-in rate)
do_rate <- 0 # dropout rate, 0 - 10% 

nsims <- 1
p_values <- rep(NA, nsims)
for (i in 1:nsims) {
  #### generate datastructure
  baselines <- rnorm(mean = .125, sd = .04, n = J)
  RRs <- rnorm(mean = .5, sd = .1, n = J)
  
  article_count <- 0
  journal_data <- NULL
  
  for (j in 1:J) {
    
    # get responses
    resp_control <- rbinom((a[j])/2, 1, baselines[j])
    resp_RR <- rbinom((a[j])/2, 1, RRs[j])
    
    resp <- c(resp_control, resp_RR)
    
    # get conditions
    cond_control <- rep(0, (a[j])/2)
    cond_RR  <- rep(1, (a[j])/2)
    
    cond <- c(cond_control, cond_RR)
    
    journal <- rep(j, a[j])
    article <- seq(article_count + 1, article_count + a[j])
    
    article_count <- article_count + a[j]
    
    journal_data <- bind_rows(journal_data, as_tibble(cbind(resp, cond, journal, article)))
  }
  model_fit <- summary(glmer(resp ~ cond + (cond|journal), family = binomial, data = journal_data))
  
  p_values[i] <- model_fit$coefficients[2,4]
}

