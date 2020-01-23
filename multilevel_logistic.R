library(tidyverse)
library(lme4)
library(sjPlot)
library(truncnorm)


glmm_power_sim <- function(J, a_per, nsims, mean1, mean2, sd1, sd2) {
  
  p_values <- cbind(rep(NA, nsims), rep(NA, nsims), rep(NA, nsims))
  estimates <- cbind(rep(NA, nsims), rep(NA, nsims), rep(NA, nsims),rep(NA, nsims))
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
    
    estimates[i, 1] <- summary(re_int_model)$coefficients[1,1]
    estimates[i, 2] <- summary(re_int_model)$coefficients[2,1]
    estimates[i, 3] <- summary(re_slope_model)$coefficients[1,1]
    estimates[i, 4] <- summary(re_slope_model)$coefficients[2,1]
  }
  power_femodel <- sum(p_values[,1] <= .05)/nsims
  power_reint <- sum(p_values[,2] <= .05)/nsims
  power_reslope <- sum(p_values[,3] <= .05)/nsims
  
  return(cbind(p_values, singularity, estimates))
}

sim_results1 <- glmm_power_sim(15, 6, 500, .125, .5, .05, .1)
sim_results2 <- glmm_power_sim(30, 6, 500, .125, .5, .05, .1)
sim_results3 <- glmm_power_sim(45, 6, 500, .125, .5, .05, .1)
sim_results4 <- glmm_power_sim(15, 12, 500, .125, .5, .05, .1)
sim_results5 <- glmm_power_sim(30, 12, 500, .125, .5, .05, .1)
sim_results6 <- glmm_power_sim(45, 12, 500, .125, .5, .05, .1)
sim_results7 <- glmm_power_sim(15, 24, 500, .125, .5, .05, .1)
sim_results8 <- glmm_power_sim(30, 24, 500, .125, .5, .05, .1)
sim_results9 <- glmm_power_sim(45, 24, 500, .125, .5, .05, .1)
sim_results10 <- glmm_power_sim(15, 48, 500, .125, .5, .05, .1)
sim_results11 <- glmm_power_sim(30, 48, 500, .125, .5, .05, .1)
sim_results12 <- glmm_power_sim(45, 48, 500, .125, .5, .05, .1)

sim_results13 <- glmm_power_sim(15, 6, 500, .125, .4, .05, .1)
sim_results14 <- glmm_power_sim(30, 6, 500, .125, .4, .05, .1)
sim_results15 <- glmm_power_sim(45, 6, 500, .125, .4, .05, .1)
sim_results16 <- glmm_power_sim(15, 12, 500, .125, .4, .05, .1)
sim_results17 <- glmm_power_sim(30, 12, 500, .125, .4, .05, .1)
sim_results18 <- glmm_power_sim(45, 12, 500, .125, .4, .05, .1)
sim_results19 <- glmm_power_sim(15, 6, 500, .125, .3, .05, .1)
sim_results20 <- glmm_power_sim(30, 6, 500, .125, .3, .05, .1)
sim_results21 <- glmm_power_sim(45, 6, 500, .125, .3, .05, .1)
sim_results22 <- glmm_power_sim(15, 12, 500, .125, .3, .05, .1)
sim_results23 <- glmm_power_sim(30, 12, 500, .125, .3, .05, .1)
sim_results24 <- glmm_power_sim(45, 12, 500, .125, .3, .05, .1)

sim_results25 <- glmm_power_sim(15, 4, 500, .125, .5, .05, .1)
sim_results26 <- glmm_power_sim(30, 4, 500, .125, .5, .05, .1)
sim_results27 <- glmm_power_sim(45, 4, 500, .125, .5, .05, .1)
sim_results28 <- glmm_power_sim(15, 4, 500, .125, .4, .05, .1)
sim_results29 <- glmm_power_sim(30, 4, 500, .125, .4, .05, .1)
sim_results30 <- glmm_power_sim(45, 4, 500, .125, .4, .05, .1)


singularity_data <- as.data.frame(matrix(c(15, 4, sum(sim_results25[,4] == 1)/500, sum(sim_results25[,5] == 1)/500,
                                           30, 4, sum(sim_results26[,4] == 1)/500, sum(sim_results26[,5] == 1)/500,
                                           45, 4, sum(sim_results27[,4] == 1)/500, sum(sim_results27[,5] == 1)/500,
                           15, 6, sum(sim_results1[,4] == 1)/500, sum(sim_results1[,5] == 1)/500,
                           30, 6, sum(sim_results2[,4] == 1)/500, sum(sim_results2[,5] == 1)/500,
                           45, 6, sum(sim_results3[,4] == 1)/500, sum(sim_results3[,5] == 1)/500,
                           15, 12, sum(sim_results4[,4] == 1)/500, sum(sim_results4[,5] == 1)/500,
                           30, 12, sum(sim_results5[,4] == 1)/500, sum(sim_results5[,5] == 1)/500,
                           45, 12, sum(sim_results6[,4] == 1)/500, sum(sim_results6[,5] == 1)/500,
                           15, 24, sum(sim_results7[,4] == 1)/500, sum(sim_results7[,5] == 1)/500,
                           30, 24, sum(sim_results8[,4] == 1)/500, sum(sim_results8[,5] == 1)/500,
                           45, 24, sum(sim_results9[,4] == 1)/500, sum(sim_results9[,5] == 1)/500,
                           15, 48, sum(sim_results10[,4] == 1)/500, sum(sim_results10[,5] == 1)/500,
                           30, 48, sum(sim_results11[,4] == 1)/500, sum(sim_results11[,5] == 1)/500,
                           45, 48, sum(sim_results12[,4] == 1)/500, sum(sim_results12[,5] == 1)/500,
                           15, 48, sum(sim_results25[,4] == 1)/500, sum(sim_results25[,5] == 1)/500,
                           30, 48, sum(sim_results26[,4] == 1)/500, sum(sim_results26[,5] == 1)/500,
                           45, 48, sum(sim_results27[,4] == 1)/500, sum(sim_results27[,5] == 1)/500), nrow = 12, ncol = 4, byrow = T)) %>%
                    rename(Journals = V1, 
                           articles_per = V2, 
                           R_int_singular = V3,
                           R_slope_singular = V4) %>%
                   mutate(Journals = as.factor(Journals))

int_model <- ggplot(singularity_data, aes(x = articles_per, y = R_int_singular, color = Journals)) + 
                  geom_point() +        
                  geom_line() +
                  labs(x = 'Articles per Journal', y = 'Percent of Models with Singular Fit', title = 'Random Journal Intercept Model') +
                  theme(text = element_text(size = 20))

slope_model <- ggplot(singularity_data, aes(x = articles_per, y = R_slope_singular, color = Journals)) + 
                  geom_point() +
                  geom_line() +
                  labs(x = 'Articles per Journal', y = 'Percent of Models with Singular Fit', title = 'Random Slope & Intercept Model') +
                  theme(text = element_text(size = 20))




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
