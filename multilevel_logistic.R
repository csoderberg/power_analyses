
#### set up basic sample size parameters 

J <- 15  ## number of journals
a <- 6 ## number of articles per journal (starting with fixed number - assuming 10 eligible per journal & 60% opt-in rate)
do_rate <- 0 # dropout rate, 0 - 10% 



# base rate of negative findings
### 10 - 15% based on Fanelli (2010) graph 

### rate in RR: 55% in novel studies, 66% in replications (https://www.nature.com/articles/d41586-018-07118-1#ref-CR3)

  
#### set up fixed effects
int <- 
cond <- 

#### set up random effects
j_int <- 
a_int <- 


# random intercept model
glmer(pos_result <- cond + (1|journal/article), data = , family = binomial)




# random slopes model
glmer(pos_result <- cond + (cond|journal/article), data = , family = binomial)
