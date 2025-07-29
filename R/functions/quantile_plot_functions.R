library(gamlss)
library(tidyverse)
get_quantile_curves = function(male_df, female_df, var, weights, cent, df) {

  compositions_male = male_df
  compositions_female = female_df
  
  y_male = compositions_male %>%
    select(all_of(var)) %>%
    unlist() %>%
    unname()
    
  y_female = compositions_female %>%
    select(all_of(var)) %>%
    unlist() %>%
    unname()
  
  weights_male = compositions_male %>%
    select(all_of(weights)) %>%
    unlist() %>%
    unname()
  
  weights_female = compositions_female %>%
    select(all_of(weights)) %>%
    unlist() %>%
    unname()
  
  fit_male = gamlss(formula =  y_male ~ pb(Age, df = df), sigma.formula = ~pb(Age, df = df), nu.formula = ~pb(Age, df = df), tau.formula = ~pb(Age, df = df), weights = weights_male, data = compositions_male)
  fit_female = gamlss(formula =  y_female ~ pb(Age, df = df), sigma.formula = ~pb(Age, df = df), nu.formula = ~pb(Age, df = df), tau.formula = ~pb(Age, df = df), weights = weights_female, data = compositions_female)
  
  preds_male = cbind(unique(centiles.pred(fit_male, xname = "Age", xvalues = compositions_male$Age, cent = cent)), Sex = "Male")
  preds_female = cbind(unique(centiles.pred(fit_female, xname = "Age", xvalues = compositions_female$Age, cent = cent)), Sex = "Female")
  
  preds = rbind(preds_male, preds_female) %>%
    rename(Age = x) %>%
    group_by(Age, Sex) %>% #Handles multiple rows of very similar predictions
    reframe(quant_50 = mean(`50`))
    
  return(preds)
}
