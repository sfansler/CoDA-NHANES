library(gamlss)
library(tidyverse)
get_quantile_curves = function(male_df, female_df, var, weights, cent, df) {

  male_df = male_df
  female_df = female_df
  
  age_male = male_df$Age
  age_female = female_df$Age
  
  y_male = male_df %>%
    select(all_of(var)) %>%
    unlist() %>%
    unname()
    
  y_female = female_df %>%
    select(all_of(var)) %>%
    unlist() %>%
    unname()
  
  weights_male = male_df %>%
    select(all_of(weights)) %>%
    unlist() %>%
    unname()
  
  weights_female = female_df %>%
    select(all_of(weights)) %>%
    unlist() %>%
    unname()
  
  fit_male = gamlss(formula =  y_male ~ pb(Age, df = df), sigma.formula = ~pb(Age, df = df), nu.formula = ~pb(Age, df = df), tau.formula = ~pb(Age, df = df), weights = weights_male, data = male_df)
  fit_female = gamlss(formula =  y_female ~ pb(Age, df = df), sigma.formula = ~pb(Age, df = df), nu.formula = ~pb(Age, df = df), tau.formula = ~pb(Age), weights = weights_female, data = female_df)
  
  preds_male = cbind(unique(centiles.pred(fit_male, xname = "Age", xvalues = age_male, cent = cent, data = male_df)), Sex = "Male")
  preds_female = cbind(unique(centiles.pred(fit_female, xname = "Age", xvalues = age_female, cent = cent, data = female_df)), Sex = "Female")
  
  preds = rbind(preds_male, preds_female) %>%
    rename(Age = x) %>%
    group_by(Age, Sex) %>% #Handles multiple rows of very similar predictions
    reframe(quant_50 = mean(`50`))
    
  return(preds)
}
