#### STRUCTURAL EQUATION MODELLING USING LAVAAN

# analysis of woody cover ibn Tanzania dataset


# restore libraries

rm(list = ls()) # clear environment

library(tidyverse)
# load the lavaan library
library(lavaan)

SEM_data <-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRq0HTbPC8h6MHiLJfmOUxRJmal8gH00_ftIfQjuKPdtzClFpDY4IlAM6EnZlHz4dgZZS3wsIQtnEIw/pub?gid=2033923084&single=true&output=csv") 
names(SEM_data)

# standardize all variables to mean 0 and standard deviation 1
SEM_data_std <- SEM_data |>
  mutate_all(~(scale(.) %>% as.vector)) |>
  as_tibble()
SEM_data_std

# make a pairs panel to inspect linearity of relations and expected normality of residuals
psych::pairs.panels(SEM_data %>% select(dist2river,elevation,rainfall,cec,
                                            burnfreq, hills, woody), stars = T, ellipses = F)
psych::pairs.panels(SEM_data_std %>% select(dist2river,elevation,rainfall,cec,
                                            burnfreq, hills, woody), stars = T, ellipses = F)

# analyse the model (response ~ predictors) with a multiple regression approach 
multreg <- lm(woody ~ hills + burnfreq + cec + rainfall + elevation + dist2river, data = SEM_data_std)
summary(multreg)

# Make a lavaan model and fit the model 
woody_model1 <- 'woody ~ cec + rainfall + dist2river
                rainfall ~ elevation 
                cec ~  rainfall + burnfreq + elevation 
                dist2river ~ elevation
                burnfreq ~ rainfall + elevation 
                '
                
woody_model <- '
  # Direct predictors of woody
  woody ~ cec + rainfall + dist2river

  # Predictors of rainfall
  rainfall ~ elevation + hills

  # Predictors of cec
  cec ~ rainfall + dist2river

  # Predictors of dist2river
  dist2river ~ rainfall
  
  # Optional: hills explained by elevation
  hills ~ elevation
'

woody_model2 <- '
  woody ~ cec + rainfall + dist2river + hills 
  rainfall ~ elevation
  cec ~ rainfall + hills + elevation
  dist2river ~ elevation 
  hills ~~ elevation
'


woody_model2
woody_fit <- lavaan::sem(woody_model2, data = SEM_data_std)
# show the model results
summary(woody_fit, standardized = T, fit.measures = T, rsquare = T)

#how can I make the model to fit better?

library(lavaanPlot)

# plot the model using lavaan plot
lavaanPlot::lavaanPlot(woody_fit, 
                       coefs = TRUE, 
                       stand = TRUE,
                       graph_options=list(rankdir="LR"),
                       stars="regress")


