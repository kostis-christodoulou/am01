library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(GGally)
library(here)
library(skimr)
library(janitor)
library(broom)
library(huxtable)

# Explore the relationship between birth weight and mother's smoking habit

# import the data set
smoking_birth_weight <- read_csv(here::here("data", "smoking_birth_weight.csv"))

# skimr::skim() gives us variable types, summary statistics, missing values
# anything that seems off
skimr::skim(smoking_birth_weight)

# how many mothers were smokers vs non smokers in our sample?
smoking_birth_weight %>% 
  count(habit, sort=TRUE) %>% 
  mutate(proportion = n/sum(n))

# descriptive statistics on weight by smoking habit
favstats(weight_kg ~ habit, data = smoking_birth_weight)
# the mean difference in weight is 144gm (3.244 - 3.100)

# write down, by hand, the two CIs
t.test(weight_kg ~ habit, data = smoking_birth_weight)


smoking_birth_weight %>% 
  filter(!is.na(habit)) %>% 
  select(weight_kg, habit ) %>% 
  ggpairs()+
  theme_bw()

# model building

# start the naive model where you just use the average
favstats(~weight_kg, data = smoking_birth_weight)
# can you create a confidence interval for mean birth weight? What is the SE?


model1 <- lm(weight_kg ~ 1, data= smoking_birth_weight)
msummary(model1)

# What is the regression's residual standard error? 
# What is the intercept standard error? 


model2 <- lm(weight_kg ~ weeks, data= smoking_birth_weight)
msummary(model2)

model3 <- lm(weight_kg ~ weeks + marital, data= smoking_birth_weight)
msummary(model3)

model4 <- lm(weight_kg ~ weeks + marital + habit, data= smoking_birth_weight)
msummary(model4)

model5 <- lm(weight_kg ~ weeks + marital + habit + whitemom + gender + gained_kg, data= smoking_birth_weight)
msummary(model5)

model6 <- lm(weight_kg ~ . , data= smoking_birth_weight)
msummary(model6)

# produce summary table comparing models using huxtable::huxreg()
huxreg(model1, model2, model3, model4, model5, model6,
       statistics = c('#observations' = 'nobs', 
                      'R squared' = 'r.squared', 
                      'Adj. R Squared' = 'adj.r.squared', 
                      'Residual SE' = 'sigma'), 
       bold_signif = 0.05
) %>% 
  set_caption('Comparison of models')
