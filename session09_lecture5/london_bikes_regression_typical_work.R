library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(GGally)
library(here)
library(skimr)
library(janitor)
library(broom)
library(huxtable)
library(lubridate)
library(car)
library(performance)
library(ggfortify)

# Explore the relationship between bikes hired and a bunch of explanatory variables

#read the CSV file
bike <- read_csv(here::here("data", "london_bikes.csv"))

# fix dates using lubridate, and generate new variables for year, month, month_name, day, and day_of _week
bike <- bike %>%   
  mutate(
    year=year(date),
    month = month(date),
    month_name=month(date, label = TRUE),
    day_of_week = wday(date, label = TRUE)) 

# generate new variable season_name to turn seasons from numbers to Winter, Spring, etc
bike <- bike %>%  
  mutate(
    season_name = case_when(
      month_name %in%  c("Dec", "Jan", "Feb")  ~ "Winter",
      month_name %in%  c("Mar", "Apr", "May")  ~ "Spring",
      month_name %in%  c("Jun", "Jul", "Aug")  ~ "Summer",
      month_name %in%  c("Sep", "Oct", "Nov")  ~ "Autumn",
    ),
    season_name = factor(season_name, 
                         levels = c("Winter", "Spring", "Summer", "Autumn"))
  )

# skimr::skim() gives us variable types, summary statistics, missing values
# anything that seems off
skim(bike)

# model building

# start the naive model where you just use the average
favstats(~bikes_hired, data = bike)
# can you create a confidence interval for mean bikes_hired? What is the SE?


model1 <- lm(bikes_hired ~ 1, data= bike)
msummary(model1)

# What is the regression's residual standard error? 
# What is the intercept standard error? 

bike %>% 
  select(cloud_cover, humidity, pressure, radiation, precipitation, sunshine, mean_temp, min_temp) %>% 
  ggpairs()


model2 <- lm(bikes_hired ~mean_temp + humidity + pressure + radiation  + precipitation + sunshine, data= bike)
msummary(model2)

model3 <- lm(bikes_hired ~ mean_temp + min_temp+ humidity + pressure + radiation  + precipitation + sunshine, data= bike)
msummary(model3)
vif(model3)
check_model(model3)

model4 <- lm(bikes_hired ~ mean_temp + humidity + pressure + radiation  + precipitation + sunshine + season_name+ factor(year), data= bike)
msummary(model4)
vif(model4)

model5 <- lm(bikes_hired ~ mean_temp + humidity + pressure + radiation  + precipitation + sunshine + season_name+ factor(year) + day_of_week, data= bike)
msummary(model5)
vif(model5)
check_model(model5)


# produce summary table comparing models using huxtable::huxreg()
huxreg(model1, model2, model3, model4, model5,
       statistics = c('#observations' = 'nobs', 
                      'R squared' = 'r.squared', 
                      'Adj. R Squared' = 'adj.r.squared', 
                      'Residual SE' = 'sigma'), 
       bold_signif = 0.05, 
       stars = NULL
) %>% 
  set_caption('Comparison of models')
