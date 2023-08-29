library(tidyverse)
library(broom)
library(GGally)
library(here)
library(httr)
library(readxl)
library(janitor)
library(huxtable)
library(skimr)
library(car)
library(ggfortify)

url <- "https://happiness-report.s3.amazonaws.com/2021/DataPanelWHR2021C2.xls"

# you can get ideas for frther EDA at
# https://ourworldindata.org/happiness-and-life-satisfaction

# Download data to temporary file
httr::GET(url, write_disk(happiness.temp <- tempfile(fileext = ".xls")))

# Use read_excel to read it as dataframe
world_happiness <- read_excel(happiness.temp,
                    sheet = "Sheet1",
                    range = cell_cols("A:K")) %>% 
  janitor::clean_names()

# inspect dataframe; how many observations, kinds of variables, etc.
glimpse(world_happiness)

# produce scatterplot-correlation matrix for 2019 using GGally::ggpairs()
world_happiness_19 <- world_happiness %>%
  filter(year==2019) 

world_happiness_19 %>% 
  select(-c(country_name,year)) %>% 
  ggpairs(alpha = 0.3)


# produce summary statistics for life_ladder, a measure of happiness score
world_happiness_19 %>% 
  select(life_ladder) %>% 
  skim()

# fit 3 models, with model1 being just the average happiness
model1 <- lm(life_ladder ~ 1, data = world_happiness_19)
mosaic::msummary(model1)

model2 <- lm(life_ladder ~ freedom_to_make_life_choices, data = world_happiness_19)
mosaic::msummary(model2)

model3 <- lm(life_ladder ~ log_gdp_per_capita + freedom_to_make_life_choices, data = world_happiness_19)
mosaic::msummary(model3)

model4 <- lm(life_ladder ~ log_gdp_per_capita + social_support + freedom_to_make_life_choices, data = world_happiness_19)
mosaic::msummary(model4)

model5 <- lm(life_ladder ~ log_gdp_per_capita + healthy_life_expectancy_at_birth + social_support + freedom_to_make_life_choices, data = world_happiness_19)
mosaic::msummary(model5)


# if you want regression summary in tibble format, use broom::tidy() and broom::glance()
# get model summary and R2, etc for both models
model2 %>% broom::tidy(conf.int = TRUE)
model2 %>% broom::glance()

model3 %>% broom::tidy(conf.int = TRUE)
model3 %>% broom::glance()


# produce summary table comparing models using huxtable::huxreg()
huxreg(model1, model2, model3, model4,model5,
       statistics = c('#observations' = 'nobs', 
                      'R squared' = 'r.squared', 
                      'Adj. R Squared' = 'adj.r.squared', 
                      'Residual SE' = 'sigma'), 
       bold_signif = 0.05, 
       stars = NULL
) %>% 
  set_caption('Comparison of models')

# Check whether any model has a VIF (Variance Inflation Factor) greater than 5
car::vif(model3)
car::vif(model4)
car::vif(model5)