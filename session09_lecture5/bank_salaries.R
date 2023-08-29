library(mosaic)
library(tidyverse)
library(here)
library(skimr)
library(huxtable)

bank_salaries <-read.csv(here("data","bank_salaries.csv"))

skim(bank_salaries)


model1 <- lm(salary ~ years_bank + gender , data=bank_salaries)
model2 <- lm(salary ~ years_bank + gender + education  , data=bank_salaries)

bank_salaries <- bank_salaries %>% 
  mutate( education = as.factor(education),
          job_grade = as.factor(job_grade))

model2f <- lm(salary ~ years_bank + gender + education  + job_grade, data=bank_salaries)


mosaic::msummary(model1)
mosaic::msummary(model2)
mosaic::msummary(model2f)


model3 <- lm(salary ~ years_bank*gender, data=bank_salaries)
mosaic::msummary(model3)



model4 <- lm(salary ~ ., data=bank_salaries)
mosaic::msummary(model4)


model5 <- lm(salary ~ years_bank*gender + education + job_grade, data=bank_salaries)
mosaic::msummary(model5)

model6 <- lm(salary ~ years_bank*gender +  job_grade, data=bank_salaries)
mosaic::msummary(model6)


bank_salaries %>% 
  group_by(gender, job_grade) %>% 
  count()

favstats(salary~job_grade, data=bank_salaries)
favstats(gender~job_grade, data=bank_salaries)

model7 <- lm(salary ~ years_bank*gender +  job_grade*gender, data=bank_salaries)
mosaic::msummary(model7)


huxreg(model1,  model2, model3, model4, model5, model6, model7,
       statistics = c('#observations' = 'nobs', 
                      'R squared' = 'r.squared', 
                      'Adj. R Squared' = 'adj.r.squared', 
                      'Residual SE' = 'sigma'), 
       bold_signif = 0.05, 
       stars = NULL
) %>% 
  set_caption('Comparison of models')
