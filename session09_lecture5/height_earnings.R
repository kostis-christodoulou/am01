library(tidyverse)
library(mosaic)
library(broom)
library(skimr)
library(GGally)
options(scipen=999, show.signif.stars=FALSE, digits=4)


height_earnings <- read_csv(here("data", "height_earnings.csv"))

skim(height_earnings)

# Quick summary stats
summary(height_earnings)



height_earnings <- height_earnings %>%  # need  to order education levels from least to most
  mutate(
    ed_level = fct_relevel(ed_level, 
                           "Elementary", 
                           "High School", 
                           "College", 
                           "Some Graduate School", 
                           "Graduate Diploma")
  )



# Do men earn more than women?
favstats(~earn, data=height_earnings)
favstats(earn ~ gender, data=height_earnings)

t.test(earn~gender,data=height_earnings)
confint(t.test(earn~gender,data=height_earnings))

favstats( earn ~ gender+ed_level, data=height_earnings)


# Regression models
model0 <- lm(earn ~ 1, data=height_earnings)


model1 <- lm(earn~height, data=height_earnings)
msummary(model1)
anova(model1)


model2 <- lm(earn~height+gender, data=height_earnings)
msummary(model2)
anova(model2)

height_earnings %>% 
  select(gender, age, height, earn) %>% 
  ggpairs(aes(colour = gender, alpha = 0.3))+
  theme_bw()

model3 <- lm(earn~., data=height_earnings)
msummary(model3)


model4 <- lm(earn~.-race -hispanic, data=height_earnings)
msummary(model4)


huxreg(model0, model1, model2, model3, model4, 
       statistics = c('#observations' = 'nobs', 
                      'R squared' = 'r.squared', 
                      'Adj. R Squared' = 'adj.r.squared', 
                      'Residual SE' = 'sigma'), 
       bold_signif = 0.05, 
       stars = NULL
) %>% 
  set_caption('Comparison of models')
