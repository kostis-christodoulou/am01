options(scipen=999, show.signif.stars=FALSE, digits=4)

library(tidyverse)
library(car)
library(mosaic)
library(ggfortify)
library(janitor)
library(GGally)
library(broom)
library(moderndive)
library(skimr)
library(here)
library(performance)

CA_test_scores <- read_csv(here("data","CA_test_scores.csv"))[-1]%>% #exclude "obs_no", the first column
  clean_names()

# examine the structure of the resulting data frame
glimpse(CA_test_scores) 

# Quick summary stats for all variables, using skimr::skim()
skim(CA_test_scores)

# Get a correlation-scatterplot matrix, using GGally::ggpairs()
CA_test_scores %>% 
  ggpairs() +
  theme_bw()


# scatter plot with linear model
ggplot(CA_test_scores, aes(x=str, y=test_score)) +
  geom_smooth(method='lm', se=TRUE, level=0.95) +
  geom_point()+
  labs(x="Student-Teacher Ratio (STR)", y="Test Score", title = "Test Scores vs STR") +
  theme_minimal() +
  NULL

#Run a simple regression of testScore on STR
model1 <- lm(test_score~str,data=CA_test_scores)
msummary(model1)
anova(model1)
confint(model1)

# if you want to have the coefficients in a dataframe, use broom::tidy()
tidy(model1)

# You can also plot the usual diagnostics plots using 
# ggfortify::autoplot() which uses the ggplot2 package
library(ggfortify)
autoplot(model1) + 
  theme_bw()

library(broom)
model1_aug <- augment(model1)


# Plot scatterplot and regression line 
ggplot(model1_aug, aes(x=str, y=test_score)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_point()+
  labs(x="Student-Teacher Ratio (STR)", y="Test Score", title = "Test Scores vs STR") +
  theme_minimal() +
  NULL


# Plot fitted line and residuals
ggplot(model1_aug, aes(x=str, y=test_score)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = str, yend = .fitted), color="red", alpha = 0.3) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = .fitted), shape = 1) +
  labs(
      x="Student-Teacher Ratio (STR)", 
      y="Test Score", 
      title = "Test Scores vs STR: Fitted Line and residuals") +
  theme_bw()  # Add theme for cleaner look +
  NULL




model2 <- lm(test_score ~ str + pct_english_learners,data=CA_test_scores)
msummary(model2) 
confint(model2)


# scatter plot with linear model and LOESS best smooth line
ggplot(CA_test_scores, aes(x=avginc, y=test_score)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Plot regression slope
  geom_smooth(color = "red", se = FALSE)+
  labs(x="District Average Income (000s $)", 
       y="Test Score", 
       title = "Test Scores vs Income") +
  theme_bw() +
  NULL





model_quadratic <- lm(test_score~avginc + I(avginc^2),data=CA_test_scores)
msummary(model_quadratic)


model3 <- lm(test_score ~ str + pct_english_learners + meal_pct, data=CA_test_scores)

model4 <- lm(test_score ~ str + pct_english_learners + public_income_assist_pct, data=CA_test_scores)


model5 <- lm(test_score ~ str + pct_english_learners + meal_pct + public_income_assist_pct, data=CA_test_scores)
summary(model4)
anova(model4)
vif(model5)

model5 <- lm(test_score ~ ., data=CA_test_scores)


# Splitting data in training and testing sets
library(rsample)
set.seed(1234)  # for reproducibility, and to always get the same split, set the random seed first

train_test_split <- initial_split(CA_test_scores, prop = 0.75)
CA_scores_train <- training(train_test_split)
CA_scores_test <- testing(train_test_split)

rmse_train <- CA_scores_train %>%
  mutate(predictions = predict(model3, .)) %>%
  summarise(sqrt(sum(predictions - test_score)**2/n())) %>% 
  pull()
rmse_train
# [1] 2.3212

rmse_test <- CA_scores_test %>%
  mutate(predictions = predict(model3, .)) %>%
  summarise(sqrt(sum(predictions - test_score)**2/n())) %>% 
  pull()
rmse_test
# [1] 4.0204

# model comparison with huxtable 
library(huxtable)
huxtable::huxreg(model1, model2, model3, model4, model5,
                 statistics = c('#observations' = 'nobs', 'R squared' = 'r.squared', 'Adj. R Squared' = 'adj.r.squared', 'Residual SE' = 'sigma'), 
                 bold_signif = 0.05, 
                 stars = NULL
) %>% 
  # theme_article() %>% 
  set_caption('Comparison of models')

# Final model (model 3) and predictions

final_model <- model3
mosaic::msummary(final_model)


# use performance::check_model() for diagnostics

performance::check_model(final_model)

get_regression_table(final_model)
get_regression_summaries(final_model)


vif(final_model)


# Here are six imaginary districts, all with the same variables except
# meal_pct, which goes up by 10% in each row
imaginary_district <- tibble(str = 22,
                             pct_english_learners = 20,
                             meal_pct =  c(5, 15, 25, 35, 45, 55))
imaginary_district

# When we plug this multi-row data frame into predict(), it'll generate a
# prediction for each row
predict(final_model, newdata = imaginary_district, interval = "prediction")


# We can also get a confidence interval for Expected value of Y, given these X's
predict(final_model, newdata = imaginary_district, interval = "confidence")

# We can also use broom::augment(). It's  essentially the same thing as predict(), 
# but it adds the predictions and SEs to the imaginary school district 
model_predictions <- broom::augment(final_model, 
                                    newdata = imaginary_district, se_fit=TRUE)

model_predictions <- model_predictions %>% 
  mutate(
    lower = .fitted - 1.96 * .se.fit,
    upper = .fitted + 1.96 * .se.fit
  )

model_predictions 

