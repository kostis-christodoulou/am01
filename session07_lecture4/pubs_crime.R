library(tidyverse)
library(lubridate)
library(here)
library(mosaic)
library(infer)
library(skimr)
library(ggthemes)
library(GGally)
library(huxtable)

# Load data
crime <- read_csv(here::here('data', 'beerhall.csv'))

# skim dataframe to get summary statistics for entire dataset
crime %>% 
  skimr::skim()

#alternatively, use mosaic:favstats()
favstats(~criminals, data = crime)

#boxplot of criminals by region
ggplot(crime, aes(y= reorder(region_name,criminals), x=criminals))+
  geom_boxplot()+
  labs(y="")+
  theme_minimal()

# https://gka.github.io/palettes
my_Palette = c('#00429d', '#2e59a8', '#4771b2', '#5d8abd', '#73a2c6', '#8abccf', '#a5d5d8', '#c5eddf', '#ffffe0')

# let us look at total number of criminals per region
crime %>% 
  group_by(region_name) %>% 
  summarise(total_crime = sum(criminals)) %>% 
  ggplot(aes(x= reorder(region_name,total_crime), y=total_crime))+
  geom_col()+
  coord_flip()+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = 'Total Crime in England, 1856', y = "criminals (per 100k population)", x= "")+
  # scale_fill_brewer(palette="Set1")+
#  scale_fill_manual(values=my_Palette)+
  NULL

#let us get a matrix of density plots, scatterplots + correlations
crime %>% 
  select(public_houses, criminals) %>% 
  ggpairs()


# ok, a scatterplot of criminals ~ pubs
ggplot(crime, aes(x=public_houses, y = criminals)) +
  geom_point()+
  geom_smooth(method = lm,se = FALSE)+
  theme_bw()+
  
  #plot line for average
  geom_hline(yintercept = 152.9, size = 1.1, colour = "red")+
  labs(title= "Relationship between Crime and Pubs, England 1856",
       x = "Pubs per 100K population",
       y = "Criminals per 100K population")+
  NULL


# ***** Fit linear regression models: First, just the mean
model0 <- lm(criminals ~ 1, data= crime)

model0 %>% 
  broom::tidy(conf.int=TRUE)

model0 %>% 
  broom::glance()

# ***** Fit linear regression models: criminals on number of pubs
model1 <- lm(criminals ~ public_houses, data= crime)

model1 %>% 
  broom::tidy(conf.int=TRUE)

model1 %>% 
  broom::glance()


# ***** Fit linear regression models: criminals on number of pubs + school + church attendance
model2 <- lm(criminals ~ public_houses + school_attendance + worship_attendance, data= crime)

model2 %>% 
  broom::tidy(conf.int=TRUE)

model2 %>% 
  broom::glance()

# produce summary table comparing models using huxtable::huxreg()
huxreg(model0, model1, model2,
       statistics = c('#observations' = 'nobs', 
                      'R squared' = 'r.squared', 
                      'Adj. R Squared' = 'adj.r.squared', 
                      'Residual SE' = 'sigma'), 
       bold_signif = 0.05, 
       stars = NULL
) %>% 
  set_caption('Comparison of models')



    