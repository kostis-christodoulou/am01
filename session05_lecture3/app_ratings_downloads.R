library(tidyverse)
library(GGally)
library(skimr)
library(mosaic)

# while it's fine to know about working directories, I suggest 
# you learn to use the package 'here' that makes organising files easy
# https://malco.io/2018/11/05/why-should-i-use-the-here-package/
app_ratings <- read_csv(here::here('data', 'app_ratings.csv')) 

app_ratings %>% 
  skim()

# summary statistics of number of downloads 
favstats(~downloads, data = app_ratings)

# summary statistics of downloads vs. free and downloads vs. OS
mosaic::favstats(downloads ~ free, data = app_ratings)
mosaic::favstats(downloads ~ os, data = app_ratings)


# ---------------------------
# Rating vs. free

mosaic::favstats(rating ~ free, data = app_ratings)

ggplot(app_ratings, aes (x=rating, y = factor(free)))+
  geom_boxplot()+
  theme_minimal()+
  geom_jitter()+
  NULL

ggplot(app_ratings, aes (x=rating, fill = factor(free)))+
  geom_density(alpha = 0.3)+
  theme_minimal()+
  NULL

t.test(rating ~ free, data = app_ratings)