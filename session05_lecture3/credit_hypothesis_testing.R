library(tidyverse)
library(GGally)
library(skimr)
library(mosaic)

# while it's fine to know about working directories, I suggest 
# you learn to use the package 'here' that makes organising files easy
# https://malco.io/2018/11/05/why-should-i-use-the-here-package/
credit <- read_csv(here::here('data', 'credit.csv')) %>% 
  rename(own_house = own)

credit %>% 
  skim()

# summary statistics of balance 
favstats(~balance, data=credit)

# summary statistics of balance vs. married and balance vs. student
mosaic::favstats(balance ~ married, data = credit)
mosaic::favstats(balance ~ student, data = credit)

mosaic::favstats(balance ~ own_house, data = credit)

# ---------------------------
# Balance vs. married

favstats(balance ~ married, data = credit)

ggplot(credit, aes (x=balance, y = married))+
  geom_boxplot()+
  theme_minimal()+
  NULL

ggplot(credit, aes (x=balance, fill = married))+
  geom_density(alpha = 0.3)+
  theme_minimal()+
  NULL


credit %>% 
  select(married, balance) %>% 
  ggpairs(aes(fill=married, alpha = 0.3)) +
  theme_bw()

t.test(balance ~ married, data = credit)


# ---------------------------
# Balance vs. student

ggplot(credit, aes (x=balance, y = student))+
  geom_boxplot()+
  theme_minimal()+
  NULL


ggplot(credit, aes (x=balance, fill = student))+
  geom_density(alpha = 0.3)+
  theme_minimal()+
  NULL



credit %>% 
  select(student, balance) %>% 
  ggpairs(aes(fill=student, alpha = 0.3)) +
  theme_bw()

t.test(balance ~ student, data = credit)

