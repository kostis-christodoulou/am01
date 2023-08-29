options(digits = 4)
library(tidyverse)
library(lubridate)
library(janitor)
library(here)
library(vroom)
library(skimr)


# Hex color codes for Democrat Blue and Republican Red
party_colors <- c("#2E74C0", "#CB454A")


# Make sure you use readr::read_csv() as it is significantly faster than read.csv()
CA_contributors_2016 <- read_csv(here::here("data", "CA_contributors_2016.csv"))

# .. or if you want real speed, go for vroom::vroom()
CA_contributors_2016 <- vroom(here::here("data", "CA_contributors_2016.csv"))

glimpse(CA_contributors_2016)

CA_contributors_2016 %>% 
  select(contb_receipt_amt) %>% 
  skim()

# Highest Individual Contribution -----------------------------------------
CA_contributors_2016 %>% 
#  ?????????  %>% 
  View()

# What was the average donation for a candidate, say Trump? ---------------
CA_contributors_2016 %>% 
  filter(cand_nm == 'Trump, Donald J.') %>% 
#  ???????

# What was the average donation by candidate, ranked in descending order?-----------
CA_contributors_2016 %>% 
# ????
# ????
# ????
  View()


# Who raised the most amount of money, ranked in descending order? 
# Besides total_contribution, you may want to also calculate
# avg_contribution, median_contribution, and count
CA_contributors_2016 %>% 
  

# Challenge 2 What were the top 10 cities in terms of total_contribution for each of the two candidates 

