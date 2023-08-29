library(tidyverse)
library(janitor)
library(infer)
library(here)
library(skimr)

set.seed(1234)

nw14sa_rents <- read_csv(here::here("data", "nw14sa_rents.csv")) %>% 
  clean_names()

nw14sa_rents %>%
  # Select 2-bedroom flat
  filter(bedrooms == 2) %>%
  skim()

mean_rent <- 3137
sd <- 1411
n <- 24
se <- sd / sqrt(n)
lower95 <- mean_rent - qt(0.975,n-1) * se
upper95 <- mean_rent + qt(0.975,n-1) * se


nw14sa_rents %>%
  # Select 2-bedroom flat
  filter(bedrooms == 2) %>%
  ggplot(aes(x=rent_pcm))+
  geom_histogram()


# bootstrap for MEAN rent
boot_rent <- nw14sa_rents %>%
  # Select 2-bedroom flat
  filter(bedrooms == 2) %>%
  
  # Specify the variable of interest
  specify(response = rent_pcm) %>%
  
  # Generate a bunch of bootstrap samples
  generate(reps = 1000, type = "bootstrap") %>%
  
  # Find the median of each sample
  calculate(stat = "median")

percentile_ci <- boot_rent %>%
  get_ci(level = 0.95, type = "percentile")

mean_rent <- ggplot(boot_rent, aes(x = stat)) +
  geom_histogram(binwidth = 50) +
  labs(title= "Bootstrap distribution of means",
       x = "Mean rent per bootstrap sample",
       y = "Count") +
  geom_vline(xintercept = percentile_ci$lower_ci, colour = 'orange', size = 2, linetype = 2) +
  geom_vline(xintercept = percentile_ci$upper_ci, colour = 'orange', size = 2, linetype = 2)

visualize(boot_rent) + 
  shade_ci(endpoints = percentile_ci,fill = "khaki")+
  geom_vline(xintercept = lower95, colour = "red")+
  geom_vline(xintercept = upper95, colour = "red")

# bootstrap for MEDIAN rent
boot_rent <- nw14sa_rents %>%
  # Select 2-bedroom flat
  filter(bedrooms == 2) %>%
  
  # Specify the variable of interest
  specify(response = rent_pcm) %>%
  
  # Generate a bunch of bootstrap samples
  generate(reps = 1000, type = "bootstrap") %>%
  
  # Find the median of each sample
  calculate(stat = "median")

percentile_ci <- boot_rent %>%
  get_ci(level = 0.95, type = "percentile")

median_rent <- ggplot(boot_rent, aes(x = stat)) +
  geom_histogram(binwidth = 50) +
  labs(title= "Bootstrap distribution of medians",
       x = "Median rent per bootstrap sample",
       y = "Count") +
  geom_vline(xintercept = percentile_ci$lower_ci, colour = 'orange', size = 2, linetype = 2) +
  geom_vline(xintercept = percentile_ci$upper_ci, colour = 'orange', size = 2, linetype = 2)

visualize(boot_rent) + 
  shade_ci(endpoints = percentile_ci,fill = "khaki")

library(patchwork)
mean_rent / median_rent
