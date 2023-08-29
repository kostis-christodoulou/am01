library(tidyverse)
library(data.table)
library(janitor)
library(wbstats)
library(countrycode)
library(skimr)
library(scales)
library(ggthemes)


url <- "https://data.un.org/_Docs/SYB/CSV/SYB63_176_202003_Tourist-Visitors%20Arrival%20and%20Expenditure.csv"

un_tourism_data <- fread(input = url,
                         skip=1) %>% 
  janitor::clean_names() %>% 
  rename(country = v2) %>% 
  select(-c(1,5,6,8,9))

# to save the data file, so you can load it locally
# write_csv(un_tourism_data, "un_tourism_data.csv")

un_tourism_data <- read_csv(here::here("data", "un_tourism_data"))

skimr::skim(un_tourism_data)

tourism_wide <-  un_tourism_data %>% 
    pivot_wider(names_from = "series",
              values_from = "value") %>% 
  clean_names() %>% 
  rename(
    tourism_expenditure = tourism_expenditure_millions_of_us_dollars ,
    tourist_arrivals = tourist_visitor_arrivals_thousands    
  )


glimpse(tourism_wide)

tourism_wide <- tourism_wide %>% 
  filter(!is.na(tourism_expenditure), 
         !is.na(tourist_arrivals))

skim(tourism_wide)

# create new variable that calculates spending per tourist
tourism_wide <- tourism_wide %>% 
  mutate(spending_per_tourist = 1000*tourism_expenditure/tourist_arrivals)


skim(tourism_wide)
#Talk about a skewed distribution for spending_per_tourist: 
# from min of 9, to max of 8376$/tourist. mean = 1135, median = 911


tourism_long <- tourism_wide %>% 
  pivot_longer(cols = 3:5, #columns 3 to 5
               names_to = "indicator",
               values_to = "value")

skimr::skim(tourism_long)

# Turn `indicator` from character to a factor variable
tourism_long <- tourism_long %>% 
  mutate(
    indicator = factor(indicator, 
                       levels = c("tourist_arrivals", "tourism_expenditure", "spending_per_tourist"),
                       labels = c("Tourist Arrivals (000s)", "Tourism Expenditure (million $)", "Spending per tourist($)"))
  )


skimr::skim(tourism_long)



tourism_long %>% 
  filter(country %in% c("France", "Spain", "Italy", "United Kingdom", "Thailand", "China")) %>% 
  ggplot(aes(x=year, y = value, colour=country))+
  geom_line()+
  geom_point(size=0.5)+
  facet_grid(rows = vars(indicator),
             cols = vars(country), 
             scales = "free")+
  theme_bw()+
  theme(legend.position = "none")+
  scale_y_continuous(labels = scales::label_comma())+
  labs(
    title = "Tourist Arrivals and Spending",
    caption = "Source: UN",
    x = "",
    y = ""
  )


# calculate growth rates, starting for each country at 1 (or 100%)
tourism_wide2 <- tourism_wide  %>% 
  group_by(country) %>%
  mutate(
    delta_arrivals = (tourist_arrivals / lag(tourist_arrivals,1)),
    delta_expenditure = (tourism_expenditure/lag(tourism_expenditure,1 )), 
    delta_spending_per_tourist = (spending_per_tourist/lag(spending_per_tourist,1))
  ) %>% 
  replace_na(list(
    delta_arrivals = 1,
    delta_expenditure = 1,
    delta_spending_per_tourist = 1
  )) %>% 
  ungroup() %>% 
  select(country, year, delta_arrivals, delta_expenditure, delta_spending_per_tourist)

# wide to long
tourism_long2 <- tourism_wide2 %>% 
  pivot_longer(cols = 3:5, #columns 3 to 5
               names_to = "indicator",
               values_to = "value")

skimr::skim(tourism_long)


### Plot % changes, with 1=100% as starting point for each country

tourism_long2 %>% 
  filter(country %in% c("France", "Spain", "Italy", "United Kingdom", "Thailand", "China")) %>% 
  ggplot(aes(x=year, y = value, colour=country))+
  geom_line()+
  geom_point(size=0.5)+
  facet_grid(rows = vars(indicator),
             cols = vars(country), 
             scales = "free")+
  theme_bw()+
  theme(legend.position = "none")+
  scale_y_continuous(labels = scales::label_percent())+
  labs(
    title = "Changes in Tourist Arrivals and Spending",
    subtitle = "Base year (100%) = 1995",
    caption = "Source: UN",
    x = "",
    y = ""
  )


# what if we wanted to build confidence intervals of tourist spending not by country, but rather by region, etc?
# we will be using the `countrycode` and `wbstats` packages, because as it turns out country names are not that easy
# to deal with/

# First, get a string of all country names
countries <- un_tourism_data %>% 
  select(country) %>% 
  distinct() %>% 
  pull()

# Use countrycode package to convert names to iso3c https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3
# https://github.com/vincentarelbundock/countrycode
countrycodes <- countries %>% 
  countrycode(
    origin = "country.name",
    destination = "iso3c"
  )

# create a tibble that has countries name and iso3c codes
countrycode_match <- tibble(
  country = countries, 
  iso3c = countrycodes
)

# add iso3c code on dataframe, using a left join
un_tourism_details <- left_join(un_tourism_data, countrycode_match, by = "country")
glimpse(un_tourism_details)


# Use wbstats::wb_countries to get regions and demographics on countries
wb_countries <- wbstats::wb_countries()
glimpse(wb_countries)


# there are quite a few NAs, so let us see how we handle them

be_careful_with_na_omit <- left_join(un_tourism_details, wb_countries, by = "iso3c") %>% 
  na.omit()

un_tourism_final <- left_join(un_tourism_details, wb_countries, by = "iso3c") %>% 
  filter(!is.na(region)) 
glimpse(un_tourism_final)


# We have "country.x" and "country.y" which are the two fields from the two dataframes we joined
un_tourism_final <- un_tourism_final %>% 
  select (-country.y) %>% 
  rename(country = country.x)

un_tourism_wide <- un_tourism_final %>% 
  pivot_wider(names_from = "series",
              values_from = "value") %>% 
  clean_names() %>% 
  filter(!is.na(tourism_expenditure_millions_of_us_dollars), !is.na(tourist_visitor_arrivals_thousands)) %>% 
  mutate(spending_per_tourist = 1000*tourism_expenditure_millions_of_us_dollars / tourist_visitor_arrivals_thousands)

glimpse(un_tourism_wide)

# let us plot the distribution of spending per tourist in each region
un_tourism_wide %>% 
  ggplot(aes(x=spending_per_tourist, colour=region)) +
  # geom_histogram()+
  geom_density()+
  facet_wrap(~region)+
  theme_bw()+
  theme(legend.position = "none")+
  NULL

# Construct a confidence interval of spending per tourist in each region
confidence_intervals <- un_tourism_wide %>% 
  group_by(region) %>% 
  summarise(
    min_spending_per_tourist =  min(spending_per_tourist),
    max_spending_per_tourist =  max(spending_per_tourist),
    median_spending_per_tourist =  median(spending_per_tourist),
    mean_spending_per_tourist = mean(spending_per_tourist),
    sd_spending_per_tourist = sd(spending_per_tourist),
    count = n(),
    se_spending_per_tourist = sd_spending_per_tourist/ sqrt(count),
    t_critical = qt(0.975, count - 1 ),
    lower = mean_spending_per_tourist - t_critical * se_spending_per_tourist,
    upper = mean_spending_per_tourist + t_critical * se_spending_per_tourist
  ) %>% 
  ungroup() %>% 
  arrange(desc(mean_spending_per_tourist))

confidence_intervals %>% 
  select(region, mean_spending_per_tourist, lower, upper) %>% 
  knitr::kable(digits = 2)

