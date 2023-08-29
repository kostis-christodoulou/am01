library(tidyverse)
library(gapminder)
library(mosaic)
library(skimr)
library(ggiraph)
library(vroom)
library(lubridate)


# in 4 steps
gapminder_2007 <- filter(gapminder, year == 2007)
gapminder_2007 <- select(gapminder_2007, country, continent, lifeExp)
gapminder_2007 <- arrange(gapminder_2007, desc(lifeExp))
gapminder_2007

# nested functions- yikes!
arrange(select(filter(gapminder, year == 2007), 
               country, continent, lifeExp), 
        desc(lifeExp)) -> gapminder_2007
gapminder_2007

# using pipe operator- ahhhhhh :-) 
gapminder %>% 
  filter(year == 2007) %>% 
  select(country, continent, lifeExp) %>% 
  arrange(desc(lifeExp))


# import CA contributions data
CA_contributions <- vroom(here::here("data","CA_contributors_2016.csv")) %>% 
  janitor::clean_names()

# inspect dataframe
dplyr::glimpse(CA_contributions)
skimr::skim(CA_contributions)

# average donation for Trump
CA_contributions %>% 
  filter(cand_nm == "Trump, Donald J.") %>%
  summarise(mean_donation = mean(contb_receipt_amt))

# average donation for all candidates
CA_contributions %>% 
  group_by(cand_nm) %>%
  summarise(mean_donation = mean(contb_receipt_amt)) %>% 
  arrange(desc(mean_donation))

# graph - Who raised the most amount of money in CA?

CA_contributions %>% 
  group_by(cand_nm) %>%
  summarise(total_donation = sum(contb_receipt_amt)) %>% 

  mutate(cand_nm = fct_reorder(cand_nm, total_donation, max)) %>% 

  ggplot(aes(x=total_donation, y = cand_nm))+
  geom_col()+
  scale_x_continuous(labels = scales::label_dollar())


# load bikes data
bikes <- readr::read_csv(
  here::here("data", "london_bikes.csv"))

bikes <- bikes %>%   
  mutate(
    wday = wday(date, label = TRUE),
    month = month(date),
    month_name=month(date, label = TRUE)) 

# generate new variable season_name to turn seasons from numbers to Winter, Spring, etc
bikes <- bikes %>%  
  mutate(
    season_name = case_when(
      month_name %in%  c("Dec", "Jan", "Feb")  ~ "Winter",
      month_name %in%  c("Mar", "Apr", "May")  ~ "Spring",
      month_name %in%  c("Jun", "Jul", "Aug")  ~ "Summer",
      month_name %in%  c("Sep", "Oct", "Nov")  ~ "Autumn",
    ),
    season_name = factor(season_name, 
                         levels = c("Winter", "Spring", "Summer", "Autumn")),
    weekend = case_when(
      wday %in%  c("Sat", "Sun")  ~ "Weekend",
      TRUE  ~ "Weekday",
    )
  )

# interactivity with {ggiraph} package
bikes_vs_temperature <- ggplot(bikes, 
                               aes(x=mean_temp, 
                                   y= bikes_hired,
                                   colour=weekend)) +
  
  # rather than using geom_point(), we use geom_point_interactive()
  geom_point_interactive(aes( 
    tooltip = glue::glue("Mean Temp: {mean_temp}\n
                                                  Bikes Hired: {bikes_hired}\n
                                                  Date: {date}")),
    alpha = 0.3) +
  geom_smooth_interactive(se = FALSE, method = lm)+
  theme_minimal()+
  facet_wrap(~weekend, ncol=1)+
  
  # remove legend, as labels show in facet_wrap
  theme(legend.position = "none") 

# you have created the ggplot object, you now pass it to
ggiraph::girafe(ggobj = bikes_vs_temperature,
                pointsize = 8, height_svg = 3)

# IMDB movie data

movies <- readr::read_csv(
  here::here("data", "movies.csv"))


# fct_reorder()
movies %>% 
  slice_max(order_by = gross, n=20) %>% 
  mutate(title = fct_reorder(title, gross)) %>% 
  ggplot(aes(x = gross, y = title)) +
  geom_col() +
  theme_minimal(base_size = 14)+
  labs(
    title = "",
    subtitle = "",
    x = "Gross earnings (US$)",
    y = NULL
  )+
  NULL


movies %>% 
  slice_max(order_by = gross, n=20) %>% 
  mutate(title = fct_reorder(title, gross)) %>%  
  ggplot(aes(x = gross, y = title, fill = genre)) +
  geom_col() +
  theme_minimal(base_size = 14)+
  labs(
    title = "Action and Adventure movies dominate the box office",
    subtitle = "Gross Earnings (US$) at the box office, sample of 3000 IMDB movies",
    x = NULL,
    y = NULL,
    fill = "Movie Genre"
  )+
  
  # make x axis scales into dollars
  scale_x_continuous(labels = scales::label_dollar())+
  
  # ensure title is top-left aligned
  theme(plot.title.position = "plot")


# geom_tile from gapminder
gapminder %>%
  filter(continent == "Europe") %>%
  mutate(
    country = fct_reorder(country, lifeExp, median)
  ) %>%
  ggplot(aes(year, country, fill = lifeExp)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C") +
  theme_minimal(base_size=12)

# facet_wrap
gapminder %>%
  filter(continent == "Europe") %>%
  mutate(country = fct_rev(fct_reorder(country, lifeExp, median))) %>%
  ggplot(aes(year, lifeExp)) + 
  geom_line() +
  facet_wrap(vars(country))+
  theme_minimal(base_size=12)+
  labs(
    title = "Life Expectancy in Europe, 1952 - 2007",
    x = NULL,
    y = NULL,
    caption = "Data Source: Gapminder Project"
  )
