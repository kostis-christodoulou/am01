library(tidyverse) # ggplot and the usual goodies
library(rvest) # to scrape wikipedia page
library(lubridate) # to handle conversions from characters to date objects
library(zoo) # to calculate rolling  averages of last k polls
library(showtext)
library(ggtext)

# load fonts we will use
font_add_google("Montserrat", "Montserrat") # official LBS font
font_add_google("Lato", "Lato")

## Automatically use showtext to render text for future devices
showtext_auto()

# data sourced from https://en.wikipedia.org/wiki/Nationwide_opinion_polling_for_the_2024_United_States_presidential_election
url <- "https://en.wikipedia.org/wiki/Nationwide_opinion_polling_for_the_2024_United_States_presidential_election"


# get tables that exist on wikipedia page 
tables <- url %>% 
  read_html() %>% 
  html_nodes(css="table")


# parse HTML tables into a dataframe called polls 
# Use purr::map() to create a list of all tables in URL
polls <- map(tables, . %>% 
               html_table(fill=TRUE)%>% 
               janitor::clean_names())

# list of relevant opinion polls
polls[[4]]  #  table 4 on the page contains the list of all opinions polls
  
# the dates of the opinion polls are given as, e.g. July 30 - August 2, 2024
# We use a regular expression to extract the latest date and use that 
library(stringr)
extract_latest_date <- function(date_string) {
  # Pattern for dates spanning two months
  pattern1 <- "(.+\\s\\d+)\\s*[-–]\\s*(.+\\s\\d+,\\s*\\d{4})"
  
  # Pattern for dates within the same month
  pattern2 <- "(.+)\\s\\d+\\s*[-–]\\s*(\\d+,\\s*\\d{4})"
  
  # Pattern for single dates
  pattern3 <- "(.+\\s\\d+,\\s*\\d{4})"
  
  match1 <- str_match(date_string, pattern1)
  match2 <- str_match(date_string, pattern2)
  match3 <- str_match(date_string, pattern3)
  
  if (!is.na(match1[1])) {
    return(match1[3])  # Return the second date (latest) for two-month span
  } else if (!is.na(match2[1])) {
    return(paste(match2[2], match2[3]))  # Combine month with the latest day for same-month span
  } else if (!is.na(match3[1])) {
    return(match3[2])  # Return the single date
  } else {
    return(NA)
  }
}


# manipulate- tidy data
us_election_polls <- polls[[4]] %>%  # table 4 on the page contains the list of all opinions polls
  filter(poll_source != "") %>%
  mutate(
    # convert characters to numbers
    harris = parse_number(kamala_harris_democratic),
    trump = parse_number(donald_trump_republican),
    other = parse_number(others_undecided),
    moe = parse_number(marginof_error),
    
    # apply our function to get closing date of poll as a character
    end_date = purrr::map_chr(date, extract_latest_date),
    
    # and now get it as a date object
    end_date = lubridate::mdy(end_date),
    ) %>% 

  # Filter for polls after 2024-07-22, when Biden announced his official withdrawal 
  # and when Harris declared her candidacy for president. 
  filter(end_date > "2024-7-22") %>% 
  
  # separate sample size and likely audience
  # LV = Likely voters, RV = Registered Voters, A = Adults
  separate_wider_delim(samplesize_b, 
                       delim = " ",
                       names = c("sample_size", "audience")) %>% 
  
  # drop columns that are not needed
  select(-c(kamala_harris_democratic,
         donald_trump_republican, 
         others_undecided,
         marginof_error))
  

# time series plot
us_election_polls_long <- us_election_polls %>% 
  select(end_date, Harris = harris, Trump = trump) %>% 
  pivot_longer(cols = 2:3,
               names_to = "candidate",
               values_to = "percent") 


# use colour codes for parties
# even though party colours is not straight-forward... 
# https://blog.datawrapper.de/partycolors/
my_colour_palette = c(
  "#1b44c9", # Democrats
  "#E81B23"  # Republicans
)


us_election_polls_long %>% 
  ggplot()+
  aes(x=end_date, y= percent, colour = candidate)+
  geom_point(alpha=0.25)+
  scale_colour_manual(values = my_colour_palette)+
  geom_smooth(se=F)+
  theme_minimal()+
  scale_x_date(date_minor_breaks = "1 month")+
  labs(
    title = "Opinion polling for the 2024 US Presidential election",
    subtitle = "Polls since Jul 22, 2024",
    x = NULL, y = "Percent %",
    caption = "Source: https://en.wikipedia.org/wiki/Nationwide_opinion_polling_for_the_2024_United_States_presidential_election"
    ) +
  # ensure title is top-left aligned
  theme(plot.title.position = "plot")+
  theme(text=element_text(size=12, family="Montserrat"))+
  NULL


# calculating a rolling average
number_of_polls <- 7

rolling_mean <- us_election_polls_long %>% 
  group_by(candidate) %>% 
  
  #  Use the rollmean() function from the zoo package to get a moving average of the last k polls
  # The first argument you want to specify is the variable you're averaging, percent in our case.
  # The second is the number of observations of that variable to average together, k=7
    mutate(rolling_average = zoo::rollmean(percent, 
                                 k = number_of_polls, 
                                 fill = NA, 
                                 align = "left"
                                 ),
           rolling_sd = zoo::rollapply(percent, 
                              FUN=sd,
                              width = number_of_polls,
                              fill = NA,
                              align = "left"
                              ),
           lower = rolling_average - qt(0.975, number_of_polls - 1) * rolling_sd / sqrt(number_of_polls),
           upper = rolling_average + qt(0.975, number_of_polls - 1) * rolling_sd / sqrt(number_of_polls),
  )

rolling_mean %>% 
  ggplot()+
  aes(x=end_date, y= rolling_average, colour=candidate, fill=candidate)+
  geom_point(alpha=0.2)+
  geom_smooth(se=F)+
  geom_ribbon(aes(ymin = lower, ymax = upper),  
              alpha = 0.1
              ) +
  scale_colour_manual(values=my_colour_palette)+
  scale_fill_manual(values=my_colour_palette)+
  theme_minimal()+
  scale_x_date(date_minor_breaks = "1 month")+
  labs(
    title = "Rolling average of last 7 polls for the 2024 US Presidential election",
    subtitle = "Polls since Jul 22, 2024",
    x = NULL, y = "Percent %",
    caption = "Data Source: https://en.wikipedia.org/wiki/Nationwide_opinion_polling_for_the_2024_United_States_presidential_election"
  ) +
  # ensure title is top-left aligned
  theme(plot.title.position = "plot")+
  theme(text=element_text(size=12, family="Lato"))+
  NULL
  
  
