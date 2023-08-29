library(tidyverse) # ggplot and the usual goodies
library(rvest) # to scrape wikipedia page
library(lubridate) # to handle conversions from characters to dates

url <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2021_German_federal_election"

# get tables that exist on wikipedia page url
tables <- url %>% 
  read_html() %>% 
  html_nodes(css="table")


# parse HTML tables into a dataframe called polls 
# Use purr::map() to create a list of all tables in URL
polls <- map(tables, . %>% 
               html_table(fill=TRUE)%>% 
               janitor::clean_names())


# list of opinion polls
german_election_polls <- 
  
  # the third table on the wikipedia page contains the list of all opinions polls
  polls[[3]] %>% 

  # drop the first two rows, as they contain variable names and actual election result 
  # and last row that contains 2017 results
  slice(3:(n()-1)) %>%  

  mutate(
    # polls are shown to run from-to, e.g. 9-13 Aug 2021. We keep the last date, 
    # 13 Aug here, as the poll date and we extract it by picking the last 11 
    # characters from that field
    end_date = str_sub(fieldwork_date, -11),
    
    # end_date is still a string, so we convert it into a date object using lubridate::dmy()
    end_date = dmy(end_date),
    
    # we also get the month and week number from the date, if we want to do analysis by month- week, etc.
    month = month(end_date),
    week = isoweek(end_date)
  )

