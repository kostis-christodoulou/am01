library(tidyverse)
library(wbstats)
library(janitor)
library(rvest)
library(countrycode)
library(GGally)
library(ggrepel)
library(patchwork)



# get World bank data using wbstats
indicators <- c("SP.POP.TOTL",
                "NY.GDP.PCAP.KD",
                "NY.GDP.MKTP.CD"
) 


worldbank_data <- wb_data(country="countries_only", #countries only- no aggregates like Latin America, Europe, etc.
                          indicator = indicators, 
                          start_date = 2019, 
                          end_date = 2019)%>% 
  clean_names()


# get a dataframe of information regarding countries, indicators, sources, regions, indicator topics, lending types, income levels,  from the World Bank API 
countries <-  wbstats::wb_cachelist$countries

# URL: to get 2016 Olympics medals
url <- "https://en.wikipedia.org/wiki/2020_Summer_Olympics_medal_table"


# get tables that exist on wikipedia page 
tables <- url %>% 
  read_html() %>% 
  html_nodes(css="table")


# parse HTML tables into a dataframe called medals 
# Use purr::map() to create a list of all tables in URL
medals <- map(tables, . %>% 
               html_table(fill=TRUE)%>% 
               janitor::clean_names())


# list of medals
olympics_2020_medals <- medals[[3]] %>% # the third table on the page contains the list of all opinions polls
    slice(1:(n()-1))  # drop the last row that contains grand total of medals

# if we try to join the tables, we will not succeed... the wikipedia page with gold medals has, e.g., Grat Britain (GBR)
# whereas the world bank data lists it as United Kingdom. To automate the process we must use the {countrycode} package
# to get the ISO3 code for each country


# Step 1 - prepare olympic medals table

medals_iso3 <-  olympics_2020_medals %>% 
  select(noc) %>% 
  pull() %>% 
  countrycode(
    origin = "country.name",
    destination = 'iso3c') %>% 
  as_tibble()

medals_iso <- bind_cols(olympics_2020_medals, medals_iso3)

# Step 2 - left_join  world bank data table with table 'country' that contains 
# country characteristics ISO3 

worldbank_data <- left_join(worldbank_data, countries, by = "iso3c")

# Step 3 - left_join  olympic medals table with ISO3 code with world bank data

my_data <- left_join(medals_iso,
                     worldbank_data,
                     by = c("value" = "iso3c")) %>% 
  rename(
    gdp_current_usd = ny_gdp_mktp_cd,
    gdp_per_cap_2010 = ny_gdp_pcap_kd,
    population = sp_pop_totl
  )

my_data %>% 
  select(total, gdp_current_usd, gdp_per_cap_2010, population) %>% 
  ggpairs()



# medals vs GDP_current USD by income level
ggplot(my_data, aes(x=gdp_current_usd, y = total))+
  geom_point()+
  geom_smooth(se=F, method="lm")+
  geom_text_repel(aes(label = value))+
  theme_bw()

# medals vs population by income_level
ggplot(my_data, aes(x=population, y = total, colour = income_level))+
  geom_point()+
  geom_smooth(se=F, method="lm")+
  geom_text_repel(aes(label = value))+
  theme_bw()


# medals vs GDP per capita by income_level
ggplot(my_data, aes(x=gdp_per_cap_2010, y = total, colour=income_level))+
  geom_point()+
  geom_smooth(se=F, method="lm")+
  geom_text_repel(aes(label = value))+
  theme_bw()+
  scale_x_continuous(labels = scales::dollar_format(suffix = "K",scale = 1e-3))



# medals vs population
medals_population <- ggplot(my_data, aes(x=population, y = total))+
  geom_point()+
  geom_smooth(se=F, method="lm")+
  geom_text_repel(aes(label = value))+
  theme_bw()+
  scale_x_continuous(labels = scales::label_number(suffix = "m", scale = 1e-6)) 
  
# medals vs population with CHN and IND filtered out
medals_population2 <- my_data %>% 
  filter(population<500e6) %>% 
  ggplot(aes(x=population, y = total))+
  geom_point()+
  geom_smooth(se=F, method="lm")+
  geom_text_repel(aes(label = value))+
  theme_bw()+
  scale_x_continuous(labels = scales::label_number(suffix = "m", scale = 1e-6)) 
  

# medals vs GDP_current USD 
medals_gdp <- ggplot(my_data, aes(x=gdp_current_usd, y = total))+
  geom_point()+
  geom_smooth(se=F, method="lm")+
  geom_text_repel(aes(label = value))+
  theme_bw()+
  scale_x_continuous(labels = scales::dollar_format(suffix = "T", scale = 1e-12))


# medals vs GDP_current USD with outliers removed

medals_gdp2  <- my_data %>% 
  filter(gdp_current_usd<1e13) %>%   
  ggplot(aes(x=gdp_current_usd, y = total))+
  geom_point()+
  geom_smooth(se=F, method="lm")+
  geom_text_repel(aes(label = value))+
  theme_bw()+
  scale_x_continuous(labels = scales::dollar_format(suffix = "T", scale = 1e-12))


# medals vs GDP per capita 
medals_gdp_per_cap <- ggplot(my_data, aes(x=gdp_per_cap_2010, y = total))+
  geom_point()+
  geom_smooth(se=F, method="lm")+
  geom_text_repel(aes(label = value))+
  theme_bw()+
  scale_x_continuous(labels = scales::dollar_format(suffix = "K",scale = 1e-3))


medals_gdp_per_cap2 <- ggplot(my_data, aes(x=gdp_per_cap_2010, y = total, colour=income_level))+
  geom_point()+
  geom_smooth(se=F, method="lm")+
  geom_text_repel(aes(label = value))+
  theme_bw()+
  scale_x_continuous(labels = scales::dollar_format(suffix = "K",scale = 1e-3))

# patchwork to arrange plots
(medals_population + medals_population2) / (medals_gdp + medals_gdp2) / (medals_gdp_per_cap + medals_gdp_per_cap2)



  