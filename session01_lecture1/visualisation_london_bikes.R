library(tidyverse)
library(mosaic)
library(here)
library(lubridate)
library(skimr)
library(ggiraph) # interactive graphs

#read the CSV file
bike <- read_csv(here::here("data", "london_bikes.csv"))

# fix dates using lubridate, and generate new variables for year, month, month_name, day, and day_of _week
bike <- bike %>%   
  mutate(
    month = month(date),
    month_name=month(date, label = TRUE)) 

# generate new variable season_name to turn seasons from numbers to Winter, Spring, etc
bike <- bike %>%  
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

# examine what the resulting data frame looks like
glimpse(bike)
skim(bike)



# Time series plot of bikes rented
ggplot(bike, aes(x=date, y=bikes_hired))+
  geom_smooth()+
  geom_point(alpha = 0.3)+
  theme_bw()+
  NULL


#summary statistics
favstats(~ bikes_hired, data= bike)

# if we wanted to get summary statistics by `year`, `day_of_week`,  `month_name`, or `season_name`
# we use mosaic's syntax `Y ~ X` that allows us to facet our analysis of a variable Y by variable X 
# using the syntax `favstats( Y ~ X, data=...)`

favstats(bikes_hired ~ year, data=bike)
favstats(bikes_hired ~ wday, data=bike)
favstats(bikes_hired ~ weekend, data=bike)
favstats(bikes_hired ~ month_name, data=bike)
favstats(bikes_hired ~ season_name, data=bike)


# Histogram of bikes rented
ggplot(bike, aes(x=bikes_hired))+
  geom_histogram()+
  theme_bw()+
  NULL

# Histogram faceted by season_name
ggplot(bike, aes(x=bikes_hired))+
  geom_histogram()+
  facet_wrap(~season_name)+
  theme_bw()+
  NULL

# Histogram faceted by month_name
ggplot(bike, aes(x=bikes_hired))+
  geom_histogram()+
  facet_wrap(~month_name)+
  theme_bw()+
  NULL

# Histogram faceted by month_name in 4 rows
ggplot(bike, aes(x=bikes_hired))+
  geom_histogram()+
  facet_wrap(~month_name, nrow = 4)+
  theme_bw()+
  NULL


# Density plot 
ggplot(bike, aes(x=bikes_hired))+
  geom_density()+
  theme_bw()+
  NULL

# Density plot filled by season_name 
ggplot(bike, aes(x=bikes_hired))+
  geom_density(aes(fill=season_name), alpha = 0.3)+
  theme_bw()+
  NULL

# Density plot filled by season_name, and faceted by season_name
ggplot(bike, aes(x=bikes_hired))+
  geom_density(aes(fill=season_name), alpha = 0.3)+
  facet_wrap(~season_name, nrow = 4)+
  theme_bw()+
  
  #remove legend to the right
  theme(legend.position = "none")+
  NULL

# Density plot filled by season_name, and faceted by month_name
ggplot(bike, aes(x=bikes_hired))+
  geom_density(aes(fill=season_name), alpha = 0.3)+
  facet_wrap(~month_name, nrow = 4)+
  theme_bw()+
  theme(legend.position="none")+
  NULL

#Boxplot of bikes_hired  by month
# since 'month' is a number, it treats it as a continuous variable; hence we get just one box
ggplot(bike, aes(x=month, y= bikes_hired))+
  geom_boxplot()+
  theme_bw()+
  NULL


#Boxplot  by month_name
ggplot(bike, aes(x=month_name, y= bikes_hired))+
  geom_boxplot()+
  theme_bw()+
  NULL


#Boxplot by month_name
ggplot(bike, aes(x=month_name, y= bikes_hired, fill=season_name))+
  geom_boxplot()+
  theme_bw()+
  NULL


#Violin plot by month_name
ggplot(bike, aes(x=month_name, y= bikes_hired))+
  geom_violin()+
  theme_bw()+
  NULL



# bikes_hired vs. weather features
ggplot(bike, aes(x=mean_temp, y= bikes_hired))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  theme_bw()+
  NULL

ggplot(bike, aes(x=mean_temp, y= bikes_hired, colour=season_name))+
  geom_point(alpha = 0.2)+
  geom_smooth(method = "lm", se=FALSE)+
  theme_bw()+
#  facet_wrap(~season_name, ncol=1)+
  NULL



temperature_by_season <- ggplot(bike, aes(x=mean_temp, y= bikes_hired,colour=season_name)) +
  
  # rather than using geom_point(), we use geom_point_interactive()
  geom_point_interactive(aes( 
                             tooltip = glue::glue("Mean Temp: {mean_temp}\nBikes Hired: {bikes_hired}\nDate: {date}")),
                         alpha = 0.3) +
  geom_smooth_interactive(se = FALSE, method = lm)+
  theme_bw()+
  facet_wrap(~season_name, ncol=1)+
#  facet_grid(season_name ~ weekend)+

    theme(legend.position = "none")+
  NULL

# you have created the ggplot object, you now pass it to
girafe(
  ggobj = temperature_by_season
)

######
ggplot(bike, aes(x=humidity, y= bikes_hired))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()+
  NULL

ggplot(bike, aes(x=humidity, y= bikes_hired, colour=season_name))+
  geom_point(alpha=1/4)+
  geom_smooth(method = "lm")+
  theme_bw()+
  facet_wrap(~season_name, ncol=1)+
  NULL

ggplot(bike, aes(x=pressure, y= bikes_hired))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()+
  NULL

# weather features
# https://www.ecad.eu/dailydata/datadictionarycountry.php?43il4bgek4lvi88fdri9d8sn97
