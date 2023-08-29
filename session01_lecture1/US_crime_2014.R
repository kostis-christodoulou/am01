library(tidyverse)
library(mosaic)
library(janitor)
library(here)
library(maps)
library(ggrepel)


us_crime <- read_csv(here::here("Data", "CrimeOneYearofData.csv")) %>% 
  filter(State != "District of Columbia") %>% 
  clean_names() %>% 
  mutate(state = str_to_lower(state))

favstats(~violent_crime_total, data=us_crime)

#Histogram of Violent Crime Total
ggplot(us_crime, aes(x=violent_crime_total))+
  geom_histogram()+
  geom_vline(xintercept = median(us_crime$violent_crime_total), colour='blue', size = 2)+ #add a vertical line at the median value
  geom_vline(xintercept = mean(us_crime$violent_crime_total), colour='red', size = 2)+ #add a vertical line at the mean value
  labs(title="Crime in the USA (2014)", 
       x ="Total number of violent crimes", 
       caption = "Source: FBI Uniform Crime Reporting, https://www.fbi.gov/services/cjis/ucr/")+
  theme_bw()+
  annotate("text", x = 12000 , y = 9, label = "Median", colour = "blue")+
  annotate("text", x = 28000 , y = 8.175, label = "Mean", colour = "red")+
  NULL

  

ggplot(us_crime, aes(x=violent_crime_total))+
  geom_density()+
  geom_vline(xintercept = median(us_crime$violent_crime_total), colour='blue', size = 2)+ #add a vertical line at the median value
  geom_vline(xintercept = mean(us_crime$violent_crime_total), colour='red', size = 2)+ #add a vertical line at the mean value
  labs(title="Crime in the USA (2014)", 
       x ="Total number of violent crimes", 
       caption = "Source: FBI Uniform Crime Reporting, https://www.fbi.gov/services/cjis/ucr/")+
  theme_bw()+
  NULL




#which are the top violent states?
us_crime %>% 
  arrange(desc(violent_crime_total))

#plot crimes vs population scatterplot
ggplot(us_crime, aes(x=population, y=violent_crime_total))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE)+
  geom_hline(yintercept = mean(us_crime$violent_crime_total), colour='red', size = 2)+ #add a line at the mean value
  geom_text_repel(data = subset(us_crime, violent_crime_total> 30000), #show those states with crime>30K
                  mapping = aes(label = state))+
  labs(title="Crime in the USA (2014)", 
       x="Population",
       y ="Total number of violent crimes", 
       caption = "Source: FBI Uniform Crime Reporting, https://www.fbi.gov/services/cjis/ucr/")+
  theme_bw()
  


#calculate violent per-capita violent crime rate (per 100K population)
us_crime <- us_crime %>%
  mutate(violent_crime_rate = 100000 * violent_crime_total / population)

favstats(~violent_crime_rate, data=us_crime)

us_crime %>% 
  select(state, population, violent_crime_total, violent_crime_rate) %>% 
  arrange(desc(violent_crime_rate)) %>% 
  View()


#Histogram of Violent Crime RATE
ggplot(us_crime, aes(x=violent_crime_rate))+
  geom_histogram()+
  geom_vline(xintercept = median(us_crime$violent_crime_rate), colour='blue', size = 2)+ #add a vertical line at the median value
  geom_vline(xintercept = mean(us_crime$violent_crime_rate), colour='red', size = 2)+ #add a vertical line at the mean value
    labs(title="Crime in the USA (2014)", 
       x ="Rate of violent crimes per 100K", 
       caption = "Source: FBI Uniform Crime Reporting, https://www.fbi.gov/services/cjis/ucr/")+
  theme_bw()+
  NULL


ggplot(us_crime, aes(x=violent_crime_rate))+
  geom_density()+
  geom_vline(xintercept = median(us_crime$violent_crime_rate), colour='blue', size = 2)+ #add a vertical line at the median value
  geom_vline(xintercept = mean(us_crime$violent_crime_rate), colour='red', size = 2)+ #add a vertical line at the mean value
  labs(title="Crime in the USA (2014)", 
       x ="Total number of violent crimes", 
       caption = "Source: FBI Uniform Crime Reporting, https://www.fbi.gov/services/cjis/ucr/")+
  theme_bw()+
  NULL


#plot crimes vs population scatterplot
ggplot(us_crime, aes(x=population, y=violent_crime_rate))+
  geom_point()+
  # geom_smooth(method='lm', se=FALSE)+
  geom_hline(yintercept = mean(us_crime$violent_crime_rate), colour='red', size = 2)+ #add a vertical line at the mean value
  geom_text_repel(data = subset(us_crime, violent_crime_rate> 400),
                  mapping = aes(label = state))+
  labs(title="Crime in the USA (2014)", 
       x="Population",
       y ="Violent crime rate per 100K", 
       caption = "Source: FBI Uniform Crime Reporting, https://www.fbi.gov/services/cjis/ucr/")+
  theme_bw()



#Histogram of Violent Crime rate
ggplot(us_crime, aes(x=violent_crime_rate))+
  geom_density()

#calculate Z scores for violent per-capita violent crime rate (per 100K population)
us_crime <- us_crime %>%
  mutate(
  violent_crime_rate_Z = 
    (violent_crime_rate - mean(violent_crime_rate)) / 
    sd(violent_crime_rate)
)
# mapping crime, as asbolute number, as rater per 100K, and as a Z 

#prepare data for mapping
us_states <- map_data("state")
us_crime_map <- left_join(us_states, us_crime, by=c("region" = "state"))


#plotting the map with totals
p0 <- ggplot(data = us_crime_map,
             mapping = aes(x = long, y = lat,
                           group = group, fill = violent_crime_total))

p1 <-  p0 + geom_polygon(color = "gray90", size = 0.1 ) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(title=" Crime in the US, 2014", fill="Number of violent crimes", caption = "Source: FBI Uniform Crime Reporting, https://www.fbi.gov/services/cjis/ucr/")+
  theme_map()+
  NULL

# playing around with different scales
p2 <-   p1+ scale_fill_gradient(low = "white", high = "#CB454A") 

p2


#plotting the map with crime rate
p0 <- ggplot(data = us_crime_map,
             mapping = aes(x = long, y = lat,
                           group = group, fill = violent_crime_rate))

p1 <-  p0 + geom_polygon(color = "gray90", size = 0.1 ) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(title=" Crime in the US, 2014", fill="Rate per 100K population", caption = "Source: FBI Uniform Crime Reporting, https://www.fbi.gov/services/cjis/ucr/")+
  theme_map()+
  NULL

p2 <-   p1+ scale_fill_gradient(low = "white", high = "#CB454A") 


p2

#plotting the map with Z
p0 <- ggplot(data = us_crime_map,
             mapping = aes(x = long, y = lat,
                           group = group, fill = violent_crime_rate_Z))

p1 <-  p0 + geom_polygon(color = "gray90", size = 0.1 ) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(title=" Crime in the US, 2014", fill= "Standradised (Z) rate per 100K ", caption = "Source: FBI Uniform Crime Reporting, https://www.fbi.gov/services/cjis/ucr/")+
  theme_map()+
  NULL

p2 <-   p1+ scale_fill_gradient(low = "white", high = "#CB454A") 

p2





