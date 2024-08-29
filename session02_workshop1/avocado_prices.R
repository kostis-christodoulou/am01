# Make sure we have the relevant packages loaded

library(tidyverse)
library(mosaic)
library(here)
library(lubridate)
library(skimr)
library(ggpubr) # to display regression equation

# The data come from https://www.kaggle.com/datasets/timmate/avocado-prices-2020 


# summary of weekly Hass avocado sales for the contiguous US. https://hassavocadoboard.com/ 
# The underlying data are from The Hass Avocado Board (free registration required). 
# Hass Avocados are the most popular variety of avocados sold in the US and the 
# Haas Avocado Board (HAB) provides crucial data on them to growers and marketers.
# 
# The HAB makes this information available to anyone who may be interested 
# (free registration required). An important note to remember is that when they use the 
# term ‘units’, it often means the weight in US pounds. The HAB does not provide 
# (at least publicly) actual piece-count sales to retailers.

# PLU codes
# 4046: small/medium Hass Avocados (~3-5 oz)
# 4225: large Hass Avocados (~8-10 oz)
# 4770: extra large Hass Avocados (~10-15 oz)

#read the CSV file
avocado <- read_csv(here::here("data", "avocado-prices.csv"))

# examine what the data frame looks like
skim(avocado)

# Task 1 ------------------------------------------------------------------


# Get the total volume of avocados sold on each `date`
# this is the weekly sales volume of avocados for the week beginning in that date
avocado |> 
  group_by(date) |> 
  summarise(total = sum(total_volume))


# Task 2 ------------------------------------------------------------------


# Get the total volume sold on each `date`, grouped by `type`, conventional or organic
avocado |> 
  group_by(date) |> 
  summarise(total = sum(total_volume))



# Task 3 ------------------------------------------------------------------

  
# Get the total volume sold on each `date`, grouped by `type`, conventional or organic
# for each `date` what was the % split of conventional vs organic acovados
avocado |>

  

# Task 4- Histogram of prices ---------------------------------------------

# Price of conventional vs. organic avocados
# In this exercise, you'll create  histograms to compare the prices of conventional and organic avocados.
avocado |> 
  ggplot()+
  aes(x=average_price, fill=type)+
  geom_histogram(alpha = 0.5)+
  theme_bw()
  


# Task 5- Table of price vs volume ------------------------------------


# Scatter plot of avg_price vs. total_volume - 
# Get a table with total volume of avocados and average_price for each `date`

avocado |>


# Task 6- scatter plot price vs volume ------------------------------------

  
# Scatter plot of avg_price vs. total_volume - 
# for the different `type` conventional vs organic
# Is there price elasticity of demand?
# Get the total volume of avocados and average_price for each `date` and `type`

# Load the avocado dataset and start the data manipulation pipeline
avocado |>
  # Group the data by 'date' and 'type' of avocado
  group_by(date, type) |>
  
  # Summarize the grouped data to calculate total volume and mean price
  summarise(total_volume = sum(total_volume),  # Total volume of avocados sold
            mean_price = mean(average_price)) |>  # Average price of avocados sold
  
  # Initialize a ggplot object for visualization
  ggplot() +
  
  # Set aesthetic mappings for the plot
  aes(x = total_volume, 
      y = mean_price, 
      colour = type) +  # Map total volume to x-axis, mean price to y-axis, and color by type
  
  # Add points to the plot with some transparency
  geom_point(alpha = 0.5) +
  
  # Customize x-axis to show numbers and y-axis to show dollar amounts
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::dollar) +
  
  # Add a linear model trend line without confidence intervals
  geom_smooth(method = "lm", se = FALSE) +
  
  # Create separate panels for each type of avocado, allowing for independent scaling
  facet_wrap(~type, scales = "free") +
  
  # Apply a clean black-and-white theme to the plot
  theme_bw() +
  
  # Remove the legend from the plot for clarity
  theme(legend.position = "none") +
  
  # End of the pipeline
  NULL


# Task 7 - Mean price by geography and type -------------------------------
avocado |> 


# Task 8 - Time series plot of mean prices --------------------------------

  
# Time series plot of mean prices
# Load the avocado dataset and start the data manipulation pipeline
avocado |> 
  # Group the data by 'date', 'type', and 'geography'
  group_by(date, type, geography) |> 
  
  # Summarize the grouped data to calculate the mean price of avocados
  summarise(mean_price = mean(average_price)) |> 
  
  # Initialize a ggplot object for visualization
  ggplot() +
  
  # Set aesthetic mappings for the plot
  aes(x = date, 
      y = mean_price, 
      colour = type) +  # Map date to x-axis, mean price to y-axis, and color by type
  
  # Add lines to the plot to represent the mean price over time
  geom_line() +
  
  # Create separate panels for each type of avocado, arranged in 2 columns
  facet_wrap(~type, 
             ncol = 2) +
  
  # Apply a clean black-and-white theme to the plot
  theme_bw() +
  
  # Remove the legend from the plot for clarity
  theme(legend.position = "none") +
  
  # Customize y-axis to show dollar amounts for mean price
  scale_y_continuous(labels = scales::dollar) +
  
  # End of the pipeline
  NULL
  

# Task 9 - Total volume by geography and type -----------------------------
avocado |> 

## Time series plot of total_volume
avocado |> 
  group_by(date, type) |> 
  summarise(total = sum(total_volume)) |> 
  ggplot()+
  aes(x=date, y = total, fill=type)+
  
  # Stacked bar chart
  geom_bar(stat="identity", 
           
           #so all bars add up to 100% for every year
           position = "fill",
           
           # set width = 1 to remove all space between bars
           width = 1)


# Task 10- Which geography generates most sales ---------------------------

  
# Which geography creates most sales  
# Does it matter how you group_by()
  

avocado |> 
  # Filter out rows where geography is "Total U.S." to focus on regions
  filter(geography != "Total U.S.") |> 
  
  # Group the data by 'geography' and 'type' of avocado
  group_by(geography, type) |> 
  
  # Summarize the grouped data to calculate the total volume of avocados sold
  summarise(total_volume = sum(total_volume)) |> 
  
  # Calculate the percentage of total volume for each group relative to the overall total
  mutate(percent = total_volume / sum(total_volume)) |> 
  
  # Arrange the results in descending order based on total volume
  arrange(desc(total_volume))

# what does the 0.963 for West and 0.98 for South Central mean?
  
avocado |> 
  filter(geography != "Total U.S.") |> 
  group_by(date, type, geography) |> 
  summarise(total_volume = sum(total_volume)) |> 
  mutate(percent = total_volume / sum(total_volume)) |> 
  arrange(desc(total_volume)) |> 
  
  # calc cmulative percent of sales
  mutate(cumulative_percent = cumsum(percent)) |> 

# what does the 0.108 for West and 0.106 for South Central mean?  
ggplot()+
  aes(x=date, y = total_volume, colour = type) |> 
  geom_line()+
  facet_wrap(~type, ncol = 2)+
  theme(legend.position = "none")
theme_bw()+
  NULL  


# Task 11 - Price elasticity of demand ---------------------------------------------------------

  
# Price elasticity of demand  
  # Load the avocado dataset and start the data manipulation pipeline
  avocado |>
    # Group the data by 'date' and 'type' of avocado
    group_by(date, type) |>
    
    # Summarize the grouped data to calculate total volume and mean price
    summarise(total_volume = sum(total_volume),  # Total volume of avocados sold
              mean_price = mean(average_price)) |>  # Average price of avocados sold
    
    # Group the summarized data by 'type' for further calculations
    group_by(type) |>
    
    # Calculate the percentage change in total volume and mean price compared to the previous observation
    mutate(delta_volume = total_volume / lag(total_volume, 1),  # Calculate the ratio of current to previous total volume
           delta_price = mean_price / lag(mean_price, 1)  # Calculate the ratio of current to previous mean price
    ) |> 
    
    # Initialize a ggplot object for visualization
    ggplot() +
    
    # Set aesthetic mappings for the plot
    # Map delta volume to x-axis, delta price to y-axis, and color by type
    aes(x = delta_volume, y = delta_price, colour = type) +  
    
    # Add points to the plot to represent the data
    geom_point() +
    
    # Customize x-axis to show percentage labels
    scale_x_continuous(labels = scales::percent) +
    
    # Customize y-axis to show percentage labels
    scale_y_continuous(labels = scales::percent) +
    
    # Add a linear model trend line without confidence intervals
    geom_smooth(method = "lm", se = FALSE) +
    
    # Add regression line equation to the plot
    stat_regline_equation() +
    
    # Create separate panels for each type of avocado, allowing for independent scaling
    facet_wrap(~type, scales = "free") +
    
    # Apply a clean black-and-white theme to the plot
    theme_bw() +
    
    # Remove the legend from the plot for clarity
    theme(legend.position = "none") +
    
    # End of the pipeline
    NULL


# Task 12 - Cumulative change of price by type ----------------------------

  
#cumulative change of price for each avocado type
  avocado |>
    group_by( type, date) |>
    summarise(mean_price = mean(average_price)) |> 
    mutate(cum_change = mean_price/first(mean_price)) |> 
    ggplot()+
    aes(x=date, y = cum_change, colour=type)+
    geom_line()+
    theme_bw()  
  

# Task 13 - Prices in top geographies over time ---------------------------

  
# Find geographies that account for more than 4% of  sales
# Plot evolution of mean price by type of avocado for these geographies
avocado |> 
    filter(geography != "Total U.S.") |> 
    group_by(geography) |> 
    summarise(total = sum(total_volume)) |> 
    mutate(perc = total/sum(total)) |> 
    arrange(desc(perc)) |> 
    filter(perc > 0.04) |> 
    select(geography) |> 
 
    left_join(avocado) |> 
    group_by(date, type, geography) |> 
    summarise(mean_price = mean(average_price)) |> 
    ggplot()+
    aes(x=date, y = mean_price, colour = type) |> 
    geom_line()+
    facet_grid(type~geography)+
    scale_y_continuous(labels= scales::dollar)+
    theme_bw()+
    theme(legend.position = "none")+
    labs(x=NULL, y=NULL)+
    NULL
  
  