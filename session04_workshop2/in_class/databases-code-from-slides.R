library(tidyverse)
library(rvest)
library(lubridate)
library(countdown)
library(dbplyr)
library(DBI)
library(duckdb)
library(arrow)
library(nycflights13)


## Databases and R

# - Virtually every database makes use of [**SQL**](https://en.wikipedia.org/wiki/SQL) (**S**tructured **Q**uery **L**anguage ). 
# - SQL is  extremely powerful and has become something of prerequisite for many data science jobs.
# - However, it is also an archaic language that is much less intuitive than the R `dplyr` tools
# - The good news: You already have all the programming skills you need to start working with databases. This is because the tidyverse --- through **dplyr** --- allows for direct communication with databases from your local R environment.
# - You can interact with the vast datasets stored in relational databases using the *same* tidyverse verbs and syntax that we already know, thanks to the **dbplyr** package


## `SQL` commands vs `dplyr` verbs 

# `dplyr` was inspired by SQL, so using `dplyr` allows you to replicate a lot of what SQL does. 

# +-----------------------------------------+-----------------------------------------+
#   |        `SQL` command...                 |          ... translate to `dplyr`  verb |
#   +=========================================+=========================================+
#   | SELECT                                  | `select()`  for columns                 |
#   |                                         |                                         |
#   |                                         | `mutate()`  for expressions             |
#   |                                         |                                         |
#   |                                         | `summarise()`  for aggregates           |
#   +-----------------------------------------+-----------------------------------------+
#   | FROM                                    | which dataframe to use                  |
#   +-----------------------------------------+-----------------------------------------+
#   | WHERE                                   | `filter()`                              |
#   +-----------------------------------------+-----------------------------------------+
#   | GROUP_BY                                | `group_by()`                            |
#   +-----------------------------------------+-----------------------------------------+
#   | ORDER_BY                                | `arrange()`                             |
#   +-----------------------------------------+-----------------------------------------+
#   | LIMIT                                   | `head()`                                |
#   +-----------------------------------------+-----------------------------------------+
  




## Establish a connection with the database




#In our case,  we open an (empty) database connection via the `DBI::dbConnect()`
# We are calling the **duckdb** package in the background for the DuckDB backend 
# We are also telling R that this is a local connection that exists in memory.


library(dplyr)        
library(nycflights13) 
library(duckdb)

connection <- DBI::dbConnect(duckdb::duckdb(), # database backend, i.e. `duckdb::duckdb()`
                             path = ":memory:") # creates an in-memory database


## Populate our database with `flights`

copy_to(
  dest = connection, 
  df = nycflights13::flights, 
  name = "flights",
  temporary = FALSE, 
  indexes = list(
    c("year", "month", "day"), 
    "carrier", "tailnum", "dest")
)


## Database objects or tibbles?

# - An SQL database will typically contain multiple *tables*. 
# - You can think of these *tables* as R data frames (or tibbles). 
# - We can browse the tables in the database using `DBI::dbListTables()`


DBI::dbListTables(connection)


## Reference the table from R
flights_db <-  dplyr::tbl(connection, "flights")



# These tables are SQL database objects in your R session which you can manipulate in the same way as a dataframe. 

 
class(flights_db)
 

## Generating queries


## Filter according to some condition
flights_db %>% 
  filter(dep_delay > 120) 


# Get mean departure delay for each origin- destination, 
# sorted in descending order. 
mean_delay_db <- flights_db %>%
  group_by(origin, dest) %>%
  summarise(mean_dep_delay = mean(dep_delay)) %>%
  arrange(desc(mean_dep_delay)) %>%
  filter(!is.na(mean_dep_delay))

class(mean_delay_db)


## Laziness as a virtue

tailnum_delay_db <-  flights_db %>% 
  group_by(tailnum) %>%
  summarise(
    mean_dep_delay = mean(dep_delay),
    mean_arr_delay = mean(arr_delay),
    n = n()) %>%
  filter(n > 100) %>% 
  arrange(desc(mean_arr_delay))


# - This sequence of operations never touches the database. When you actually ask for the data, **dplyr** generates the SQL and requests the results from the database
# - It still tries to do as little as possible and only displays a few rows


tailnum_delay_db



## `collect()` the data into your local R environment

tailnum_delay_df  <-  
  tailnum_delay_db %>% 
  collect()

tailnum_delay_df


## Use local dataframe to plot

tailnum_delay_df %>%
  ggplot(aes(x = mean_dep_delay, y=mean_arr_delay, size=n)) +
  geom_point(alpha=0.20) +
  geom_abline(intercept = 0, 
              slope = 1, 
              size = 1.5, 
              colour="tomato") +
  theme_light()+
  labs(
    title = "Most planes manage to make up time even if they depart late",
    x = "Mean departure delay (minutes)",
    y = "Mean arrival delay (minutes)",
    size = 'Number of flights'
  )

  

## Joins
  
# **dplyr**'s collection of joining functions (left_join(), inner_join(), etc) are based on SQL .

## Copy over the "planes" dataset to the same DuckDB connection.
copy_to(
    dest = connection, 
    df = nycflights13::planes, 
    name = "planes",
    temporary = FALSE, 
    indexes = "tailnum")

## List tables in our "con" database connection (i.e. now "flights" and "planes")
dbListTables(connection)

## Reference from dplyr
planes_db <-  dplyr::tbl(connection, 'planes')

## Run a left join 
left_join(
    flights_db,
    planes_db %>% rename(year_built = year),
    by = "tailnum" ## Be specific about the key, or joining column
    ) %>%
    select(year, month, day, dep_time, arr_time, carrier, flight, tailnum,
           year_built, type, model) 

## Translate to SQL with `dplyr::show_query()`    
#  **dplyr** translates your R code into SQL


tailnum_delay_db %>% 
  show_query()