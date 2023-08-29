library(tidyverse)
library(rvest)
library(lubridate)
library(countdown)
library(knitr)


# - **rvest** works with webpages that are built server-side and thus requires knowledge of the relevant CSS selectors.


## `rvest` core functions

# | Function            | Description                                                       |
# |---------------------|-------------------------------------------------------------------|
# | `xml2::read_html()` | read HTML from a character string or connection                   |
# | `html_nodes()`      | select specified nodes from the HTML document using CSS selectors |
# | `html_table()`      | parse an HTML table into a data frame                             |
# | `html_text()`       | extract tag pairs' content                                        |
# | `html_name()`       | extract tags' names                                               |
# | `html_attrs()`      | extract all of each tag's attributes                              |
# | `html_attr()`       | extract tags' attribute value by name                             |


## More on CSS selectors



# Selector          |  Example         | Description
# :-----------------|:-----------------|:--------------------------------------------------
# element           |  `p`             | Select all &lt;p&gt; elements
# element element   |  `div p`         | Select all &lt;p&gt; elements inside a &lt;div&gt; element
# element>element   |  `div > p`       | Select all &lt;p&gt; elements with &lt;div&gt; as a parent
# .class            |  `.title`        | Select all elements with class="title"
# `#id`             |  `#name`         | Select all elements with id="name"
# [attribute]       |  `[class]`       | Select all elements with a class attribute
# [attribute=value] |  `[class=title]` | Select all elements with class="title"

# For more CSS selector references click [here](https://www.w3schools.com/cssref/css_selectors.asp).



## Get the page

url <- "https://www.presidency.ucsb.edu/documents/address-the-dedication-the-national-cemetery-gettysburg-pennsylvania-gettysburg-address"
gettysburg_address <- read_html(url)
gettysburg_address



## Find `a` (anchor) elements


html_elements(x = gettysburg_address,
              css = "a")


## Find the CSS selector

# Right click and `inpect` to find the CSS selector for the document's *speaker*.
                  
# with firefox
gettysburg_address %>%
  html_elements(css = ".diet-title > a:nth-child(1)")


# with Chrome
gettysburg_address %>%
  html_elements(css = "#block-system-main > div > div > div.col-sm-8 > div.field-docs-person > div > div.field-title > h3 > a")

                  
## Get attributes and text of elements


gettysburg_address %>%
  html_elements(css = ".diet-title > a:nth-child(1)")

#| label: get-speaker-link
#| code-line-numbers: "3"

speaker_link <- gettysburg_address %>%
  html_elements(css = ".diet-title > a:nth-child(1)") %>%
  html_attr("href")

speaker_link


#| label: get-speaker-text
#| code-line-numbers: "3"

speaker <- gettysburg_address %>%
  html_elements(css = ".diet-title > a:nth-child(1)") %>%
  html_text2() # Select text of element

speaker

                  
## Date of statement and text

#| label = "get-date"
date <- gettysburg_address %>%
  html_elements(css = ".date-display-single") %>%
  html_text2() %>% # Grab element text
  lubridate::mdy() # Format date using lubridate mdy- month, date,year

date


#| label = "get-speaker-2"
text <- gettysburg_address %>%
  html_elements(css = ".field-docs-content") %>%
  html_text2()

text

                  
## Store scraped fields in a data frame{.smaller}

date
speaker
text


# store in a data frame
url_data <- tibble(
  date = date,
  speaker = speaker,
  text = text
)


                  
                  
## `get_address()` to scrape any presidential address{.smaller}

# Scrapes the URL of an individual presidential address
# Returns a dataframe containing the date, speaker link, and full text

get_address <- function(url) {
  
  address <- read_html(url)
  
  date <- address %>%
    html_elements(css = ".date-display-single") %>%
    html_text2() %>% # Grab element text
    lubridate::mdy() # Format date using lubridate mdy- month, date,year
  
  speaker <- address %>%
    html_elements(css = ".diet-title > a:nth-child(1)") %>%
    html_text2() # Select text of element
  
  speaker_link <- address %>%
    html_elements(css = ".diet-title > a:nth-child(1)") %>%
    html_attr("href")
  
  text <- address %>%
    html_elements(css = ".field-docs-content") %>%
    html_text2()
  
  address_df <- tibble(
    date     = date,
    speaker     = speaker,
    speaker_link    = speaker_link,
    text = text
  )
  
  return(address_df)
}

url <-  "https://www.presidency.ucsb.edu/documents/address-before-joint-session-the-congress-the-state-the-union-25"
get_address(url)

                  
## Books to scrape

url <- "http://books.toscrape.com/catalogue/page-1.html"
books_html <- read_html(url)

## Solution

# example for page 1, see how everything works
url <- "http://books.toscrape.com/catalogue/page-1.html"
books_html <- read_html(url)

# just showing code for star-rating
books_html %>% 
  html_nodes(css = ".star-rating") %>% 
  html_attr(name = "class") %>% 
  str_remove(pattern = "star-rating ")


# turn our code into a function
get_books <- function(page) {
  
  base_url <- "http://books.toscrape.com/catalogue/page-"
  url <- str_c(base_url, page, ".html")
  
  books_html <- read_html(url)
  
  ratings <- books_html %>% 
    html_nodes(css = ".star-rating") %>% 
    html_attr(name = "class") %>% 
    str_remove(pattern = "star-rating ")
  
  
  books_df <- tibble(
  #  title     = titles,
  #  price     = prices,
    rating    = ratings,
  #  available = availabilities
  )
  
  return(books_df)
}

# iterate across pages 1-5
pages <- 1:5

# use purrr::map_df(), as we want the output to be a dataframe
books <- map_df(pages, get_books)


# Wikipedia scraping

url <- "http://en.wikipedia.org/wiki/Men%27s_100_metres_world_record_progression"

m100 <- url  %>%
  read_html()

## Table 1: Unofficial progression before the IAAF

# Find the table's unique CSS selector. Confirm it is `table.wikitable:nth-child(11)`
#  Use this unique CSS selector to isolate the pre-IAAF table content from the rest of the HTML document. 
# The core **rvest** function that we'll use to extract the table content is `html_elements()`, before piping it on to `html_table()` to parse the HTML table into an R data frame.
                                                                                                    

pre_iaaf  <- m100 %>%
  html_element("table.wikitable:nth-child(11)") %>% ## select table element
  html_table()                                      ## convert to data frame

pre_iaaf

# Use `janitor::clean_names()` to remove spaces and capital letters from  column names.
# Use `lubridate::mdy()` function to convert string to a date object

pre_iaaf  <-
  pre_iaaf %>%
  janitor::clean_names() %>%          ## fix column names
  mutate(date = lubridate::mdy(date))  ## convert string to date format

pre_iaaf


## Work with tables on a page

## UK cost of living

url  <-  "https://www.numbeo.com/cost-of-living/country_result.jsp?country=United+Kingdom"


# get tables that exist on url
tables <- url %>%
  read_html() %>%
  html_nodes(css="table") %>% # this will isolate all tables on page
  html_table() # Parse an html table into a dataframe

tables


## Tables are inside a `list` of 6 tibbles (dataframes){.smaller}
glimpse(tables)


## Use `tables[[5]]` to get fifth table
tables[[5]] |>
  janitor::clean_names() %>% 
  mutate(rank = row_number())

