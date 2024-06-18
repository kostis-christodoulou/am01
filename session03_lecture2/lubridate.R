library(lubridate)

date <- ymd("2022-12-25")
today <- Sys.Date() #reads system's current date

year(date)
month(date) # Equal to 12
month(date,label = TRUE) # Equal to "Dec"

day(date)
wday(date,label = TRUE) # Equal to "Wed"

isoweek(date)

date - today 

