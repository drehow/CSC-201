##################################################################################
### PACKAGES ###
##################################################################################
library(rtweet)
library(dplyr)
library(tidyr)
library (tidytext)
library(httpuv)
library(ggplot2)
library(lubridate)
library(chron)

##################################################################################
###TWEET ACCESS ###
##################################################################################

## label key and secret key
api_key <- "7lIgKeBPEe36d1daOhJKQ4I5i"
api_secret_key <- "68UmK5FGxNMoiQdjYirve2TmHoRF0gIPsaYlavXjYRe4c8HyHS"

### Create Token for API access ###
token <- create_token(
  app = "Soederstocks",
  consumer_key = api_key,
  consumer_secret = api_secret_key)

##################################################################################
###uploading TWEET data ###
##################################################################################

## upload aapl tweets
AAPLtweets <- rtweet::search_tweets(q = '$AAPL', 
                                   n = 10000,
                                   since = "2020-11-10",
                                   until = "2020-11-19")

##split Created at into date and time
AAPLtweets$date <- as.Date(AAPLtweets$created_at, format = "%Y / %m / %d")  
##AAPLtweets$time <- chron(AAPLtweets$created_at, format = "%H:%M") ## needs work




##################################################################################
###STOCKS DATA ###
##################################################################################

## import data
AAPLstock <- readr::read_csv('C:/Users/drew.howell/Desktop/AAPL.txt')

## define COL names
names(AAPLstock) <- c("date", "time", "adj_open" , "adj_high", "adj_low", "adj_close" , "volume")

##set date as date
AAPLstock$date <- as.Date(AAPLstock$date, format = "%m / %d / %Y")

##split date to 3 seperate col
AAPLstock <- mutate(AAPLstock,
                 year = as.numeric(format(AAPLstock$date, format = "%Y")),
                 month = as.numeric(format(AAPLstock$date, format = "%m")),
                 day = as.numeric(format(AAPLstock$date, format = "%d")))

## filter to selected time period
AAPLstock <- filter(AAPLstock, year == 2020, month == 11, day >= 10)

## delete date column
AAPLstock <- select(AAPLstock, !date)


## create new columns <- %change, range (variation), and %range (volatility?)
AAPLstock <- mutate(AAPLstock, perc_change = ((adj_close - adj_open)/adj_open),
                    range = (adj_high - adj_low),
                    perc_range = ((adj_high - adj_low)/ adj_open))


