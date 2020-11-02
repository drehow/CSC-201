setwd('/Users/drewhowell/Desktop/CSC-201/2020_Fall/Lab 3')

library(readr)
library(dplyr)

emissions <- read_csv('emissions.csv')
stocks <- read_csv('sp500.csv')

emissions <- emissions %>%
  group_by(YYYYMM) %>%
  summarise(emission_tonnes = sum(Value))

emissions <- emissions[substr(emissions$YYYYMM,5,6) != 13,]

emissions$Date <- as.Date(paste0(as.character(emissions$YYYYMM),'01'), '%Y%m%d')
emissions$YYYYMM <- NULL

stocks$Date <- as.Date(stocks$Date)

df <- inner_join(stocks, emissions)

df <- df[!is.na(df$emission_tonnes),]

df$emission_change <- NA
df$close_change <- NA
for(i in 2:nrow(df)){
  if(df$emission_tonnes[(i-1)] > df$emission_tonnes[i]){ 
    df$emission_change[i] <- 'decrease'
  } else {
    df$emission_change[i] <- 'increase'
  }
  if(df$Close[(i-1)] > df$Close[i]){ 
    df$close_change[i] <- 'decrease'
  } else {
    df$close_change[i] <- 'increase'
  }
}

df$same <- ifelse(df$emission_change==df$close_change, 'Y','N')

ggplot(df) +
  geom_bar(aes(same)) + 
  theme_bw()

ggplot(df) + 
  geom_line(aes(x=Date, y=Close))
ggplot(df) + 
  geom_line(aes(x=Date, y=emission_tonnes))

# df$emission_ma <- NA
# for(i in 12:nrow(df)){
#   df$emission_ma[i] <- mean(df$emission_tonnes[(i-11):(i-1)])
# }
# ggplot(df) + 
#   geom_line(aes(x=Date, y=emission_ma))
