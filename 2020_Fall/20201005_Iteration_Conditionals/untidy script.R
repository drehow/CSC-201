if(!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

tdf_winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')
tdf_winners <- tdf_winners[!is.na(tdf_winners$time_overall),]

df <- tdf_winners

distance_unit <- NA
df <- add_column(df, distance_unit, .after = 5)
for (i in seq(1,nrow(df),3)){
  df$distance[i] <- df$distance[i]*0.621371
  df$distance_unit[i] <- 'MI'
}
df$distance_unit <- ifelse(is.na(df$distance_unit), 'KM', 'MI')

time_unit <- NA
df <- add_column(df, time_unit, .after = 8)
for (i in seq(1,nrow(df),5)){
  df$time_overall[i] <- df$time_overall[i]*3600
  df$time_margin[i] <- df$time_margin[i]*3600
  df$time_unit[i] <- 'sec'
}
df$time_unit <- ifelse(is.na(df$time_unit), 'hr', 'sec')


library(stringr)

for(i in seq(1,nrow(df),11)){
  num <- unlist(str_locate_all(pattern =' ', df$winner_name[i])[1])
  df$winner_name[i] <- paste0(substr(df$winner_name[i],(num+1), nchar(df$winner_name[i])), ', ',
                              substr(df$winner_name[i],1,(num-1)))
}


write_csv(df, 'C:/Users/drew.howell/Desktop/CSC-201/2020_Fall/20201005_Iteration_Conditionals/tdf_winners_new.csv')

# PLOT COUNTS OF WINNERS THAT WON BY 100 METERS OR LESS (CALCULATED BY AVERAGE PACE) VS GREATER THAN 100 METERS
