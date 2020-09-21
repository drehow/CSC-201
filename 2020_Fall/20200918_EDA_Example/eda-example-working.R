library(here)
library(ggplot2)
library(dplyr)

setwd('C:\\Users\\drew.howell\\Desktop\\CSC-201\\2020_Fall\\20200918_EDA_Example\\')

p1_power <- read.csv('source_data\\Plant_1_Generation_Data.csv')



p1_power <- p1_power  %>% 
  mutate(Date = as.Date(substr(p1_power$DATE_TIME,1,10), '%d-%m-%Y')) %>%
  select(Date, everything()) %>% 
  select(-c(3:4)) %>%
  mutate(Time = substr(p1_power$DATE_TIME,12,17)) %>%
  select(Date, Time,everything()) %>%
  select(-c(DATE_TIME))

df <- p1_power %>%
  group_by(Date, Time) %>%
  summarise(average = mean(DAILY_YIELD)) 

ggplot(df) + 
  geom_point(aes(x=Time, y=average)) + 
  coord_flip()
