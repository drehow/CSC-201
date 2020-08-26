
if (!require(readr)) install.packages('readr')
library(readr)
library(ggplot2)

covidData <- read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv')
covidData_OK <- covidData[covidData$state == 'Oklahoma',]

ggplot(data = covidData_OK) + 
  geom_line(mapping = aes(x = date, y = cases))

covidData_NY <- covidData[covidData$state == 'New York',]

ggplot(data = covidData_NY) + 
  geom_line(mapping = aes(x = date, y = cases))
covidData_yesterday <- covidData[covidData$date == as.character(Sys.Date()-1),]
ggplot(data = covidData_yesterday) + 
  geom_point(mapping = aes(x = cases, y = deaths)) + 
  geom_smooth(mapping = aes(x = cases, y = deaths))

library(dplyr)

covidData_yesterdayNoNY <- covidData_yesterday %>% filter(state != 'New York')

ggplot(data = covidData_yesterdayNoNY) + 
  geom_point(mapping = aes(x = cases, y = deaths)) + 
  geom_smooth(mapping = aes(x = cases, y = deaths))

covidData_yesterday[covidData_yesterday$deaths>30000,]
covidData_yesterday[covidData_yesterday$state=='Oklahoma',]
library(readr)
library(dplyr)
stPops <- read_csv('https://raw.githubusercontent.com/drehow/CSC-201/master/2020_Fall/20200821_Analytics%20with%20Excel_2/nst-est2019-alldata.csv')

stPops <- stPops[-c(1:5,nrow(stPops)),] %>% select(NAME, POPESTIMATE2019)

covidData <- left_join(covidData,stPops, by = c('state'='NAME'))

covidData$per100k <- round(covidData$cases / covidData$POPESTIMATE2019 * 10000,2)
covidData <- covidData[!is.na(covidData$per100k),]

ggplot(data = covidData) + 
  geom_point(mapping = aes(x = date, y = per100k
                          #, group = state, color = state
                          ))

ggplot(data = covidData) + 
  geom_line(mapping = aes(x = date, y = per100k, group = state))
 votes <- read_csv('https://raw.githubusercontent.com/drehow/CSC-201/master/2020_Fall/20200824_Beginning%20with%20R%20...%20Visualizing%20Data/1976-2016-president.csv')

library(scales)
library(tidyr)

votes <- votes %>% 
  filter(year == 2016, writein == F, (candidate == 'Trump, Donald J.' & party =='republican') | (candidate == 'Clinton, Hillary' & party=='democrat')) %>%
  select(state, party, candidatevotes) %>%
  spread(party, candidatevotes, fill=NA) 

votes <- votes %>% 
  mutate(voted = ifelse(votes$democrat > votes$republican,'democrat','republican')) %>% 
  select(state, voted)

covidData <- left_join(covidData, votes)

covidData$population <- covidData$POPESTIMATE2019
covidData$POPESTIMATE2019 <- NULL



ggplot(data = covidData) + 
  geom_line(mapping = aes(x = date, y = per100k, group = state, color = voted))

ggplot(data = covidData) + 
  geom_line(mapping = aes(x = date, y = per100k, group = state
                          , color = voted, alpha = population))

 ggplot(data = covidData) + 
  geom_line(mapping = aes(x = date, y = per100k, group = state
                          , color = voted, alpha = population
                          ))+
   labs(title="COVID Cases by State", subtitle="Data as of 8/25/2020", y="Cases per 100k People", x="Date (2020)", caption="Sources: New York Times, U.S. Census Bureau, Harvard Dataverse") 
   
ggplot(data = covidData) + 
  geom_line(mapping = aes(x = date, y = per100k, group = state
                          , color = voted, alpha = population
                          ))+
   labs(title="COVID Cases by State", subtitle="Data as of 8/25/2020", y="Cases per 100k People", x="Date (2020)", caption="Sources: New York Times, U.S. Census Bureau, Harvard Dataverse") +
    scale_alpha_continuous(labels = comma) 

ggplot(data = covidData) + 
  geom_line(mapping = aes(x = date, y = per100k, group = state
                          , color = voted, alpha = population
                          ))+
  labs(title="COVID Cases by State", subtitle="Data as of 8/25/2020", y="Cases per 100k People", x="Date (2020)", caption="Sources: New York Times, U.S. Census Bureau, Harvard Dataverse") +
  scale_alpha_continuous(labels = comma) +
  scale_x_date(date_breaks = "1 month",
                labels = date_format("%B"),
                limits = as.Date(c('2020-03-15','2020-08-23')))

ggplot(data = covidData) + 
  geom_line(mapping = aes(x = date, y = per100k, group = state
                          , color = voted, alpha = population
                          ))+
  labs(title="COVID Cases by State", subtitle="Data as of 8/25/2020", y="Cases per 100k People", x="Date (2020)", caption="Sources: New York Times, U.S. Census Bureau, Harvard Dataverse") +
  scale_alpha_continuous(labels = comma) +
  scale_x_date(date_breaks = "1 month",
                labels = date_format("%B"),
                limits = as.Date(c('2020-03-15','2020-08-23'))) +
  theme(legend.position = c(0.11, 0.67))

ggplot(data = covidData) + 
  geom_line(mapping = aes(x = date, y = per100k, group = state
                          , color = voted, alpha = population
                          ))+
  labs(title="COVID Cases by State", subtitle="Data as of 8/25/2020", y="Cases per 100k People", x="Date (2020)", caption="Sources: New York Times, U.S. Census Bureau, Harvard Dataverse") +
  scale_alpha_continuous(labels = comma) +
  scale_x_date(date_breaks = "1 month",
                labels = date_format("%B"),
                limits = as.Date(c('2020-03-15','2020-08-23'))) +
  theme_classic() +
  theme(legend.position = c(0.11, 0.67))



ggplot(data = covidData) + 
  geom_line(mapping = aes(x = date, 
                          y = per100k, 
                          group = state
                          , color = voted, 
                          alpha = population
                          )) + 
  scale_alpha_continuous(labels = comma) +
  scale_x_date(date_breaks = "1 month",
                labels = date_format("%B"),
                limits = as.Date(c('2020-03-15','2020-08-23'))) +
  scale_color_manual(values=c('#002fd9','#d90000')) +
  labs(title="COVID Cases by State", 
      subtitle="Data as of 8/25/2020", 
      y="Cases per 100k People", 
      x="Date (2020)", 
      caption="Sources: New York Times, U.S. Census Bureau, Harvard Dataverse") +
  theme_classic() +
  theme(legend.position = c(0.11, 0.67))
  

covidData_party <- covidData %>% 
  group_by(date, voted) %>% 
  summarise(per100k = mean(per100k))

ggplot(data = covidData_party) + 
  geom_line(mapping = aes(x = date, y = per100k, group = voted, color = voted))


ggplot(data = covidData[covidData$date == max(covidData$date),]) + 
  geom_boxplot(mapping = aes(x = voted, y = cases)) 
+ 
  coord_cartesian(ylim = c(0,8000))





