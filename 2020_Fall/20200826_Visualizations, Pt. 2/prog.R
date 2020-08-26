# loading initial packages I know I'm going to be using
if (!require(readr)) install.packages('readr')
library(readr)
library(ggplot2)

# reading in covid data
covidData <- read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv')

# making OK specific covid data
covidData_OK <- covidData[covidData$state == 'Oklahoma',]

# initial cases graph
ggplot(data = covidData_OK) + 
  geom_line(mapping = aes(x = date, y = cases))

# NY specific covid data
covidData_NY <- covidData[covidData$state == 'New York',]

# NY cases graph
ggplot(data = covidData_NY) + 
  geom_line(mapping = aes(x = date, y = cases))

# making yesterday snapshot
covidData_yesterday <- covidData[covidData$date == as.character(Sys.Date()-1),]

# adding trend line
ggplot(data = covidData_yesterday) + 
  geom_point(mapping = aes(x = cases, y = deaths)) + 
  geom_smooth(mapping = aes(x = cases, y = deaths))

# needed to load this package to filter()
library(dplyr)

# filtering NY out of yesterday snapshot
covidData_yesterdayNoNY <- covidData_yesterday %>% filter(state != 'New York')

# same plot as before but without Ny
ggplot(data = covidData_yesterdayNoNY) + 
  geom_point(mapping = aes(x = cases, y = deaths)) + 
  geom_smooth(mapping = aes(x = cases, y = deaths))

# finding which state was so high in deaths
covidData_yesterday[covidData_yesterday$deaths>30000,]

# looking at OK
covidData_yesterday[covidData_yesterday$state=='Oklahoma',]

# getting state populations data
stPops <- read_csv('https://raw.githubusercontent.com/drehow/CSC-201/master/2020_Fall/20200821_Analytics%20with%20Excel_2/nst-est2019-alldata.csv')

# filtering out rows and columns we don't need
stPops <- stPops[-c(1:5,nrow(stPops)),] %>% select(NAME, POPESTIMATE2019)

# joining to main dataset
covidData <- left_join(covidData,stPops, by = c('state'='NAME'))

# calculating a concentrations column
covidData$per100k <- round(covidData$cases / covidData$POPESTIMATE2019 * 10000,2)

# getting rid of rows where per100k column is NA 
covidData <- covidData[!is.na(covidData$per100k),]

# starting to build my final plot
ggplot(data = covidData) + 
  geom_point(mapping = aes(x = date, y = per100k
                          #, group = state, color = state
                          ))

# changing to lines
ggplot(data = covidData) + 
  geom_line(mapping = aes(x = date, y = per100k
                          #, group = state)
                          
                          ))


# reading in voting data
 votes <- read_csv('https://raw.githubusercontent.com/drehow/CSC-201/master/2020_Fall/20200824_Beginning%20with%20R%20...%20Visualizing%20Data/1976-2016-president.csv')

 
# needed some functions from these libraries based on examples I found online
library(scales)
library(tidyr)

 # cleaning votes data set
votes <- votes %>% 
  filter(year == 2016, writein == F, (candidate == 'Trump, Donald J.' & party =='republican') | (candidate == 'Clinton, Hillary' & party=='democrat')) %>%
  select(state, party, candidatevotes) %>%
  spread(party, candidatevotes, fill=NA) 

# setting to binary dem/rep column
votes <- votes %>% 
  mutate(voted = ifelse(votes$democrat > votes$republican,'democrat','republican')) %>% 
  select(state, voted)

# joining to main dataset
covidData <- left_join(covidData, votes)

# changing to better column name
covidData$population <- covidData$POPESTIMATE2019
covidData$POPESTIMATE2019 <- NULL


# back to the plots
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
  

# extra plots just for exploring the data

covidData_party <- covidData %>% 
  group_by(date, voted) %>% 
  summarise(per100k = mean(per100k))

ggplot(data = covidData_party) + 
  geom_line(mapping = aes(x = date, y = per100k, group = voted, color = voted))


ggplot(data = covidData[covidData$date == max(covidData$date),]) + 
  geom_boxplot(mapping = aes(x = voted, y = cases)) 
+ 
  coord_cartesian(ylim = c(0,8000))





