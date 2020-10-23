library(readr)

setwd('C:/Users/drew.howell/Desktop/CSC-201/2020_Fall/20201019_Linear_Models/')

education <- read_csv('education_county.csv')
voted <- read_csv('voted_county.csv')
pops <- read_csv('populations_county.csv')

voted$ID <- paste(voted$county, voted$state_po)
voted <- voted[voted$year == max(voted$year),]
voted$version <- NULL

ids <- unique(voted$ID)


i=1
for (i in 1:length(ids)){
  temp <- voted[voted$ID == ids[i],]
  if(!F %in% is.na(temp$candidatevotes)){
    next
  }
  temp$spectrum <- -as.numeric(na.omit((temp$candidatevotes[temp$party=='democrat']/temp$totalvotes[temp$party=='democrat'] *10))) + 
    as.numeric(na.omit((temp$candidatevotes[temp$party=='republican']/temp$totalvotes[temp$party=='republican'] *10)))
  x <- temp[temp$candidatevotes == max(temp$candidatevotes),]
  
  if(i==1){
    clean_voted <- x
  } else {
    clean_voted <- rbind(clean_voted, x)
  }
}

education <- education[!is.na(education$`2003 Rural-urban Continuum Code`),]
education$ed_spectrum <- -(2*education$`Percent of adults with less than a high school diploma, 2014-18` + 
  education$`Percent of adults with a high school diploma only, 2014-18`) + 
  education$`Percent of adults completing some college or associate's degree, 2014-18` +
  education$`Percent of adults with a bachelor's degree or higher, 2014-18`*2

library(dplyr)
education <- select(education, State, `Area name`, ed_spectrum)
education <- education[-1,]  
education$`Area name` <- gsub(' County','', education$`Area name`)  
education$ID <- paste(education$`Area name`, education$State)
education$State <- education$`Area name` <- NULL

df <- inner_join(clean_voted, education, by = c('ID' = 'ID'))

pops$CTYNAME <- gsub(' County','', pops$CTYNAME) 
for(i in 1:nrow(pops)){
  pops$STNAME[i] <- ifelse(length(state.abb[grep(pops$STNAME[i], state.name)])==0, NA, state.abb[grep(pops$STNAME[i], state.name)])
}
pops$ID <- paste(pops$CTYNAME, pops$STNAME)

pops <- select(pops, ID, POPESTIMATE2019)
pops <- pops[!duplicated(pops$ID),]

df <- inner_join(df, pops)

df$turnout <- df$totalvotes / df$POPESTIMATE2019
df <- df[df$turnout < 1,]

model <- lm(spectrum ~ turnout + ed_spectrum, df)
model_a <- lm(spectrum ~ turnout, df)
model_b <- lm(spectrum ~ ed_spectrum, df)
library(jtools)
summ(model)

summ(model_a)
summ(model_b)

normalize <- function(x){
  return((x - min(x))/(max(x) - min(x)))
}

df$ed_spectrum <- normalize(df$ed_spectrum)
df$turnout <- normalize(df$turnout)

model <- lm(spectrum ~ turnout + ed_spectrum, df)
model_a <- lm(spectrum ~ turnout, df)
model_b <- lm(spectrum ~ ed_spectrum, df)
library(jtools)
summ(model)

summ(model_a)
summ(model_b)

library(ggplot2)
ggplot(df, aes(turnout, spectrum)) + 
  geom_point() + 
  geom_smooth(method='lm')

ggplot(df, aes(ed_spectrum, spectrum)) + 
  geom_point() + 
  geom_smooth(method='lm')


######
######



df$ed_percentile <- percent_rank(df$ed_spectrum)
df$ed_rank <- ifelse(df$ed_percentile < 0.25, 'Q1', 
                     ifelse(df$ed_percentile >=0.25 & df$ed_percentile < 0.5, 'Q2',
                            ifelse(df$ed_percentile >=0.5 & df$ed_percentile < 0.75, 'Q3','Q4')))

ggplot(df) + 
  geom_bar(aes(df$ed_rank))

df$edRank_Q1 <- ifelse(df$ed_rank == 'Q1',1,0)
df$edRank_Q2 <- ifelse(df$ed_rank == 'Q2',1,0)
df$edRank_Q3 <- ifelse(df$ed_rank == 'Q3',1,0)
df$edRank_Q4 <- ifelse(df$ed_rank == 'Q4',1,0)

model <- lm(spectrum ~ 
              # edRank_Q1 +
              edRank_Q2 +
              edRank_Q3 +
              edRank_Q4
            , df)


model <- lm(spectrum ~ turnout + ed_rank, df)
model_a <- lm(spectrum ~ turnout, df)
model_b <- lm(spectrum ~ ed_rank, df)


summ(model)
ols_coll_diag(model)
# install.packages('olsrr')
library(olsrr)

vif(model)

ggplot(df, aes(x=edRank_Q4, y=spectrum)) + 
  geom_point( alpha = 1/50) + 
  geom_smooth(method = 'lm') + 
  theme_bw()

summ(model_b)
