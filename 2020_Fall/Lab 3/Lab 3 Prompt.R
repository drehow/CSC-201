############################
############################
################# Lab 3
############################
############################

### This lab focuses on cleaning data in preparation for analysis. Specifically, you'll need to make use of joins, loops, and conditionals.
### 
### There are two datasets:
###   American emissions by month and industrial sector
###   Price metrics regarding the S&P500 by day 
### 
### The goal is to reach a conclusion about whether monthly changes in US stock prices correlate with monthly changes in carbon emissions. 
### Your code should join and clean the data, and produce a simple bar chart that compares the number of times the two metrics changed in the same direction to the number of times they did not. 
### 
### I've laid out the steps that need to happen to get to the bar chart, but you have to write the code that performs those steps. 
### 
### You will only need to submit this R file for grading

# 1 - Read in the data (already provided)
library(readr)

emissions <- read_csv('https://raw.githubusercontent.com/drehow/CSC-201/master/2020_Fall/Lab%203/emissions.csv')
stocks <- read_csv('https://raw.githubusercontent.com/drehow/CSC-201/master/2020_Fall/Lab%203/sp500.csv')

# 2 - Group emissions by the YYYYMM column and summarise the Value column using the sum() function, so you get the total emissions for that month across all sectors


# 3 - get rid of the dates that have a MM = 13 (already provided)
emissions <- emissions[substr(emissions$YYYYMM,5,6) != 13,]

# 4 - convert the YYYYMM column to a date type
# hint: https://stackoverflow.com/questions/41327852/convert-a-yyyymm-format-to-date-in-r


# 5 - convert the stocks Date column to a date type (might already be a Date)


# 6 - INNER join the two data sets on the date columns


# 7 - drop the rows where the values are NA


# 8 - create columns for emission changes and Close price changes (the Close column)
#   For example: 
#   
#     df$emission_change <- NA
#     for(i in 2:nrow(df)){
#       if(df$emission_total[(i-1)] > df$emission_total[i]){ 
#         df$emission_change[i] <- 'decrease'
#       } else {
#         df$emission_change[i] <- 'increase'
#       }
#     } 
#       
#     And likewise with the Close price change column
#                  


# 9 - create a column that denotes whether the emissions change and Close change columns are equal or not


# 10 - create a bar plot for the column you made in step 9


# EXTRA CREDIT - plot both the emissions and close price columns and explain in a comment in this script why you think they are or are not correlated
