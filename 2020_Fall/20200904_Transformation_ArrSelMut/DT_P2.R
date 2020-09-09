library(readr)
library(dplyr)

# reading in the tuition cost data from github using the code supplied on the README.md file
tuition_cost <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv')
# reading in the salary potential data from github using the code supplied on the README.md file
salary_potential <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/salary_potential.csv')

# we haven't covered joins in detail yet - but just know that this code is putting the two data sets together by the school name column. And because it's an inner join (instead of left, right, outer, etc.), it's dropping rows from either data set that do not appear in the other data set. 
df <- inner_join(tuition_cost, salary_potential)

names(df)
str(df)
summary(df)

summary(df)



# selection by defining what you want to keep
df2 <- df %>%
  select(name, out_of_state_tuition, early_career_pay)
summary(df2)

# selection by defining what you want to get rid of. 
df2a <- df %>% 
  select(-(state:in_state_total), -(out_of_state_total:state_name), -(mid_career_pay:stem_percent))
summary(df2a)



df3 <- df2 %>%
  mutate(
    roi = round((df2$early_career_pay / df2$out_of_state_tuition - 1) * 100,2)
  )

summary(df3)



df4 <- df3 %>%
  arrange(roi)

head(df4)

