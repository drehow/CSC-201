NA
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
        arrange(
          desc(roi)
        )

head(df4)
df5 <- df4 %>%
        mutate(rank = 1:nrow(df4))
df5 %>% filter(name == 'Oral Roberts University')

# Let's get the percentiles (the higher the better)
round((1-df5$rank[df5$name=='Oral Roberts University']/nrow(df5))*100)
round((1-df5$rank[df5$name=='University of Tulsa']/nrow(df5))*100)
round((1-df5$rank[df5$name=='Yale University']/nrow(df5))*100)
round((1-df5$rank[df5$name=='University of Central Oklahoma']/nrow(df5))*100)
library(ggplot2)

ggplot(df5) + 
  geom_line(aes(x = rank, y=roi))


ggplot(df5) +
  geom_point(aes(x = rank, y = out_of_state_tuition), color = 'red') 


ggplot(df5) + 
  geom_point(aes(x = rank, y = early_career_pay), color = 'blue')

