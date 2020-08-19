# for read_delim()
library(readr)
# for gather()
library(tidyr)
# for %>%
library(dplyr)
# for visualizations
library(ggplot2)
# for random forest modeling
library(randomForest)
# for naive Bayes and SVM models
library(e1071)
# for confusion matrix and model stats
library(caret)

# reading in data
doc <- readLines('C:/Users/drew.howell/Downloads/proj1_data.txt')
doc[1:10]

# getting rid of quotes, which keep it from being read correctly
doc <- gsub('"','',doc[1:length(doc)])

# read in as clean data
df <- read_delim(doc, col_names = F, delim = ',')

# setting col names
names(df) <- c(
'age'
,'workclass'
,'fnlwgt'
,'education'
,'educationNum'
,'maritalStatus'
,'occupation'
,'relationship'
,'race'
,'sex'
,'capitalGain'
,'capitalLoss'
,'hoursPerWeek'
,'nativeCountry'
,'label')

# check types
str(df)

# setting numeric columns
cols.num <- c("fnlwgt","educationNum",'capitalGain','capitalLoss','hoursPerWeek')
df[cols.num] <- sapply(df[cols.num],as.numeric)

# check types
str(df)


ggplot(df, aes( `educationNum`,`capitalGain`)) +
            geom_point() +
            geom_smooth(method="lm") + 
            theme_bw()+
            ggtitle("Capital Gain per Education Level")

ggplot(df, aes( `sex`,`hoursPerWeek`)) +
  geom_boxplot(aes(color=sex)) +
  theme_bw()+
  ggtitle("Hours worked per week by Sex")

ggplot(df[df$sex==' Male',], aes(x=`hoursPerWeek`))+ 
  geom_histogram(binwidth=10 ,aes(y=..density..), colour="black", fill="white") + 
  geom_density(alpha=.2,
               adjust = 5, 
               fill='#FF6666')+
  theme_bw()+
  ggtitle("Distribution of hours worked for Men")
ggplot(df[df$sex==' Female',], aes(x=`hoursPerWeek`)) +   
  geom_histogram(binwidth=10 ,aes(y=..density..), colour="black", fill="white") + 
  geom_density(alpha=.2,
               adjust = 5, 
               fill='#FF6666')+
  theme_bw()+
  ggtitle("Distribution of hours worked for Women")

ggplot(df, aes( `nativeCountry`)) +
  geom_bar() +
  theme_bw()+
  coord_flip()+
  ggtitle("")

df <- df[df$`nativeCountry`==' United-States',]

ggplot(df, aes(x=sex, fill=label))+
  geom_bar(position=position_dodge())+
  theme_bw()+
  ggtitle("")

ggplot(df, aes(x=workclass, fill=label))+
  geom_bar(position=position_dodge())+
  theme_bw()+
  coord_flip()+
  ggtitle("")

ggplot(df, aes(x=education, fill=label))+
  geom_bar(position=position_dodge())+
  theme_bw()+
  coord_flip()+
  ggtitle("")

ggplot(df, aes(x=`maritalStatus`, fill=label))+
  geom_bar(position=position_dodge())+
  theme_bw()+
  coord_flip()+
  ggtitle("")
ggplot(df, aes(x=`maritalStatus`, y=age))+
  geom_point()+
  coord_flip()

ggplot(df[df$sex==' Female',], aes(x=`hoursPerWeek`)) +   
  geom_histogram(binwidth=10 ,aes(y=..density..), colour="black", fill="white") + 
  geom_density(alpha=.2,
               adjust = 5, 
               fill='#FF6666')+
  theme_bw()+
  ggtitle("Distribution of hours worked for Women")

df %>% 
  select(age, fnlwgt, educationNum , capitalGain  , capitalLoss  , hoursPerWeek ) %>% 
  gather(metric, value) %>% 
  ggplot(aes(value, fill = metric)) + 
  geom_density(show.legend = FALSE) + 
  facet_wrap(~ metric, scales = "free")

# calculating split point
nrow(df)
# want to split on 75/25
0.75*nrow(df)
# need to round to integer
round(0.75*nrow(df))
# set split
split <- round(0.75*nrow(df))
# split into test and train
train <- df[1:split,]
test <- df[(split+1):nrow(df),]
# save and remove labels for test set
ylab <- test$label
test$label <- NULL



# naive bayes
mod1 <- naiveBayes(label~.,data=train, laplace = 0.001)
x <- predict(mod1,test, type='raw')
x <- data.frame(x)
x$under50K <- x$X...50K
x$above <- x$X..50K

x$pred <- ifelse(x$under50K>x$above,' <=50K',' >50K')

compareNB <- data.frame('pred'=x$pred,'actual'=ylab)
cf_nb <- confusionMatrix(compareNB$actual,compareNB$pred)

# randomForest
df$sex <- as.factor(df$sex)
df$race <- as.factor(df$race)
df$relationship <- as.factor(df$relationship)
df$occupation <- as.factor(df$occupation)
df$maritalStatus <- as.factor(df$maritalStatus)
df$education <- as.factor(df$education)
df$workclass <- as.factor(df$workclass)
df$nativeCountry <- NULL
split <- round(0.75*nrow(df))

trainRF <- df[1:split,]
testRF <- df[(split+1):nrow(df),]
testLab <- testRF$label
testRF$label <- NULL
str(df)
ylab <- trainRF$label
trainRF$label <- as.factor(trainRF$label)
trainRF <- na.omit(trainRF)

trees <- c(25,50,100,150,200,250,500)
for (i in 1:length(trees)){
mod2 <- randomForest(label~.,
                     data=trainRF, 
                     na.action=na.omit,
                      mtry = 2,
                     ntree = trees[i])
ypred <- predict(mod2, testRF)
compare <- data.frame('pred'=ypred,'actual'=testLab)
cm_rf <- confusionMatrix(compare$actual,compare$pred)[[3]][[1]]
}
plot(trees, rf_Accuracies, type = "l", log = "x")

mod2 <- randomForest(label~.,
                     data=trainRF, 
                     na.action=na.omit,
                     mtry = 2,
                     ntree = 200)
ypred <- predict(mod2, testRF)
compare <- data.frame('pred'=ypred,'actual'=testLab)
cm_rf <- confusionMatrix(compare$actual,compare$pred)

plot(mod2)
varImpPlot(mod2)

# SVM
trainSVM <- trainRF
testSVM <- testRF
train$nativeCountry <- NULL

costs <- c(0.001,0.01,0.1,1,10,100,1000)
accuracies <- numeric(0)
for (i in 1:length(costs)){
  mod3 <- svm(label~.,
              data=trainSVM,
              kernel = 'radial',
              cost = costs[i])
  ypred <- predict(mod3, testSVM, type='raw')
  accuracies[i] <- confusionMatrix(as.factor(testLab),ypred)[[3]][[1]]
}
plot(costs, accuracies, type = "l", log = "x")

mod3 <- svm(label~.,
            data=trainSVM,
            kernel = 'radial',
            cost = #BEST)
ypred <- predict(mod3, testSVM, type='raw')
confusionMatrix(as.factor(testLab),ypred)