library(randomForest)
require(caTools)

data <- read.csv(
  "C:/Users/drew.howell/Desktop/CSC-201/2020_Fall/20201102_Random_Forest/processed.cleveland.data",
  header=FALSE
)

dim(data)
names(data) <- c("age", "sex", "cp", "trestbps", "choi", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thai", "num")

data$num[data$num > 1] <- 1

sapply(data, class)

data <- transform(
  data,
  age=as.integer(age),
  sex=as.factor(sex),
  cp=as.factor(cp),
  trestbps=as.integer(trestbps),
  choi=as.integer(choi),
  fbs=as.factor(fbs),
  restecg=as.factor(restecg),
  thalach=as.integer(thalach),
  exang=as.factor(exang),
  oldpeak=as.numeric(oldpeak),
  slope=as.factor(slope),
  ca=as.factor(ca),
  thai=as.factor(thai),
  num=as.factor(num)
)
sapply(data, class)

data[ data == "?"] <- NA
colSums(is.na(data))

data$thai[which(is.na(data$thai))] <- as.factor("3.0")
data <- data[!(data$ca %in% c(NA)),]
colSums(is.na(data))
summary(data)

data$ca <- factor(data$ca)
data$thai <- factor(data$thai)
summary(data)


sample = sample.split(data$num, SplitRatio = .75)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
dim(train)
dim(test)

rf <- randomForest(
  num ~ .,
  data=train
  ,mtry = 6
  ,ntree = 500
)

pred = predict(rf, newdata=test[-14])

table(test[,14], pred)

library(caret)
gbmImp <- varImpPlot(rf, type=2)
plot(gbmImp)

library(rpart)
fit=rpart(num ~ .,
          data=train)
plot(fit) 
text(fit)

library(e1071)
nb_mod <- naiveBayes(num ~ .,data=train)
pred_nb = predict(nb_mod, newdata=test[-14])
table(test[,14], pred_nb)

sv_mod <- svm(num ~ .,data=train, kernel = 'linear')
pred_SVl = predict(sv_mod, newdata=test[-14])
table(test[,14], pred_SVl)

sv_mod <- svm(num ~ .,data=train, kernel = 'radial')
pred_SVl = predict(sv_mod, newdata=test[-14])
table(test[,14], pred_SVl)

sv_mod <- svm(num ~ .,data=train, kernel = 'sigmoid')
pred_SVl = predict(sv_mod, newdata=test[-14])
table(test[,14], pred_SVl)

sv_mod <- svm(num ~ .,data=train, kernel = 'polynomial')
pred_SVl = predict(sv_mod, newdata=test[-14])
table(test[,14], pred_SVl)
