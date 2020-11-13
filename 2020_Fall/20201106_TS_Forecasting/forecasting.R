library(forecast)
library(MLmetrics)
data=AirPassengers
#Create samples
training=window(data, start = c(1949,1), end = c(1955,12))
validation=window(data, start = c(1956,1))

# Naive
naive = naive(training, h=length(validation))
plot(data, col="blue", xlab="Year", ylab="Passengers", main="Seasonal Naive Forecast", type='l')
lines(naive$mean, col="red", lwd=2)
MAPE(naive$mean, validation) * 100

# Seasonal Naive
naive = snaive(training, h=length(validation))
plot(data, col="blue", xlab="Year", ylab="Passengers", main="Seasonal Naive Forecast", type='l')
lines(naive$mean, col="red", lwd=2)
MAPE(naive$mean, validation) * 100

# Exponential Smoothing
ets_model = ets(training, allow.multiplicative.trend = TRUE)
summary(ets_model)
ets_forecast = forecast(ets_model, h=length(validation))
plot(data, col="blue", xlab="Year", ylab="Passengers", main="Seasonal Naive Forecast", type='l')
lines(ets_forecast$mean, col="red", lwd=2)
MAPE(ets_forecast$mean, validation) *100

# Double-seasonal Holt-Winters
dshw_model = dshw(training, period1=4, period2 = 12, h=length(validation))
plot(data, col="blue", xlab="Year", ylab="Passengers", main="Seasonal Naive Forecast", type='l')
lines(dshw_model$mean, col="red", lwd=2)
MAPE(dshw_model$mean, validation)*100

# TBATS
tbats_model = tbats(training)
tbats_forecast = forecast(tbats_model, h=length(validation))
plot(data, col="blue", xlab="Year", ylab="Passengers", main="Seasonal Naive Forecast", type='l')
lines(tbats_forecast$mean, col="red", lwd=2)
MAPE(tbats_forecast$mean, validation) * 100

# ARIMA
arima_optimal = auto.arima(training)

library(astsa)
sarima_forecast = sarima.for(training, n.ahead=length(validation),
                             p=0,d=1,q=1,P=1,D=1,Q=0,S=12)
plot(data, col="blue", xlab="Year", ylab="Passengers", main="Seasonal Naive Forecast", type='l')
lines(sarima_forecast$pred, col="red", lwd=2)
MAPE(sarima_forecast$pred, validation) * 100

# One Step Ahead ARIMA
one_step_ahead_sarima = matrix(ncol = 2, nrow = 60)
for (i in 1:60){
  
  training_observed = window(data, start = c(1949,1), end = c(1955,(12+i)), frequency = 12)
  
  forecasted.sarima = sarima.for(training_observed,n.ahead=1,p=0,d=1,q=1,P=1,D=1,Q=0,S=12)
  
  demandforecast = forecasted.sarima$pred
  observed = validation[[i]]
  
  one_step_ahead_sarima[i,1]= observed
  one_step_ahead_sarima[i,2]= demandforecast
}
MAPE(one_step_ahead_sarima[,1], one_step_ahead_sarima[,2]) * 100
