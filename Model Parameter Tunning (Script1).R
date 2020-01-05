library(rvest)
library(dplyr)
library(forecast)
library(ggplot2)
library(seasonal)
library(lubridate)
library(scales)
library(tidyverse)
library(zoo)
library(readr)
library(stringr)
library(DT)
library(tools)
library(prophet)
library(readxl)
library(timeDate)

data = read.csv('data.csv')
data <- data[,2:3]

data = data %>% 
  rename(
    date = Date
  )


train.data=data[year(data$date)<2017,]

test.data=data[year(data$date)>=2017,]
test.data[1,]
test.data[nrow(test.data),]

# classical decomposition method


# vec <- c()
# for(i in 1:970){
#   ms=msts(c(train.data$load,test.data$load[1:(6+(i-1)*24)]),seasonal.periods=c(24,24*7,24*2*7,24*30.5),ts.frequency=24*30.5)
#   model=mstl(ms) # lambda = 0 do the multiplicative decomposition
#   fc=forecast(model,h = 42, level = 0,allow.multiplicative.trend = T)
#   testerror=data.frame(accuracy(fc$mean[19:42],test.data$load[(i*24+1):((i+1)*24)]))
#   
#   vec<- c(vec,round(testerror$MAPE,2))
# }
# decomp_mape_update_vec = vec
# decomp_mape_update = mean(vec)

# average mape: 7.105639

972


train.data <- ts(train.data$load, start=c(1,1), frequency = 24)
test.data <- ts(test.data$load, start=c(1,1), frequency = 24)
ts.data <- ts(data$load, start=c(1,1), frequency = 24)

#smoothing
m_smooth <- ets(train.data)
# forecastval <- forecast(m_smooth,h=42,level=95)
# forecastval
# class(c(train.data, window(test.data, start(1, 1), end(1, (7+(2-1)*24)))))
# class(train.data)
# window(test.data, start(1, 1), end(1, (7+(2-1)*24)))
vec <- c()

for(i in 1:973){
  ts1 <- train.data
  ts2 <- window(test.data, start=c(1, 0), end=c(1, (7+(i-1)*24)))
  m_smooth2 = ets(ts(c(ts1,ts2), start=start(ts1), frequency=frequency(ts1)), model = m_smooth,use.initial.values = TRUE)
  forecastval <- forecast(m_smooth2,h=41,level=95)
  error=accuracy(forecastval$mean[18:41], test.data[((i*24)+1):((i+1)*24)])
  vec<- c(vec,round(error['Test set','MAPE'],2))
}

smooth_mape_vec = vec
smooth_mape = mean(vec)
#Smoothing mape: 8.2442



#ARIMA

train.data=data[year(data$date)<2017,]
test.data=data[year(data$date)>=2017,]


train.data <- ts(train.data$load, start=c(1,1), frequency = 24)
test.data <- ts(test.data$load, start=c(1,1), frequency = 24)


auto.arima(train.data)

# Series: train.data 
# ARIMA(5,0,0)(2,1,0)[24] 
# 
# Coefficients:
#   ar1      ar2     ar3      ar4     ar5     sar1     sar2
# 1.2101  -0.1115  0.0075  -0.2050  0.0576  -0.4172  -0.2240
# s.e.  0.0078   0.0106  0.0098   0.0096  0.0066   0.0062   0.0059
# 
# sigma^2 estimated as 49170:  log likelihood=-179242.5
# AIC=358501   AICc=358501   BIC=358566.4

auto.arima(ts.data)



# Series: ts.data 
# ARIMA(4,0,4)(2,1,0)[24] 
# 
# Coefficients:
#   ar1     ar2     ar3      ar4     ma1     ma2      ma3     ma4     sar1     sar2
# 0.4332  0.3836  0.6909  -0.5945  0.8379  0.5341  -0.2131  0.0630  -0.4185  -0.2349
# s.e.  0.0184  0.0239  0.0181   0.0115  0.0190  0.0164   0.0162  0.0076   0.0048   0.0045
# 
# sigma^2 estimated as 40881:  log likelihood=-334092.6
# AIC=668207.2   AICc=668207.2   BIC=668304.1

m_arima <- Arima(train.data, order=c(4,0,4),seasonal=c(2,1,0))
#m_arima <- Arima(train.data, order=c(4,0,4),seasonal=c(2,1,0))


vec <- c()
for(i in 1:973){
  ts1 <- train.data
  ts2 <- window(test.data, start=c(1, 0), end=c(1, (7+(i-1)*24)))
  m_arima2 = Arima(ts(c(ts1,ts2), start=start(ts1), frequency=frequency(ts1)), model = m_arima)
  forecast <- forecast(m_arima2,h=41,level=95)
  error=MAPE(forecast$mean[18:41], test.data[(i*24+1):((i+1)*24)])
  vec<- c(vec,round(error,2))
  
}
arima_mape_vec = vec
arima_mape = mean(vec)
# arima mape 6.77
tsdisplay(arima_mape_vec)



#Prophet

train.data=data[year(data$date)<2017,]
test.data=data[year(data$date)>=2017,]
names(train.data)=c("ds","y")
names(test.data)=c("ds","y")

m_prophet <- prophet(train.data,
                     growth = 'linear',
                     seasonality.mode = 'multiplicative',
                     changepoint.prior.scale = 30,
                     seasonality.prior.scale = 35
)

future <- make_future_dataframe(m_prophet, periods = 42, freq = 3600)
future = rbind(train.data, test.data[1:50,])
future = future%>%select(ds)
forecast <- predict(m_prophet, future)


forecast2 <- predict(m_prophet, test.data$ds[1:42])

# Function that returns Mean Absolute Error
mape <- function(error, truevalue)
{
  mean(abs(error/truevalue))
}
error=forecast[(nrow(train.data)+1):(nrow(train.data)+42),'yhat']-test.data$y[1:42]

round(mape(error, test.data$y[1:42])*100,2)
