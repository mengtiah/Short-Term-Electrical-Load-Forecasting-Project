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
library(forecast)
library(readxl)
library(chron)
library(timeDate)
library(leaps)
library(MLmetrics)





# For simple method

data = read.csv('data.csv')
data <- data[,2:3]

data = data %>% 
  rename(date = Date)

train.data=data[year(data$date)<2017,]
test.data=data[year(data$date)>=2017,]

train.data <- ts(train.data$load, start=c(1,1), frequency = 24)
test.data <- ts(test.data$load, start=c(1,1), frequency = 24)
ts.data <- ts(data$load, start=c(1,1), frequency = 24)

timestamp.training <- data[year(data$date)<2017,'date']
timestamp.testing <- data[year(data$date)>=2017,'date']

finalmatrix <- data.frame(date = data[, 'date'], actual_value = data[, 'load'])


# Smoothing method
m_smooth_simple <- ets(train.data)
pred.test1 <- c()

for(i in 1:973){
  ts1 <- train.data
  ts2 <- window(test.data, start=c(1, 0), end=c(1, (7+(i-1)*24)))
  m_smooth <- ets(ts(c(ts1,ts2), start=start(ts1), frequency=24), model = m_smooth_simple,use.initial.values = TRUE)
  forecastval <- forecast(m_smooth,h=41,level=95)
  pred.test1 <- c(pred.test1, forecastval$mean[18:41])
}

matrix1 <- data.frame(date = timestamp.training, simple_smoothing_pred = as.numeric(m_smooth_simple$fitted))
matrix2 <- data.frame(date = timestamp.testing[-(1:24)], simple_smoothing_pred = pred.test1)
matrix_temp <- rbind(matrix1, matrix2)

finalmatrix <- left_join(finalmatrix, matrix_temp, by = 'date')



# Arima
m_arima <- Arima(train.data, order=c(4,0,4),seasonal=c(2,1,0))

pred.test2 <- c()
for(i in 1:973){
  ts1 <- train.data
  ts2 <- window(test.data, start=c(1, 0), end=c(1, (7+(i-1)*24)))
  m_arima2 = Arima(ts(c(ts1,ts2), start=start(ts1), frequency=24), model = m_arima)
  forecastval <- forecast(m_arima2,h=41,level=95)
  pred.test2 <- c(pred.test2, forecastval$mean[18:41])
}

MAPE(pred.test2, test.data[-(1:24)])

matrix1 <- data.frame(date = timestamp.training, simple_arima_pred = as.numeric(m_arima$fitted))
matrix2 <- data.frame(date = timestamp.testing[-(1:24)], simple_arima_pred = pred.test2)
matrix_temp <- rbind(matrix1, matrix2)

finalmatrix <- left_join(finalmatrix, matrix_temp, by = 'date')



# Theta
pred.test3 <- c()

for(i in 1:973){
  ts1 <- train.data
  ts2 <- window(test.data, start=c(1, 0), end=c(1, (7+(i-1)*24)))
  m_theta = thetaf(ts(c(ts1,ts2), start=start(ts1), frequency=24), h = 41, level = 95, fan = T)
  forecastval <- m_theta$mean
  pred.test3 <- c(pred.test3, forecastval[18:41])
}

m_theta_ori = thetaf(train.data, h = 24, level = 95, fan = T)

MAPE(pred.test3, test.data[-(1:24)])

matrix1 <- data.frame(date = timestamp.training, simple_theta_pred = as.numeric(m_theta_ori$fitted))
matrix2 <- data.frame(date = timestamp.testing[-(1:24)], simple_theta_pred = pred.test3)
matrix_temp <- rbind(matrix1, matrix2)

finalmatrix <- left_join(finalmatrix, matrix_temp, by = 'date')




## decompose
decom <- read.csv('decomp.csv')
a <- decom[-(1:26328),]
MAPE(a$simple_decomp_pred, a$load)
decom <- decom[,c('date', 'simple_decomp_pred')]
finalmatrix <- left_join(finalmatrix, decom, by = 'date')





# For regression based

df = read.csv('pro_df_nona.csv')
df[,'time']
timestamp.training2 <- df[43:26250,'time']
timestamp.testing2 <- df[26233:49590,'time']

# Regression - Feature Selection Best Subset

df$loadlag1 = lag(df$load, 1)
df$loadlag2 = lag(df$load, 2)
df$loadlag3 = lag(df$load, 3)
df$loadlag4 = lag(df$load, 4)
df$loadlag5 = lag(df$load, 5)
df$loadlag6 = lag(df$load, 6)
df$loadlag7 = lag(df$load, 7)
df$loadlag8 = lag(df$load, 8)
df$loadlag9 = lag(df$load, 9)
df$loadlag10 = lag(df$load, 10)
df$loadlag11 = lag(df$load, 11)
df$loadlag12 = lag(df$load, 12)
df$loadlag13 = lag(df$load, 13)
df$loadlag14 = lag(df$load, 14)
df$loadlag15 = lag(df$load, 15)
df$loadlag16 = lag(df$load, 16)
df$loadlag17 = lag(df$load, 17)
df$loadlag18 = lag(df$load, 18)
df$loadlag19 = lag(df$load, 19)
df$loadlag20 = lag(df$load, 20)
df$loadlag21 = lag(df$load, 21)
df$loadlag22 = lag(df$load, 22)
df$loadlag23 = lag(df$load, 23)
df$loadlag24 = lag(df$load, 24)

df.train = df[1:26232,]
df.test = df[26233:49590,]

rm = lm(load18~holiday+
          mon+
          tue+
          wed+
          thu+
          fri+
          sat+
          sep+
          summer+
          tmpf_TRM48+
          sknt_WJF48+
          sknt_CQT48+
          Temp_Forecast_LAX.18+
          windSpeed_Forecast_LAX.18+
          tmpf_riv_min_24+
          tmpf_riv_range_6+
          loadlag1+
          loadlag6+
          loadlag14+
          loadlag24, data = df.train)

pred_y = predict.lm(rm, newdata = df.test)
rm$fitted.values
MAPE(pred_y[-(1:6)], test.data[-(1:24)])

matrix1 <- data.frame(date = timestamp.training2, regression_bestsubset_pred = as.numeric(rm$fitted.values)[1:26208])
matrix2 <- data.frame(date = timestamp.testing[-(1:24)], regression_bestsubset_pred = pred_y[7:23358])
matrix_temp <- rbind(matrix1, matrix2)
finalmatrix <- left_join(finalmatrix, matrix_temp, by = 'date')


# Regression - Gradient Boosting Choice
rm2 = lm(load18~tmpf_CQT48+tmpf_RIV48+feel_CQT48+dwpf_TRM+
           feel_LAX48+tmpf_riv_max_6+feel_TRM+feel_TRM48+
           dwpf_TRM1+tmpf_TRM48+dwpf_LAX42+tmpf_WJF48+
           tmpf_riv_min_12+sknt_RIV48+dwpf_CQT42+feel_WJF48+
           feel_RIV48+relh_TRM48+tmpf_LAX48+dwpf_CQT1+
           dwpf_LAX+dwpf_LAX1+dwpf_CQT+tmpf_riv_min_6+
           dwpf_LAX48+tmpf_wjf_min_12+drct_LAX48+
           tmpf_riv_mean_12+tmpf_CQT42+tmpf_riv_mean_6+
           relh_WJF+tmpf_riv_mean_24+alti_TRM48+tmpf_RIV+
           mslp_TRM48+tmpf_riv_min_24+tmpf_RIV1+mslp_CQT42+
           tmpf_wjf_std_12+tmpf_wjf_min_6+mslp_RIV48+
           feel_RIV,
         data=df.train)

pred_y2 = predict.lm(rm2, newdata = df.test)
rm2$fitted.values
MAPE(pred_y2[-(1:6)], test.data[-(1:24)])

matrix1 <- data.frame(date = timestamp.training2, regression_gradientboosting_subset_pred = as.numeric(rm2$fitted.values)[1:26208])
matrix2 <- data.frame(date = timestamp.testing[-(1:24)], regression_gradientboosting_subset_pred = pred_y2[7:23358])
matrix_temp <- rbind(matrix1, matrix2)
finalmatrix <- left_join(finalmatrix, matrix_temp, by = 'date')


# Residual - Prepare
rmtestresiduals = df.test$load18 - pred_y
rmtestresiduals = ts(rmtestresiduals,frequency = 24)
rmresiduals = ts(residuals(rm),frequency = 24)
test.y = df.test$load18


# Residual - Arima
m_arima3 <- Arima(rmresiduals, order=c(4,0,4),seasonal=c(0,0,0))

pred.test4 <- c()
for(i in 1:973){
  ts1 <- rmresiduals[1:(26208-24)]
  ts1_temp <- ts(c(rmresiduals[(26208-23):26208],rmtestresiduals), start=start(ts1), frequency=24)
  ts2 <- window(ts1_temp, start=c(1, 0), end=c(1, (i*24-11)))
  m_arimax = Arima(ts(c(ts1,ts2), start=start(ts1), frequency=24), model = m_arima3)
  forecast <- forecast(m_arimax,h=41,level=95)
  forecastval = forecast$mean[18:41]+pred_y[(7+(i-1)*24):(7+(i-1)*24+23)]
  pred.test4 <- c(pred.test4, forecastval)
}

MAPE(pred.test4, test.y[7:23358])

matrix1 <- data.frame(date = timestamp.training2, residual_arima_pred = as.numeric(rm$fitted.values)[1:26208]+as.numeric(m_arima3$fitted))
matrix2 <- data.frame(date = timestamp.testing[-(1:24)], residual_arima_pred = pred.test4)
matrix_temp <- rbind(matrix1, matrix2)
finalmatrix <- left_join(finalmatrix, matrix_temp, by = 'date')



# Residual - Smoothing
m_smooth_2 <- ets(rmresiduals)

pred.test5 <- c()
for(i in 1:973){
  ts1 <- rmresiduals[1:(26208-24)]
  ts1_temp <- ts(c(rmresiduals[(26208-23):26208],rmtestresiduals), start=start(ts1), frequency=24)
  ts2 <- window(ts1_temp, start=c(1, 0), end=c(1, (i*24-11)))
  m_smooth <- ets(ts(c(ts1,ts2), start=start(ts1), frequency=24), model = m_smooth_2,use.initial.values = TRUE)
  forecast <- forecast(m_smooth,h=41,level=95)
  forecastval = forecast$mean[18:41]+pred_y[(7+(i-1)*24):(7+(i-1)*24+23)]
  pred.test5 <- c(pred.test5, forecastval)
}

MAPE(pred.test5, test.y[7:23358])

matrix1 <- data.frame(date = timestamp.training2, residual_smoothing_pred = as.numeric(rm$fitted.values)[1:26208]+as.numeric(m_smooth_2$fitted))
matrix2 <- data.frame(date = timestamp.testing[-(1:24)], residual_smoothing_pred = pred.test5)
matrix_temp <- rbind(matrix1, matrix2)
finalmatrix <- left_join(finalmatrix, matrix_temp, by = 'date')


# Residual - Theta
pred.test6 <- c()
for(i in 1:973){
  ts1 <- rmresiduals[1:(26208-24)]
  ts1_temp <- ts(c(rmresiduals[(26208-23):26208],rmtestresiduals), start=start(ts1), frequency=24)
  ts2 <- window(ts1_temp, start=c(1, 0), end=c(1, (i*24-11)))
  m_theta = thetaf(ts(c(ts1,ts2), start=start(ts1), frequency=24), h = 41, level = 95, fan = T)
  forecastval <- m_theta$mean[18:41]+pred_y[(7+(i-1)*24):(7+(i-1)*24+23)]
  pred.test6 <- c(pred.test6, forecastval)
}

m_theta_ori = thetaf(rmresiduals, h = 24, level = 95, fan = T)

MAPE(pred.test6, test.y[7:23358])

matrix1 <- data.frame(date = timestamp.training2, residual_theta_pred = as.numeric(rm$fitted.values)[1:26208]+as.numeric(m_theta_ori$fitted)[1:26208])
matrix2 <- data.frame(date = timestamp.testing[-(1:24)], residual_theta_pred = pred.test6)
matrix_temp <- rbind(matrix1, matrix2)
finalmatrix <- left_join(finalmatrix, matrix_temp, by = 'date')



## nn
nn <- read.csv('nnetar.csv')
MAPE(nn$nnetar_pred, nn$actual)
names(nn)
nn <- nn[,c('date', 'nnetar_pred')]
nn$date <- as.character(mdy_hm(nn$date))

finalmatrix <- left_join(finalmatrix, nn, by = 'date')



## ML nn
mlnn <- read.csv('nn_results.csv')
MAPE(mlnn$ml_nn_pred, test.data[-(1:24)])
names(mlnn)
mlnn <- mlnn[,c('date', 'ml_nn_pred')]
mlnn$date <- as.character(mdy_hm(mlnn$date))

finalmatrix <- left_join(finalmatrix, mlnn, by = 'date')



write.csv(finalmatrix, 'final results.csv')



