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

data = read_xlsx('20140101-20190901 SCE & CAISO Actual Load  9 27 2019.xlsx', sheet = 'new_df')


data = data %>% 
  rename(
    date = Date
  )


train.data=data[year(data$date)<2017,]
test.data=data[year(data$date)>=2017,]

timestamp.training <- data[year(data$date)<2017,'date']
timestamp.testing <- data[year(data$date)>=2017,'date']

finalmatrix <- data.frame(date = data[, 'date'], actual_value = data[, 'load'])


# classical decomposition method
ms=msts(train.data$load,seasonal.periods=c(24,24*7,24*2*7,24*30.5),ts.frequency=24*30.5)
model=mstl(ms)
fctest=forecast(model, h=1)
decompyhatvec = c()
vec <- c()
for(i in 1:973){
  ms=msts(c(train.data$load,test.data$load[1:(7+(i-1)*24)]),seasonal.periods=c(24,24*7,24*2*7,24*30.5),ts.frequency=24*30.5)
  model=mstl(ms) # lambda = 0 do the multiplicative decomposition
  fc=forecast(model,h = 42, level = 0,allow.multiplicative.trend = T)
  testerror=data.frame(accuracy(fc$mean[18:41],test.data$load[(i*24+1):((i+1)*24)]))
  
  vec<- c(vec,round(testerror$MAPE,2))
  decompyhatvec = c(decompyhatvec, fc$mean[18:41])
}
decomp_mape_update_vec = vec
decomp_mape_update = mean(vec)

df_decomp = data.frame(decomp_mape_update_vec)
write.csv(df_decomp,'decomp mape.csv')

df_decomp_yhat = data.frame(decompyhatvec)
write.csv(df_decomp_yhat,'decomp yhat.csv')

timestamp.testing[1:3,1]


matrix1 <- data.frame(date = timestamp.training, simple_decomp_pred = as.numeric(fctest$fitted))
matrix2 <- data.frame(date = timestamp.testing[25:23376,], simple_decomp_pred = decompyhatvec)
matrix_temp <- rbind(matrix1, matrix2)

finalmatrix <- left_join(finalmatrix, matrix_temp, by = 'date')




################################################################################################


library(dplyr)
library(ggplot2)
library(readxl)
library(chron)
library(timeDate)
library(leaps)
library(MLmetrics)
library(forecast)

##########################################################################################
##########################################################################################
######REGRESSION
df = read.csv('pro_df_nona.csv')

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
nrow(df.test)
df[26233,c('time')]
df[49590,]


### NN
df.train.x = df.train%>% select(c(holiday,
                                  mon,
                                  tue,
                                  wed,
                                  thu,
                                  fri,
                                  sat,
                                  sep,
                                  summer,
                                  tmpf_TRM48,
                                  sknt_WJF48,
                                  sknt_CQT48,
                                  Temp_Forecast_LAX.18,
                                  windSpeed_Forecast_LAX.18,
                                  tmpf_riv_min_24,
                                  tmpf_riv_range_6,
                                  loadlag1,
                                  loadlag6,
                                  loadlag14,
                                  loadlag24))

df.test.x = df.test%>% select(c(holiday,
                                mon,
                                tue,
                                wed,
                                thu,
                                fri,
                                sat,
                                sep,
                                summer,
                                tmpf_TRM48,
                                sknt_WJF48,
                                sknt_CQT48,
                                Temp_Forecast_LAX.18,
                                windSpeed_Forecast_LAX.18,
                                tmpf_riv_min_24,
                                tmpf_riv_range_6,
                                loadlag1,
                                loadlag6,
                                loadlag14,
                                loadlag24))

nn_model = nnetar(ts(df.train$load,frequency = 24), p = 72, P = 2, size = 10, xreg = df.train.x)
forecasttest = forecast(nn_model,h=1, xreg=df.test.x[1,]) 

yvec = c()
yhatvec = c()
mapevec = c()

for(i in 1:973){
  ts1 <- ts(df.train$load,frequency = 24)
  ts2 <- window(ts(df.test$load, frequency = 24), start=c(1, 1), end=c(1, (7+(i-1)*24)))
  nn_model2 = nnetar(ts(c(ts1,ts2), start=start(ts1), frequency=frequency(ts1)), model = nn_model, xreg = rbind(df.train.x, df.test.x[1:(7+(i-1)*24),]))
  
  forecast = forecast(nn_model2,h=41, xreg=df.test.x[(8+(i-1)*24):(8+(i-1)*24+40),]) 
  yvec=c(yvec, ts(df.test$load,start = c(1094,1),frequency = 24)[(i*24+1):((i+1)*24)])
  yhatvec<- c(yhatvec,forecast$mean[18:41])
  
}



df_nn_yhat = data.frame(actual = yvec, nnetar_pred = yhatvec)
write.csv(df_nn_yhat,'nn yhat.csv')

nn_select_mape_vec = mapevec
nn_select_mape = mean(mapevec)


df_nn = data.frame(nn_select_mape_vec)


write.csv(df_nn,'nn mape.csv')
