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

names(df.train)

models <- regsubsets(load18~X+
                       load+holiday+hour0+hour1+
                       hour2+hour3+hour4+hour5+
                       hour6+hour7+hour8+hour9+
                       hour10+hour11+hour12+hour13+
                       hour14+hour15+hour16+hour17+
                       hour18+hour19+hour20+hour21+
                       hour22+mon+tue+wed+
                       thu+fri+sat+jan+
                       feb+mar+apr+may+
                       jun+jul+aug+sep+
                       oct+nov+spring+summer+
                       fall+tmpf_RIV42+tmpf_RIV48+
                       dwpf_RIV42+dwpf_RIV48+relh_RIV42+relh_RIV48+
                       drct_RIV42+drct_RIV48+sknt_RIV42+sknt_RIV48+
                       alti_RIV42+alti_RIV48+mslp_RIV42+mslp_RIV48+
                       vsby_RIV42+vsby_RIV48+feel_RIV42+feel_RIV48+
                       tmpf_TRM42+tmpf_TRM48+dwpf_TRM42+dwpf_TRM48+
                       relh_TRM42+relh_TRM48+drct_TRM42+drct_TRM48+
                       sknt_TRM42+sknt_TRM48+alti_TRM42+alti_TRM48+
                       mslp_TRM42+mslp_TRM48+vsby_TRM42+vsby_TRM48+
                       feel_TRM42+feel_TRM48+tmpf_WJF42+tmpf_WJF48+
                       dwpf_WJF42+dwpf_WJF48+relh_WJF42+relh_WJF48+
                       drct_WJF42+drct_WJF48+sknt_WJF42+sknt_WJF48+
                       alti_WJF42+alti_WJF48+mslp_WJF42+mslp_WJF48+
                       vsby_WJF42+vsby_WJF48+feel_WJF42+feel_WJF48+
                       tmpf_CQT42+tmpf_CQT48+dwpf_CQT42+dwpf_CQT48+
                       relh_CQT42+relh_CQT48+drct_CQT42+drct_CQT48+
                       sknt_CQT42+sknt_CQT48+alti_CQT42+alti_CQT48+
                       mslp_CQT42+mslp_CQT48+vsby_CQT42+vsby_CQT48+
                       feel_CQT42+feel_CQT48+tmpf_LAX42+tmpf_LAX48+
                       dwpf_LAX42+dwpf_LAX48+relh_LAX42+relh_LAX48+
                       drct_LAX42+drct_LAX48+sknt_LAX42+sknt_LAX48+
                       alti_LAX42+alti_LAX48+mslp_LAX42+mslp_LAX48+
                       vsby_LAX42+vsby_LAX48+feel_LAX42+feel_LAX48+
                       Temp_Forecast_LAX.18+precipProbability_Forecast_LAX.18+humidity_Forecast_LAX.18+windSpeed_Forecast_LAX.18+
                       visibility_Forecast_LAX.18+Temp_Forecast_TRM.18+precipProbability_Forecast_TRM.18+humidity_Forecast_TRM.18+
                       windSpeed_Forecast_TRM.18+visibility_Forecast_TRM.18+tmpf_RIV+dwpf_RIV+
                       relh_RIV+drct_RIV+sknt_RIV+alti_RIV+
                       mslp_RIV+vsby_RIV+feel_RIV+tmpf_TRM+
                       dwpf_TRM+relh_TRM+drct_TRM+sknt_TRM+
                       alti_TRM+mslp_TRM+vsby_TRM+feel_TRM+
                       tmpf_WJF+dwpf_WJF+relh_WJF+drct_WJF+
                       sknt_WJF+alti_WJF+mslp_WJF+vsby_WJF+
                       feel_WJF+tmpf_CQT+dwpf_CQT+relh_CQT+
                       drct_CQT+sknt_CQT+alti_CQT+mslp_CQT+
                       vsby_CQT+feel_CQT+tmpf_LAX+dwpf_LAX+
                       relh_LAX+drct_LAX+sknt_LAX+alti_LAX+
                       mslp_LAX+vsby_LAX+feel_LAX+tmpf_RIV1+
                       dwpf_RIV1+relh_RIV1+drct_RIV1+sknt_RIV1+
                       alti_RIV1+mslp_RIV1+vsby_RIV1+feel_RIV1+
                       tmpf_TRM1+dwpf_TRM1+relh_TRM1+drct_TRM1+
                       sknt_TRM1+alti_TRM1+mslp_TRM1+vsby_TRM1+
                       feel_TRM1+tmpf_WJF1+dwpf_WJF1+relh_WJF1+
                       drct_WJF1+sknt_WJF1+alti_WJF1+mslp_WJF1+
                       vsby_WJF1+feel_WJF1+tmpf_CQT1+dwpf_CQT1+
                       relh_CQT1+drct_CQT1+sknt_CQT1+alti_CQT1+
                       mslp_CQT1+vsby_CQT1+feel_CQT1+tmpf_LAX1+
                       dwpf_LAX1+relh_LAX1+drct_LAX1+sknt_LAX1+
                       alti_LAX1+mslp_LAX1+vsby_LAX1+feel_LAX1+
                       tmpf_lax_mean_6+tmpf_cqt_mean_6+tmpf_riv_mean_6+tmpf_trm_mean_6+
                       tmpf_wjf_mean_6+tmpf_lax_mean_12+tmpf_cqt_mean_12+tmpf_riv_mean_12+
                       tmpf_trm_mean_12+tmpf_wjf_mean_12+tmpf_lax_mean_24+tmpf_cqt_mean_24+
                       tmpf_riv_mean_24+tmpf_trm_mean_24+tmpf_wjf_mean_24+tmpf_lax_max_6+
                       tmpf_cqt_max_6+tmpf_riv_max_6+tmpf_trm_max_6+tmpf_wjf_max_6+
                       tmpf_lax_max_12+tmpf_cqt_max_12+tmpf_riv_max_12+tmpf_trm_max_12+
                       tmpf_wjf_max_12+tmpf_lax_max_24+tmpf_cqt_max_24+tmpf_riv_max_24+
                       tmpf_trm_max_24+tmpf_wjf_max_24+tmpf_lax_min_6+tmpf_cqt_min_6+
                       tmpf_riv_min_6+tmpf_trm_min_6+tmpf_wjf_min_6+tmpf_lax_min_12+
                       tmpf_cqt_min_12+tmpf_riv_min_12+tmpf_trm_min_12+tmpf_wjf_min_12+
                       tmpf_lax_min_24+tmpf_cqt_min_24+tmpf_riv_min_24+tmpf_trm_min_24+
                       tmpf_wjf_min_24+tmpf_lax_std_6+tmpf_cqt_std_6+tmpf_riv_std_6+
                       tmpf_trm_std_6+tmpf_wjf_std_6+tmpf_lax_std_12+tmpf_cqt_std_12+
                       tmpf_riv_std_12+tmpf_trm_std_12+tmpf_wjf_std_12+tmpf_lax_std_24+
                       tmpf_cqt_std_24+tmpf_riv_std_24+tmpf_trm_std_24+tmpf_wjf_std_24+
                       tmpf_lax_range_6+tmpf_cqt_range_6+tmpf_riv_range_6+tmpf_trm_range_6+
                       tmpf_wjf_range_6+tmpf_lax_range_12+tmpf_cqt_range_12+tmpf_riv_range_12+
                       tmpf_trm_range_12+tmpf_wjf_range_12+tmpf_lax_range_24+tmpf_cqt_range_24+
                       tmpf_riv_range_24+tmpf_trm_range_24+tmpf_wjf_range_24+loadlag1+loadlag2+loadlag3+loadlag4+loadlag5+
                       loadlag6+loadlag7+loadlag8+loadlag9+loadlag10+loadlag11+loadlag12+loadlag13+loadlag14+loadlag15+
                       loadlag16+loadlag17+loadlag18+loadlag19+loadlag20+loadlag21+loadlag22+loadlag23+loadlag24
                     , data = df.train, nvmax = 100, method = 'forward')
summary=summary(models)


write.csv(summary[["outmat"]], '20v.csv', row.names = FALSE)

which.min(summary$cp)
which.min(summary$bic)
which.max(summary$adjr2)
par(mfrow = c(2, 2))
plot(summary$cp)
plot(summary$bic)
plot(summary$adjr2)


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

summary(rm)

accuracy(rm)

tsdisplay(residuals(rm))

pred_y = predict.lm(rm, newdata = df.test)
rmtestresiduals = df.test$load18 - pred_y
rmtestresiduals = ts(rmtestresiduals,frequency = 24)


length(residuals(rm))
length(rmtestresiduals)

MAPE(pred_y, df.test$load18) 
# mape = 0.0462351

rmresiduals = ts(residuals(rm),frequency = 24)

auto.arima(rmresiduals)

# Series: rmresiduals 
# ARIMA(3,0,3)(2,0,0)[24] with zero mean 
# 
# Coefficients:
#   ar1     ar2      ar3      ma1      ma2     ma3     sar1     sar2
# 1.1054  0.2463  -0.4303  -0.0547  -0.3552  0.0820  -0.2040  -0.1262
# s.e.  0.0608  0.1052   0.0482   0.0608   0.0424  0.0074   0.0062   0.0062
# 
# sigma^2 estimated as 69658:  log likelihood=-183312.9
# AIC=366643.7   AICc=366643.7   BIC=366717.3


auto.arima(rmtestresiduals)

# Series: rmtestresiduals 
# ARIMA(3,0,2)(2,0,1)[24] with non-zero mean 
# 
# Coefficients:
#   ar1      ar2     ar3      ma1     ma2     sar1     sar2    sma1     mean
# 2.3479  -1.8791  0.5116  -1.2395  0.4493  -0.2882  -0.1709  0.1157  -4.3022
# s.e.  0.1006   0.1681  0.0724   0.0974  0.0498   0.0285   0.0075  0.0285  12.7632
# 
# sigma^2 estimated as 56560:  log likelihood=-160945
# AIC=321910.1   AICc=321910.1   BIC=321990.7

auto.arima(ts(c(rmresiduals,rmtestresiduals), start=start(rmresiduals), frequency = 24))

# Series: ts(c(rmresiduals, rmtestresiduals), start = start(rmresiduals),      frequency = 24) 
# ARIMA(4,0,4)(2,0,1)[24] with zero mean 
# 
# Coefficients:
#   ar1     ar2     ar3      ar4     ma1      ma2      ma3     ma4     sar1     sar2    sma1
# 0.0183  1.1356  0.3920  -0.7151  1.0566  -0.0417  -0.5189  0.0605  -0.2429  -0.1404  0.0623
# s.e.  0.0096  0.0080  0.0068   0.0082  0.0105   0.0179   0.0143  0.0056   0.0224   0.0056  0.0224
# 
# sigma^2 estimated as 63544:  log likelihood=-344415.1
# AIC=688854.2   AICc=688854.2   BIC=688959.9


head(rmtestresiduals)
head(rmresiduals)


m_arima1 <- Arima(rmresiduals, order=c(3,0,3),seasonal=c(2,0,0))
m_arima2 <- Arima(rmresiduals, order=c(3,0,2),seasonal=c(2,0,1))
m_arima3 <- Arima(rmresiduals, order=c(4,0,4),seasonal=c(0,0,0))
# 404, 201 4.669
# 404, 000 4.59


test.y = df.test$load18

vec1 = c()
vec2 = c()
vec3 = c()

## 1
for(i in 1:972){
  ts1 <- rmresiduals
  ts2 <- window(rmtestresiduals, start=c(1, 0), end=c(1, (7+(i-1)*24)))
  m_arimax = Arima(ts(c(ts1,ts2), start=start(ts1), frequency=frequency(ts1)), model = m_arima1)
  forecast <- forecast(m_arimax,h=41,level=95)
  forecast2 = forecast$mean+pred_y[(8+(i-1)*24):(8+(i-1)*24+40)]
  mape=MAPE(forecast2[18:41], test.y[(i*24+1):((i+1)*24)])
  vec1<- c(vec1,round(mape,2))
  # rmse=RMSE(forecast2[18:41], test.y[(i*24+1):((i+1)*24)])
  # vec1<- c(vec1,round(rmse,2))
}

arima_mape_vec1 = vec1
arima_mape1 = mean(vec1)


## 2
for(i in 1:972){
  ts1 <- rmresiduals
  ts2 <- window(rmtestresiduals, start=c(1, 0), end=c(1, (7+(i-1)*24)))
  m_arimax = Arima(ts(c(ts1,ts2), start=start(ts1), frequency=frequency(ts1)), model = m_arima2)
  forecast <- forecast(m_arimax,h=41,level=95)
  forecast2 = forecast$mean+pred_y[(8+(i-1)*24):(8+(i-1)*24+40)]
  mape=MAPE(forecast2[18:41], test.y[(i*24+1):((i+1)*24)])
  vec2<- c(vec2,round(mape,2))
  # rmse=RMSE(forecast2[18:41], test.y[(i*24+1):((i+1)*24)])
  # vec1<- c(vec1,round(rmse,2))
}

arima_mape_vec2 = vec2
arima_mape2 = mean(vec2)


## 3
for(i in 1:972){
  ts1 <- rmresiduals
  ts2 <- window(rmtestresiduals, start=c(1, 0), end=c(1, (7+(i-1)*24)))
  m_arimax = Arima(ts(c(ts1,ts2), start=start(ts1), frequency=frequency(ts1)), model = m_arima3)
  forecast <- forecast(m_arimax,h=41,level=95)
  forecast2 = forecast$mean+pred_y[(8+(i-1)*24):(8+(i-1)*24+40)]
  mape=MAPE(forecast2[18:41], test.y[(i*24+1):((i+1)*24)])
  vec3<- c(vec3,round(mape,2))
  # rmse=RMSE(forecast2[18:41], test.y[(i*24+1):((i+1)*24)])
  # vec1<- c(vec1,round(rmse,2))
}

arima_mape_vec3 = vec3
arima_mape3 = mean(vec3)




### Smoothing Method

m_smooth <- ets(rmresiduals)

## 4
vec4 = c()

for(i in 1:972){
  ts1 <- rmresiduals
  ts2 <- window(rmtestresiduals, start=c(1, 0), end=c(1, (7+(i-1)*24)))
  m_smooth_x = ets(ts(c(ts1,ts2), start=start(ts1), frequency=24), model = m_smooth,use.initial.values = TRUE)
  forecast <- forecast(m_smooth_x,h=41,level=95)
  forecast2 = forecast$mean+pred_y[(8+(i-1)*24):(8+(i-1)*24+40)]
  mape=MAPE(forecast2[18:41], test.y[(i*24+1):((i+1)*24)])
  vec4<- c(vec4,round(mape,2))
  # rmse=RMSE(forecast2[18:41], test.y[(i*24+1):((i+1)*24)])
  # vec1<- c(vec1,round(rmse,2))
}

smooth_mape_vec4 = vec4
smooth_mape4 = mean(vec4)
## 7.00


## 5

m_smooth2 <- ets(ts(c(rmresiduals,rmtestresiduals), start=start(rmresiduals), frequency = 24))

vec5 = c()

for(i in 1:972){
  ts1 <- rmresiduals
  ts2 <- window(rmtestresiduals, start=c(1, 0), end=c(1, (7+(i-1)*24)))
  m_smooth_x = ets(ts(c(ts1,ts2), start=start(ts1), frequency=24), model = m_smooth2,use.initial.values = TRUE)
  forecast <- forecast(m_smooth_x,h=41,level=95)
  forecast2 = forecast$mean+pred_y[(8+(i-1)*24):(8+(i-1)*24+40)]
  mape=MAPE(forecast2[18:41], test.y[(i*24+1):((i+1)*24)])
  vec5<- c(vec5,round(mape,2))
  # rmse=RMSE(forecast2[18:41], test.y[(i*24+1):((i+1)*24)])
  # vec1<- c(vec1,round(rmse,2))
}

smooth_mape_vec5 = vec5
smooth_mape5 = mean(vec5)
## 6.68





