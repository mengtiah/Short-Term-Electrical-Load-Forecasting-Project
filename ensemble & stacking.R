library(dplyr)
library(lubridate)
library(forecast)
library(MLmetrics)
library(xgboost)

df <- read.csv('final results.csv')

a <- ymd_hms('2014-1-5 18:00:00')
mdy_hm(df[,'date']) >= a

names(df.test)
# [1] "X"                                       "date"                                   
# [3] "actual_value"                            "simple_smoothing_pred"                  
# [5] "simple_arima_pred"                       "simple_theta_pred"                      
# [7] "simple_decomp_pred"                      "regression_bestsubset_pred"             
# [9] "regression_gradientboosting_subset_pred" "residual_arima_pred"                    
# [11] "residual_smoothing_pred"                 "residual_theta_pred"                    
# [13] "nnetar_pred"                             "ml_nn_pred"

df.train1 <- df[115:26304,]
df.test1 <- df[26329:nrow(df),]

df.train <- df.train1 %>%
  select(date:residual_theta_pred)
df.test <- df.test1 %>%
  select(date:residual_theta_pred)


# directly mean
mean1 <- rowMeans(df.test[,-(1:2)])
mean2 <- rowMeans(df.test[,c(7,9)])

for (i in 3:11) {
  res = MAPE(df.train[,i], df.rain[,'actual_value'])
  print(res)
}
# [1] 0.08244382
# [1] 0.06769124
# [1] 0.1110241
# [1] 0.05479468
# [1] 0.04622481
# [1] 0.1105477
# [1] 0.04607501
# [1] 0.06567043
# [1] 0.05680105

vec1 <- c()
for (i in 3:10) {
  for (j in (i+1):11) {
    res = MAPE(rowMeans(df.train[,c(i,j)]), df.train[,'actual_value'])
    print(res)
    vec1 = c(vec1, res)
  }
}
min(vec1)
which.min(vec1)



vec2 <- c()
vec_pra <- c()
for (i in 3:9) {
  for (j in (i+1):10) {
    for (k in (j+1):11) {
      res = MAPE(rowMeans(df.train[,c(i,j,k)]), df.train[,'actual_value'])
    print(res)
    vec_pra = c(vec_pra, list(c(i,j,k)))
    vec2 = c(vec2, res)
    }
  }
}
min(vec2)
which.min(vec2)
vec_pra[66]


vec3 <- c()
for (i in 3:8) {
  for (j in (i+1):9) {
    for (k in (j+1):10) {
      for (l in (k+1):11) {
        res = MAPE(rowMeans(df.train[,c(i,j,k,l)]), df.train[,'actual_value'])
        print(res)
        vec3 = c(vec3, res)
      }
    }
  }
}
min(vec3)
which.min(vec3)



vec4 <- c()
for (i in 3:7) {
  for (j in (i+1):8) {
    for (k in (j+1):9) {
      for (l in (k+1):10) {
        for (m in (l+1):11) {
          res = MAPE(rowMeans(df.train[,c(i,j,k,l,m)]), df.train[,'actual_value'])
          print(res)
          vec4 = c(vec4, res)
        }
      }
    }
  }
}
min(vec4)
which.min(vec4)



vec5 <- c()
for (i in 3:6) {
  for (j in (i+1):7) {
    for (k in (j+1):8) {
      for (l in (k+1):9) {
        for (m in (l+1):10) {
          for (n in (m+1):11) {
            res = MAPE(rowMeans(df.train[,c(i,j,k,l,m,n)]), df.train[,'actual_value'])
          print(res)
          vec5 = c(vec5, res)
          }
        }
      }
    }
  }
}
min(vec5)
which.min(vec5)



vec6 <- c()
for (i in 3:5) {
  for (j in (i+1):6) {
    for (k in (j+1):7) {
      for (l in (k+1):8) {
        for (m in (l+1):9) {
          for (n in (m+1):10) {
            for (o in (n+1):11) {
              res = MAPE(rowMeans(df.train[,c(i,j,k,l,m,n,o)]), df.train[,'actual_value'])
              print(res)
              vec6 = c(vec6, res)
            }
          }
        }
      }
    }
  }
}
min(vec6)
which.min(vec6)



vec7 <- c()
for (i in 3:4) {
  for (j in (i+1):5) {
    for (k in (j+1):6) {
      for (l in (k+1):7) {
        for (m in (l+1):8) {
          for (n in (m+1):9) {
            for (o in (n+1):10) {
              for (p in (o+1):11) {
                res = MAPE(rowMeans(df.train[,c(i,j,k,l,m,n,o,p)]), df.train[,'actual_value'])
                vec7 = c(vec7, res)
              }
            }
          }
        }
      }
    }
  }
}
min(vec7)
which.min(vec7)

MAPE(rowMeans(df.test[,c(3:11)]), df.test[,'actual_value'])
MAPE(rowMeans(df.train[,c(3:11)]), df.train[,'actual_value'])

names(df.train)

# Best Mean - with column 6,7,9
MAPE(rowMeans(df.test[,c(6,7,9)]), df.test[,'actual_value'])
# = 4.108%

stacking1 <- data.frame(date=df.test$date, actual_value=df.test$actual_value, stacking_pred1=rowMeans(df.test[,c(6,7,9)]))
MAPE(stacking1$stacking_pred1, stacking1$actual_value)
write.csv(stacking1, 'stacking result.csv')

names(df.test)

### Try linear

linear <- lm(actual_value~., data = dftrain2)
summary(linear)
linear2 <- lm(actual_value~.-regression_bestsubset_pred , data = dftrain2)
summary(linear2)

pred_y <- predict.lm(linear2, newdata = df.test)

MAPE(pred_y, df.test[,'actual_value'])

# linear3 <- lm(actual_value~., data = dftest2)
# linear3$fitted.values
# summary(linear3)
# MAPE(linear3$fitted.values, df.test[,'actual_value'])
# 
# 
# regit.full <- regsubsets(actual_value~., data = dftest2)
# reg.summary <- summary(regit.full)
# names(summary(regit.full))
# which.min(reg.summary$rsq)
# which.min(reg.summary$rss)
# which.min(reg.summary$adjr2)
# which.min(reg.summary$cp)
# which.min(reg.summary$bic)

# try XGBoost
xgbtrain <- model.matrix(actual_value~., data = dftrain2)
xgbtrain <- xgb.DMatrix(xgbtrain, label = dftrain2$actual_value)
xgbtest <- model.matrix(actual_value~., data = dftest2)
xgbtest <- xgb.DMatrix(xgbtest, label = dftest2$actual_value)


param <- list(max_depth = 9, eta = 0.1, silent = 1, objective = "reg:linear")
bst <- xgboost(data = xgbtrain, nrounds = 100, params = param)
pred <- predict(bst, xgbtest)
MAPE(pred, df.test[,'actual_value'])

