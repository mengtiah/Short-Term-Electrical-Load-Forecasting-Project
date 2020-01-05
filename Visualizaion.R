library(ggplot2)
library(dplyr)
library(MLmetrics)
library(lubridate)
library(magrittr)
df = read.csv('final results.csv')

# df = read.csv('stacking result.csv')

price = read.csv('Price_Data_2015_2019.csv')
df$dateonly = mdy_hm(df$date)
df$date = as.Date(df$dateonly)
df$hour = hour(df$dateonly)
df$year = year(df$date)
df$month = month(df$date)
df$yearmonth = paste(df$year, df$month)
price$date = ymd(price$DATE_KEY)
price$HE = price$HE - 1
price$key = paste(price$date,price$HE)
df$key = paste(df$date,df$hour)





df1 = df[26305:49680,] %>% mutate(absdiff = abs((actual_value-nnetar_pred)/actual_value)) %>%
  group_by(date) %>% summarise(dailymape = mean(absdiff))


  # ggplot(aes(x = date, y = dailymape)) +
  # geom_line(color = 'steelblue')+
  # xlab('Date')+
  # ylab('Daily MAPE')+
  # ggtitle('Daily MAPE of Simple Smoothing')+
  # theme_light()

df2 = df[26305:49680,] %>% filter(hour %in% c(18,19,20,21)) %>% 
  mutate(absdiff = abs((actual_value-nnetar_pred)/actual_value)) %>%
  group_by(date) %>% summarise(dailymape = mean(absdiff))


ggplot(data = df2, aes(x = date, y = dailymape),fill = "transparent" ) +
  geom_line(color = '#1C816B', alpha = 0.8)+
  geom_line(data = df1, aes(x = date, y = dailymape), color = '#FFE34A', alpha = 0.8)+
  xlab('Date')+
  ylab('MAPE')+
  ggtitle('MAPE of NNETAR')+
    theme_light()+
  theme(#panel.border = element_blank(),
  #       legend.key = element_blank(),
  #       axis.ticks = element_blank(),
  #       axis.text.y = element_blank(),
  #       axis.text.x = element_blank(),
  #       panel.grid = element_blank(),
  #       panel.grid.minor = element_blank(), 
  #       panel.grid.major = element_blank(),
        #panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))

ggsave("test.png", bg = "transparent", width=785, height=447, limitsize = FALSE)

pricetable = df[26305:49680,] %>% inner_join(price, by = c('key'='key'))
df3 = pricetable %>% mutate(cost = Price*pmax((actual_value-nnetar_pred),0))

df3 %>% group_by(date.x) %>% summarise(totalcost = sum(cost), year = min(year)) %>%
  group_by(year) %>% summarise(totalcost = mean(na.omit(totalcost)))



df3 %>% group_by(date.x) %>% summarise(totalcost = sum(cost)) %>%
  ggplot(aes(x = date.x, y = totalcost)) +
  geom_line(color = 'steelblue')+
  xlab('Date')+
  ylab('Daily Cost')+
  ggtitle('Daily Real Time Energy Purchasing Cost - NNETAR Model')+
  theme_light()

df4 = pricetable %>% mutate(over = pmax((nnetar_pred-actual_value),0))

df4 %>% group_by(date.x) %>% summarise(totalover = sum(over)) %>%
  ggplot(aes(x = date.x, y = totalover)) +
  geom_line(color = 'steelblue')+
  xlab('Date')+
  ylab('Daily Overestimation')+
  ggtitle('Daily Overestimation - NNETAR Model')+
  theme_light()


df10=df %>% group_by(yearmonth) %>% summarise(total = sum(actual_value))
%>%
  ggplot(aes(x=yearmonth, y=total))+
  geom_line()

l=length(df$actual_value[df$year==2017])
100*MAPE(df$actual_value[df$year==2017][25:(l-24)],df$nnetar_pred[df$year==2017][25:(l-24)])

df$nnetar_pred[df$year==2017][1:(l-24)]

MAPE(df$actual_value[df$year==2018],df$nnetar_pred[df$year==2018])

l=length(df$actual_value[df$year==2017])
100*MAPE(df$actual_value[df$year==2017][25:(l-24)],df$stacking_pred1[df$year==2017][25:(l-24)])



