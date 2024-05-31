library(data.table)
library(ggplot2)
library(lubridate)
library(forecast)
library(urca)

#Introduction ----

data <- read.csv("C:/Users/alpsr/Desktop/Homework 3/RealTimeConsumption-01012016-20052021.csv")
data <- data.table(data)
str(data)

data[, 'Date' := dmy(Date)]
data[, 'Day' := lubridate::wday(Date, label = TRUE)]
data[, 'Month' := month(Date, label =TRUE)]
data[, 'Hour' := hour(hm(Hour))]
setnames(data, "Consumption..MWh.", "Consumption")
data[, 'Consumption' := gsub(",","", Consumption)]
data[, 'Consumption' := as.numeric(Consumption)]

ggplot(data, aes(x = Date))+
  geom_line(aes(y = Consumption, color = 'Consumption'))

acf(data[,Consumption])
Box.test(data$Consumption, type = 'Ljung-Box')
test <- ur.kpss(data$Consumption, use.lag = 168)
summary(test)

#Analyzing Seasonality----

#Daily - on a hourly level----
hour_avg <- data[,list(AvgCons = mean(Consumption)), by = .(year(Date), Hour)]
varhour <- data[,list(Var = var(Consumption)), by = .(Hour)]
#hour_avg <- data[,list(AvgCons = mean(Consumption)), by = .(Hour)]

ggplot(hour_avg, aes(x = Hour))+
  geom_line(aes(y = AvgCons, group = factor(year), color = factor(year)))

dailyts = ts(data$Consumption, freq = 24)
dailyts_short <- ts(dailyts[1:1200], freq = 24)
monthplot(dailyts_short)

daily_dec <- decompose(dailyts)
plot(daily_dec)
plot(daily_dec$random)

mean(daily_dec$random, na.rm = TRUE)
summary(ur.kpss(daily_dec$random))
#Box.test(daily_dec$random, type = 'Ljung-Box')

#Weekly - on a daily level----
day_avg <- data[,list(AvgCons = mean(Consumption)), by = .(year(Date), Day)]
ggplot(day_avg, aes(x = Day))+
  geom_line(aes(y = AvgCons, group = factor(year), color = factor(year)))

weeklyts = ts(data$Consumption, freq = 168)
monthplot(weeklyts[1:1680])

weekly_dec <- decompose(weeklyts)
plot(weekly_dec)

#Box.test(weekly_dec$random, type = 'Ljung-Box')
plot(weekly_dec$random)
summary(ur.kpss(weekly_dec$random))

#Monthly ----
month_avg <- data[,list(AvgCons = mean(Consumption)), by = .(year(Date), day(Date))]
ggplot(month_avg, aes(x = day))+
  geom_line(aes(y = AvgCons, group = factor(year), color = factor(year)))

monthlyts = ts(data$Consumption, freq = 720)
monthplot(monthlyts[1:7200])

monthly_dec <- decompose(monthlyts)
plot(monthly_dec)

#Box.test(weekly_dec$random, type = 'Ljung-Box')
plot(monthly_dec$random)
summary(ur.kpss(monthly_dec$random))

#Yearly - on a monthly level ----
yrmo_avg <- data[,list(AvgCons = mean(DailyCons)), by = .(year(Date), Month)]
ggplot(yrmo_avg, aes(x = Month))+
  geom_line(aes(y = AvgCons, group = factor(year), color = factor(year)))

yearlyts = ts(data$Consumption, freq = 8760)
monthplot(yearlyts[1:15000])

yearly_dec <- decompose(yearlyts)
plot(yearly_dec)

#Box.test(weekly_dec$random, type = 'Ljung-Box')
plot(yearly_dec$random)
summary(ur.kpss(yearly_dec$random))

#seasonality on all levels

#Stationarity - Taking Logs----
logdata <- data[, log_cons := log(Consumption)]
test <- ur.kpss(logdata$log_cons, use.lag = 168)
summary(test)

log_ts <- ts(logdata$log_cons, freq = 168)
log_dec <- decompose(log_ts)
plot(log_dec)
Box.test(log_dec$random, type = "Ljung-Box")
ur.kpss()
?ur.kpss
#Stationarity - Differencing----
diffdata <- data[, diff := Consumption - shift(Consumption)]
diff_ts <- ts(data$diff, freq = 168)

test <- ur.kpss(diff_ts)
summary(test)

diff_dec <- decompose(diff_ts)
plot(diff_dec)

tsdisplay(diff_ts, lag.max = 168)
Box.test(log_dec$random, type = "Ljung-Box")

#Part B----
cons <- ts(data$Consumption, frequency = 168)
tsdisplay(cons)

acf(cons)
pacf(cons)

cons_dec <- decompose(cons, type = 'additive')
plot(cons_dec)

test_stat <- ur.kpss(cons_dec$random)
summary(test_stat) #Data is stationary after decomposition
#str(cons_dec)

random <-  cons - cons_dec$trend - cons_dec$seasonal
ts.plot(random)

test_stat <- ur.kpss(random, use.lag = 168)
summary(test_stat)

acfs <- acf(random, na.action = na.pass, lag.max = 336) #Sinusodial pattern with period = 24, spikes at lag = 0, 24, 48....
pacfs <- pacf(random, na.action = na.pass, lag.max = 336) #Lag 1 very high, lag 2 very low, sharp decrease still meaningful, sharp increase at lag 24.
acfs$acf[2] #Positive autocorrelation at lag = 1, suggesting ar terms will work best
#Conclusion: AR will work best maybe p = 2.
#ACf shows a sinusodial pattern with spikes at 24,48,... this shows there may be a seasonal pattern in the data. ALso the length of these spikes are decreasing indiciating a trend maybe. 
#pacf spiky at lags 24-25 and so on
#this indicates data may not be stationary enough. (like a seasonal pattern in the data)(differencing or seasonal differencing may be required)
#also all pacf and acf are high -- not stationary enough
#but the data has an ar signature

#pacf(diff(random), na.action = na.pass)

#Part C - Autoregressive models ----
acfs <- acf(random, na.action = na.pass)
pacfs <- pacf(random, na.action = na.pass)

sdiff <- diff(random, lag =24 )
tsdisplay(sdiff)

#based on the partial autocorrelation -> lag 1 or lag 2 relatively go on to try 3 and 4

ar1 <- arima(random, order = c(1,0,0))
AIC(ar1)

ar2 <- arima(random, order = c(2,0,0))
AIC(ar2)

ar3 <- arima(random, order = c(3,0,0))
AIC(ar3)

ar4 <- arima(random, c(4,0,0))
AIC(ar4)
summary(ar4)

ar5 <- arima(random, c(5,0,0))
AIC(ar5)

ar6 <- arima(random, c(6,0,0))
AIC(ar6)

model <- auto.arima(random, trace = TRUE, seasonal = TRUE, allowmean = FALSE, allowdrift = FALSE)
model <- auto.arima(random, trace = TRUE)
model <- auto.arima(random, trace = TRUE, seasonal = FALSE)
model2 <- arima(random, c(5,0,5))
AIC(model2)

res <- 0
for(i in c(1:24)){
  temp <- arima(random, c(i,0,0))
  res[i] <- AIC(temp)
}
res
diff(res)
deneme <- auto.arima(random, max.q = 0, max.p = 10, seasonal = FALSE, trace = TRUE, allowmean = TRUE)

#Part D - Moving Average Models ----
ma1 <- arima(random, order = c(0,0,1))
AIC(ma1)

ma2 <- arima(random, order = c(0,0,2))
AIC(ma2)

ma3 <- arima(random, order = c(0,0,3))
AIC(ma3)

ma4 <- arima(random, order = c(0,0,4))
AIC(ma4)

ma5 <- arima(random, order = c(0,0,5))
AIC(ma5)

#Part E----
data[,Trend := 0]
data[,Seasonal := 0]
train <- ts(data[Date <= '2021-05-05', Consumption], frequency = 168)
test <- ts(data[Date >= '2021-05-06', Consumption], frequency = 168)
testdata <- data[Date >= '2021-05-06',]

dec <- decompose(train)
rem <- dec$random

MA <- arima(rem, order = c(0,0,5))
AR <- arima(rem, order = c(4,0,0))

row <- 46848
for(i in seq(1,360)){
  tra <- ts(data[1:(row+i-1), Consumption], frequency = 168)
  #tes <- ts(data[(row+i):47208, Consumption], frequency = 168)
  
  dec <- decompose(tra)
  rem <- dec$random
  
  testdata[i, Trend := tail(dec$trend[!is.na(dec$trend)],1)]
  testdata[i, Seasonal := dec$seasonal[length(dec$seasonal)-167]]

}

ARf <- Arima(tail(rem[!is.na(rem)],5), model = AR)
MAf <- Arima(rem[!is.na(rem)], model = MA)

testdata[,'AR_random' := forecast(ARf, h = 360)$mean]
testdata[,'MA_random' := forecast(MAf, h = 360)$mean]

#testdata[,'AR_random' := forecast(ARf, h = 360)$mean]
#testdata[,'MA_random' := forecast(MAf, h = 360)$mean]
#setnames(testdata, c("AR_error", "MA_error"), c("AR_random", "MA_random"))

testdata[,'AR_forecast' := Trend+Seasonal+AR_random]
testdata[,'MA_forecast' := Trend+Seasonal+MA_random]

accu=function(actual,forecast){
  n=length(actual)
  error=actual-forecast
  mean=mean(actual)
  sd=sd(actual)
  CV=sd/mean
  FBias=sum(error)/sum(actual)
  MAPE=sum(abs(error/actual))/n
  RMSE=sqrt(sum(error^2)/n)
  MAD=sum(abs(error))/n
  MADP=sum(abs(error))/sum(abs(actual))
  WMAPE=MAD/mean
  l=data.frame(n,mean,sd,CV,FBias,MAPE,RMSE,MAD,MADP,WMAPE)
  return(l)
}

accu(testdata$Consumption,testdata$AR_forecast)
accu(testdata$Consumption,testdata$MA_forecast)