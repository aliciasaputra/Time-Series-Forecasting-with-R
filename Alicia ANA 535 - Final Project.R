# ....................................
# PRELIMINARY WORK

#Library

library(forecast)
library(zoo)
library(readxl)

#Dataset
inflationdata <- read_excel('/Users/aliciasaputra/Desktop/inflation_new.xls',sheet="Inflation Data")
head(inflationdata)

#Taking logs, create date variables, and define time series
inflation = inflation$CPI
nv <- length(inflation)
ln.inflation = log(inflation) #logs
d.ln.inflation = diff(ln.inflation) #growth rates

inflation.raw.ts <- ts(inflation[2:nv], start = c(2006,3), end = c(2022,10), freq = 12)
inflation.ts <- ts(ln.inflation [2:nv], start = c(2006,3), end = c(2022,10), freq = 12)
dinflation.ts <- ts(d.ln.inflation, start = c(2006,3), end = c(2022,10), freq = 12)

#DataFrame
inflation.data <- data.frame(inflation.raw.ts,inflation.ts,dinflation.ts)

#Descriptive Statistics
library(pastecs)
ds.inflation <- stat.desc(inflation.data, basic = F)
ds.inflation

round(ds.inflation, 5)

#Plotting the data

#Raw Data
plot(inflation.raw.ts, type = 'l', xlab = 'Time', ylab = 'Inflation Raw', 
     xlim=c(2006,2026),main = 'Inflation Raw Data', lty = 1, lwd = 2, col = 'blue')

lines(c(2023+8/12 - nValid/12, 2023+8/12 - nValid/12), c(0, 6)) 
lines(c(2022+8/12, 2022+8/12), c(0, 6))
text(2010, 6, "Training")
text(2021+8/12, 6, "Validation")
text(2020.6 + 3, 4, "Future")
arrows(2006, 5, 2021+8/12 - nValid/12, 5, code = 3, length = 0.1, lwd = 1,angle = 25)
arrows(2020+8/12, 5, 2022+8/12, 5, code = 3, length = 0.1, lwd = 1,angle = 25)
arrows(2022+8/12, 3, 2023+8/12 + nValid/12, 3, code = 3, length = 0.1, lwd = 1,angle = 25)

#Natural Logs
plot(inflation.ts, type = 'l', xlab = 'Time', ylab = 'Inflation in Log', xlim=c(2006,2026),
     main = 'Inflation Data in Log', lty = 1, lwd = 2, col = 'blue')
lines(c(2023+8/12 - nValid/12, 2023+8/12 - nValid/12), c(-1, 6)) 
lines(c(2022+8/12, 2022+8/12), c(-1, 6))
text(2010, 1.7, "Training")
text(2021+8/12, 1.7, "Validation")
text(2020.6 + 3, 1.5, "Future")
arrows(2006, 1.3, 2021+8/12 - nValid/12, 1.3, code = 3, length = 0.1, lwd = 1,angle = 25)
arrows(2020+8/12, 1.3, 2022+8/12, 1.3, code = 3, length = 0.1, lwd = 1,angle = 25)
arrows(2022+8/12, 1, 2023+8/12 + nValid/12, 1, code = 3, length = 0.1, lwd = 1,angle = 25)

#Growth Rate
plot(dinflation.ts, type = 'l', xlab = 'Time', ylab = 'Inflation',
     main = 'Inflation Data Growth Rate', lty = 1, lwd = 2, col = 'blue')

# ....................................
# END OF PRELIMINARY WORK

# ....................................
# PARTITIONING

length(inflation.ts)

nvalid <- 48 #4 years training period
ntrain <- length(inflation.ts) - nvalid
train.ts <- window(inflation.ts, start = c(2006, 3), end = c(2006, ntrain))
valid.ts <- window(inflation.ts, start = c(2006, ntrain + 1), end = c(2006, ntrain + nvalid))
inflation.lm <-  tslm(train.ts ~ trend + I(trend^2))
inflation.lm.pred <- forecast(inflation.lm, h = nvalid, level = 95)

# ....................................
# FORECASTING

#NAIVE FORECAST

inflation.naive <- naive(train.ts, h = nvalid)
plot(inflation.naive,  ylab = "Inflation in Log", xlab = "Time", 
     bty = "l", xlim = c(2006,2025), lwd = 2, xaxt = "n", main = "Naive Inflation Rate Forecast", flty = 2)
axis(1, at = seq(2006, 2025,5), labels = format(seq(2006, 2025,5))) 
lines(inflation.naive$mean, lwd = 2, col = "cyan3", lty = 2)


#Diagnostics for Naive Forecast
accuracy(inflation.naive$mean, valid.ts)

#MOVING AVERAGE FORECAST
library(zoo)
ma.trailing <- rollmean(inflation.ts, k = 12, align = "right")
ma.centered <- ma(inflation.ts, order = 12)
plot(inflation.ts, ylab = 'Inflation in Log', xlab = 'Time', 
     bty = "l", lwd = 2, xaxt = "n", xlim = c(2006, 2025), main = 'Moving Averages',)
axis(1, at = seq(2006,2025, 1), labels = format(seq(2006, 2025, 1)))
lines(ma.centered, lwd = 3, lty = 2, col = 'red')
lines(ma.trailing, lwd = 3, lty = 3, col = 'blue')
legend(2009,2.1, c("Raw Data","Centered Moving Average","Trailing Moving Averages"), lty=c(1,1,2), lwd=c(1,2,2), bty = "n")

#Diagnostics for Moving Average
accuracy(ma.centered, inflation.ts) #Centered MA
accuracy(ma.trailing, inflation.ts) #Trailing MA

#EXPONENTIAL SMOOTHING

hwin <- ets(train.ts, model = "ZAN")
hwin.pred <- forecast(hwin, h = nValid, level = 0.2) 

plot(hwin.pred, ylab = "ln Inflation Rate", xlab = "Date", bty = "l", xaxt = "n", 
     xlim = c(2006,2028), lwd= 2, main = 'Exponential Smoothing Forecast', flty = 2)
axis(1, at = seq(2006, 2028, 1), labels = format(seq(2006, 2028, 1)))
lines(hwin.pred$fitted, lwd = 1, col = "red")

accuracy(hwin.pred)

#LINEAR MODEl

train.lm.linear.trend <- tslm(train.ts ~ trend)
train.lm.linear.trend.pred <- forecast(train.lm.linear.trend, h = nvalid, level = 0)

summary(train.lm.linear.trend)


plot(train.ts, ylab = "Inflation in Log", xlab = "Date", bty = "l", xaxt = "n", 
     xlim = c(2006,2025), lwd= 2, main = "Inflation Forecast Linear Model", lty = 1)
axis(1, at = seq(2006, 2025, 1), labels = format(seq(2006, 2025, 1)))
lines(train.lm.linear.trend.pred$fitted, lwd = 2, col = "red", lty = 3)
lines(train.lm.linear.trend.pred$mean, lwd = 2, col = "red", lty = 3)

#Diagnostics for Linear Model
accuracy(train.lm.linear.trend.pred)

#POLYNOMIAL MODEL
inflation.lm.poly.trend <- tslm(train.ts ~ trend + I(trend^2))
inflation.lm.poly.trend.pred <- forecast(inflation.lm.poly.trend, h = nvalid, level = 0)
plot(train.ts, ylab = "Inflation in Log", xlab = "Date", bty = "l", xaxt = "n", 
     xlim = c(2006,2026), lwd= 2, main = "Inflation Forecast Polynomial Model", lty = 1)
axis(1, at = seq(2006, 2026, 1), labels = format(seq(2006, 2026, 1)))
lines(inflation.lm.poly.trend.pred$fitted, lwd = 2, col = "red", lty = 3)
lines(inflation.lm.poly.trend.pred$mean, lwd = 2, col = "red", lty = 3)

#calculate the MAE, RMSE, and MAPE.
accuracy(inflation.lm.poly.trend.pred)



#Unit Root Forecast
installed.packages('urca')
library(urca)

#Removing NA value
inflation.ts<-inflation.ts[!is.na(inflation.ts)]

#Unit Root test
lc.df <- ur.df(inflation.ts, lags = 1, type = "drift")
summary(lc.df)

plot(lc.df)

#dickey fuller test
library(tseries)
dft<-adf.test(inflation.ts)
dft

#Forecast Inflation from random walk with drift

inflation.forecast.ts <- rwf(train.ts, h = nvalid, drift = TRUE, level=c(80,95), fan = FALSE, LAMBDA = 0.004)

plot(inflation.forecast.ts, type = 'l', xlab = 'Time', ylab = 'Trucks Sales in Log', xlim = c(2006,2026),
     main = 'Forecast Inflation from Random Walk with Drift', lty = 1, lwd = 2, col = 'blue')

accuracy(inflation.forecast.ts)

#ARIMA Model
fit <- auto.arima(train.ts)
future <- plot(forecast(fit,h=nvalid), xlim=c(2006,2025),type = 'l', xlab = 'Time', ylab = 'Inflation in Log',
               main = 'Forecast Inflation with ARIMA', lty = 1, lwd = 2, col = 'blue')
lines(valid.ts,lty = 1, lwd = 2, col = 'red')

accuracy(future$mean,valid.ts)

#Structural Forecast

wage.ts <- ts(inflationdata$Wage[2:nv],start = c(2006,3), end = c(2022,10), freq = 12)
unemployment.ts <- ts(inflationdata$unemployment_rate[2:nv],start = c(2006,3), end = c(2022,10), freq = 12)

#Check for length to make sure everything is the same length
length(inflation.ts)
length(wage.ts)
length(unemployment.ts)

train.gs.linear.trend <- tslm(inflation.ts ~ wage.ts + unemployment.ts + trend)
summary(train.gs.linear.trend)
gs.res = resid(train.gs.linear.trend)
plot(gs.res, type = 'l', xlab = 'Date', ylab = "Residuals", main = 'Structural Model Residuals',
     lty = 1, col = 'red')



#Use the model above to forecast truck sales for a 4 years validation period

nValid <- 48
nTrain <- length(inflation.ts) - nValid
yTrain.ts <- window(trucks.ts, start = c(2006, 3), end = c(2006, nTrain))
test <- data.frame(cbind(wage.ts, unemployment.ts))
xTrain.ts <- ts(test, start = c(2006, 3), end = c(2006, nTrain))

stlm.reg.fit <- stlm(yTrain.ts, s.window = "periodic", xreg = NULL, method = "ets") 
stlm.reg.fit$model

xTest.ts <- ts(xTrain.ts, start = c(2006, nTrain + 1), end = c(2006, nTrain + nValid)) 
stlm.reg.pred <- forecast(stlm.reg.fit, xreg = xTest.ts, h = nValid)
plot(stlm.reg.pred,xlab = "Date", xlim=c(2006,2025),type = 'l',ylab = 'Inflation in Log',
     main = 'Forecasts from STL + Regression with ETS Model', lty = 1, lwd = 2, col = 'blue')

accuracy(stlm.reg.pred)




