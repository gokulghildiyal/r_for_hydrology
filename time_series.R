library(e1071)
library(quantmod)
library(forecast)
library(tseries)
library(Quandl)
library(rugarch)
library(tseries)
library(pastecs)
library(forecast)
library(ISLR)
library(imputeTS)
set.seed(13)
#F1
data=F1$...2
data
index=c(1:847)
#plotting to observe patterm
plot(data,type='l')
#boxplot for outliers
boxplot(data)
boxplot.stats(data)$out
#removing outliers
data1=data[-c(1:3)]
data1=ts(data1,frequency=4)
#data1=data

#seasonal decomposition
sea_decomp=decompose(data1)
data2=data1-sea_decomp$trend
data2=na.omit(data2)
#checking if time series is stationary
adf.test(data2)
#time series is stationary

model=auto.arima(data2)
#fitting an ARMA model
acf(data2,lag.max=100)
pacf(data2)
#gradual damping of correlogram indicates high ordered ar model model with one root positive and another root negative
ar_2=arima(data2,order=c(3,0,5))
fit_ar2=data2-residuals(ar_2)
plot(data2,type="l")
points(fit_ar2,col="red")
AIC(ar_2)
BIC(ar_2)
plot(residuals(ar_2))#random error ensuring good fit
checkresiduals(ar_2)
accuracy(ar_2)
#fitting ar_2+cubic trend
final_fit=fit_ar2+fit
plot(data1,type="l")
lines(final_fit,col="red")
#F2
d=as.numeric(unlist(F2[,2]))
#removing missing values doesnt make sense in time series
#d1=na.omit(d)
index1=c(1:1068)
d1=data.frame(d[1:1068])
#plot
plot(ts(d1),type="l")
d1=ts(d1)
#imputing NA
statsNA(d1)

plotNA.distribution(d1)
plotNA.distributionBar(d1, breaks = 20)
plotNA.gapsize(d1)

ts.ma_simple = na_ma(d1,weighting = "simple")
plotNA.imputations(d1, ts.ma_simple)
ts1 = ts.interpolation

#searchin for trend
ts2=ts1
#searching for seasonal
decomp=decompose(ts2)
ts3=ts2-decomp$trend
ts3=na.omit(ts3)
adf.test(ts3)

model=auto.arima(ts2)
acf(ts3)

pacf(ts2)
#
arm=arima(ts2,order=c(6,0,1))
checkresiduals(arm)
accuracy(arm)


ts=d1
ts.median = na_mean(ts,option = "median")
plotNA.imputations(ts, ts.median)

ts.mean = na_mean(ts,option = "mean")
plotNA.imputations(ts, ts.mean)

ts.mode = na_mean(ts,option = "mode")
plotNA.imputations(ts, ts.mode)

ts.linear = na_interpolation(ts,option = "linear")
plotNA.imputations(ts, ts.linear)

ts.interpolation = na_interpolation(ts,option = "spline")
plotNA.imputations(ts, ts.interpolation)

ts.locf = na_locf(ts,option = "locf")
plotNA.imputations(ts, ts.locf)

ts.nocb = na_locf(ts,option = "nocb")
plotNA.imputations(ts, ts.nocb)

ts.ma_simple = na_ma(ts,weighting = "simple")
plotNA.imputations(ts, ts.ma_simple)

ts.ma_linear = na_ma(ts,weighting = "linear")
plotNA.imputations(ts, ts.ma_linear)

ts.ma_exponential = na_ma(ts,weighting = "exponential")
plotNA.imputations(ts, ts.ma_exponential)

ts.random = na_random(ts)
plotNA.imputations(ts, ts.random)

