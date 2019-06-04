library(forecast)
library(dplyr)

data.air<-read.csv("DATA/airandpatient.csv",stringsAsFactors = FALSE)
data.air<-read.csv("DATA/airandpatient_weekday.csv",stringsAsFactors = FALSE)

data.air$date<-as.Date(data.air$date)


x=ts(data.air$CO)
Box.test(x, lag=20, type="Ljung-Box")
plot.ts(x)
# plot(y=x,x=data.air$date,type = "l")
# abline(reg=lm(x~data.air$date),col="red")
acf(x,lag.max = 100)
pacf(x,lag.max = 100)
holt_x<-holt(x,h=5)
Box.test(holt_x$residuals, lag=20, type="Ljung-Box")

plot.ts(holt_x$residuals)

#find best diff
ndiffs(x)
dx<-diff(x,1)
acf(dx,lag.max = 100)
pacf(dx,lag.max = 100)

Box.test(dx, lag=20, type="Ljung-Box")
plot.ts(dx)

fitdx = auto.arima(dx[1:238],ic = "aic",test = "adf",
                   max.p = 20,max.q = 20,max.P = 20,max.Q = 20,
                   max.d = 5,max.D = 5)
summary(fitdx)
a<-
plot(forecast(fitdx,h=5))
plot.ts(dx)

plot(fitdx$fitted,x=1:238,type="l")

Box.test(fitdx$residuals, lag=20, type="Ljung-Box")

####
# holt_dx<-holt(dx,h=5)
# Box.test(holt_dx$residuals, lag=20, type="Ljung-Box")
# plot.ts(holt_dx$residuals)
####

##
HWsmoothig<-HoltWinters(dx, gamma=FALSE)
plot(HWsmoothig)
##

#find the diff

ndiffs(data.air$patient)
dp<-data.air$patient
acf(dp,lag.max = 100)
pacf(dp,lag.max = 100)
Box.test(dp, lag=20, type="Ljung-Box")


fitdp = auto.arima(dp[1:239],ic = "aic",test = "adf",
                   max.p = 20,max.q = 20,max.P = 20,max.Q = 20,
                   max.d = 5,max.D = 5)
summary(fitdp)
plot(forecast(fitdp,h=5))
plot.ts(dp,ylim=c(0,35))
Box.test(fitdp$residuals, lag=20, type="Ljung-Box")
# 
# dp1<-diff(data.air$patient,1)
# acf(dp1,lag.max = 100)
# pacf(dp1,lag.max = 100)
# Box.test(dp1, lag=20, type="Ljung-Box")
# plot.ts(dp1)


patientHWsmoothig<-HoltWinters(dp, gamma=FALSE)
plot(patientHWsmoothig)























































#moving average data

m2<-stats::filter(x,filter=c(rep(1/2,2)))%>%na.omit()
plot.ts(m2)
acf(m2,lag.max = 100)
m3<-filter(x,filter=c(rep(1/3,3)))

m4<-filter(x,filter=c(rep(1/4,4)))
m5<-filter(x,filter=c(rep(1/5,5)))
m6<-filter(x,filter=c(rep(1/6,6)))
m7<-filter(x,filter=c(rep(1/7,7)))

plot(y=x,x=data.air$date,type="l",xlab="time",ylab="ppm",main="CO")
abline(reg=lm(m5~data.air$date),col="red")

dm2<-diff(m2,1)
acf(dm2,lag.max = 100)
dm3<-diff(m3)
dm4<-diff(m4)
dm5<-diff(m5)
dm6<-diff(m6)
dm7<-diff(m7)  
# 
# plot(y=dm5,x=data.air$date[-365],type = "l")
# abline(reg=lm(dm5~data.air$date[-365]),col="red")

plot(y=dm5,x=data.air$date[-249],type = "l")
abline(reg=lm(dm5~data.air$date[-249]),col="red")


acf(x, lag.max = 100)
