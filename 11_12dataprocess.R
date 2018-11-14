library(tseries)
library(forecast)
library(xts)
plot(outpatient_csv$`708&995.3`,pch=20,xlab="time",ylab="patients"
     ,main="Frequency plot of patient")
m1<-filter(outpatient_csv$`708&995.3`,filter=c(rep(1/5,5)))
lines(m1,col="red",cex=1.5)
mO3<-filter(O3_weekday$Mean,filter=c(rep(1/5,5)))
lines(mO3*1/5,col="blue",cex=1.5)
#original data
adf.test(outpatient_csv$`708&995.3`)#UNIT ROOT TEST
ndiffs(outpatient_csv$`708&995.3`)
acf(outpatient_csv$`708&995.3`)
pacf(outpatient_csv$`708&995.3`)
ar_patient<-ar(outpatient_csv$`708&995.3`)# choose order

arima_patient_lag5<-arima(outpatient_csv$`708&995.3`,order=c(5,0,0),
                          seasonal=list(order=c(1,1,0),period=5))
tsdiag(arima_patient_lag5)

arima_patient_lag3<-arima(outpatient_csv$`708&995.3`,order=c(3,0,0),
                          seasonal=list(order=c(1,1,0),period=3)) 

tsdiag(arima_patient_lag3)

#O3
plot(O3_weekday$Mean,pch=20,xlab="time",ylab="O3")
qqnorm(O3_weekday$Mean,pch=20,main="Normal Q-Q plot of O3")
qqline(O3_weekday$Mean)

adf.test(O3_weekday$Mean)#UNIT ROOT TEST
ndiffs(O3_weekday$Mean)
acf(O3_weekday$Mean)
pacf(O3_weekday$Mean)
ar_O3<-ar(O3_weekday$Mean)# choose order
  
arima_O3_lag3<-arima(O3_weekday$Mean,order=c(3,0,0),
                            seasonal=list(order=c(1,1,0),period=3))
tsdiag(arima_O3_lag3)
  
arima_O3_lag6<-arima(O3_weekday$Mean,order=c(6,0,0),
                            seasonal=list(order=c(1,1,0),period=6)) 
tsdiag(arima_O3_lag6)

#CO
plot(CO_weekday$Mean,pch=20,xlab="time",ylab="CO")
qqnorm(CO_weekday$Mean,pch=20,main="Normal Q-Q plot of CO")
qqline(CO_weekday$Mean)

adf.test(CO_weekday$Mean)#UNIT ROOT TEST
ndiffs(CO_weekday$Mean)
dCO<-diff(CO_weekday$Mean,1)
adf.test(dCO)#UNIT ROOT TEST
acf(dCO)
pacf(dCO)
ar_CO<-ar(dCO)# choose order

arima_CO_lag3<-arima(dCO,order=c(3,1,0),
                     seasonal=list(order=c(1,1,0),period=3))
tsdiag(arima_CO_lag3)

arima_CO_lag8<-arima(dCO,order=c(8,1,0),
                     seasonal=list(order=c(1,1,0),period=8)) 
tsdiag(arima_CO_lag8)

#NO
plot(NO_weekday$Mean,pch=20,xlab="time",ylab="NO")
qqnorm(NO_weekday$Mean,pch=20,main="Normal Q-Q plot of NO")
qqline(NO_weekday$Mean)

adf.test(NO_weekday$Mean)#UNIT ROOT TEST
ndiffs(NO_weekday$Mean)
acf(NO_weekday$Mean)
pacf(NO_weekday$Mean)
ar_NO<-ar(NO_weekday$Mean)# choose order

arima_NO_lag3<-arima(NO_weekday$Mean,order=c(3,0,0),
                     seasonal=list(order=c(1,1,0),period=3))
tsdiag(arima_NO_lag3)

arima_NO_lag1<-arima(NO_weekday$Mean,order=c(1,0,0),
                     seasonal=list(order=c(1,1,0),period=1)) 
tsdiag(arima_NO_lag1)

#NO2
plot(NO2_weekday$Mean,pch=20,xlab="time",ylab="NO2")
qqnorm(NO2_weekday$Mean,pch=20,main="Normal Q-Q plot of NO2")
qqline(NO2_weekday$Mean)

adf.test(NO2_weekday$Mean)#UNIT ROOT TEST
ndiffs(NO2_weekday$Mean)
dNO2<-diff(NO2_weekday$Mean,1)
adf.test(dNO2)
acf(dNO2)
pacf(dNO2)
ar_NO2<-ar(dNO2)# choose order

arima_NO2_lag3<-arima(dNO2,order=c(3,1,0),
                     seasonal=list(order=c(1,1,0),period=3))
tsdiag(arima_NO2_lag3)

arima_NO2_lag4<-arima(dNO2,order=c(4,1,0),
                     seasonal=list(order=c(1,1,0),period=4)) 
tsdiag(arima_NO2_lag4)

#PM10
plot(PM10_weekday$Mean,pch=20,xlab="time",ylab="PM10")
qqnorm(PM10_weekday$Mean,pch=20,main="Normal Q-Q plot of PM10")
qqline(PM10_weekday$Mean)

adf.test(PM10_weekday$Mean)#UNIT ROOT TEST
ndiffs(PM10_weekday$Mean)
dPM10<-diff(PM10_weekday$Mean,1)
acf(dPM10)
pacf(dPM10)
ar_PM10<-ar(dPM10)# choose order

arima_PM10_lag3<-arima(dPM10,order=c(3,1,0),
                     seasonal=list(order=c(1,1,0),period=3))
tsdiag(arima_PM10_lag3)

arima_PM10_lag10<-arima(dPM10,order=c(10,1,0),
                     seasonal=list(order=c(1,1,0),period=10)) 
tsdiag(arima_PM10_lag10)

#PM2.5
plot(PM2.5_weekday$Mean,pch=20,xlab="time",ylab="PM2.5")
qqnorm(PM2.5_weekday$Mean,pch=20,main="Normal Q-Q plot of PM2.5")
qqline(PM2.5_weekday$Mean)

adf.test(PM2.5_weekday$Mean)#UNIT ROOT TEST
ndiffs(PM2.5_weekday$Mean)
dPM2.5<-diff(PM2.5_weekday$Mean,1)
adf.test(dPM2.5)
acf(dPM2.5)
pacf(dPM2.5)
ar_PM2.5<-ar(dPM2.5)# choose order

arima_PM2.5_lag3<-arima(dPM2.5,order=c(3,1,0),
                     seasonal=list(order=c(1,1,0),period=3))
tsdiag(arima_PM2.5_lag3)

arima_PM2.5_lag8<-arima(PM2.5_weekday$Mean,order=c(8,1,0),
                     seasonal=list(order=c(1,1,0),period=8)) 
tsdiag(arima_PM2.5_lag8)

#SO2
plot(SO2_weekday$Mean,pch=20,xlab="time",ylab="SO2")
qqnorm(SO2_weekday$Mean,pch=20,main="Normal Q-Q plot of SO2")
qqline(SO2_weekday$Mean)

adf.test(SO2_weekday$Mean)#UNIT ROOT TEST
ndiffs(SO2_weekday$Mean)
dSO2<-diff(SO2_weekday$Mean)
adf.test(dSO2)
acf(SO2_weekday$Mean)
acf(dSO2)
pacf(SO2_weekday$Mean)
pacf(dSO2)
ar_SO2<-ar(SO2_weekday$Mean)# choose order
ar_dSO2<-ar(dSO2)
arima_SO2_lag0<-arima(SO2_weekday$Mean,order=c(0,0,0),
                     seasonal=list(order=c(1,1,0),period=0))
tsdiag(arima_SO2_lag0)

arima_SO2_lag09<-arima(dSO2,order=c(9,1,0),
                     seasonal=list(order=c(1,1,0),period=9)) 
tsdiag(arima_SO2_lag9)


