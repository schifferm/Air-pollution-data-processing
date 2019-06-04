library(tseries)
library(forecast)
library(xts)
library(urca)
library(TSA)
#CO
ts.plot(data.air$CO)
data.air$CO %>% ur.kpss() %>% summary()
data.air$CO %>%adf.test() 
acf(data.air$CO,lag.max = 50)
pacf(data.air$CO,lag.max = 20)
Box.test(data.air$CO,lag =7,type="Ljung-Box")#ACF TEST
plot(x=data.air$date,y=data.air$CO,type="l")
ndiffs(data.air$CO)
eacf(data.air$CO)
#create ar model and fit values
ar_1_CO<-arima(data.air$CO, order=c(1,0,0))
arima(data.air$CO, order=c(1,1,0))
arima(diff(data.air$CO,1), order=c(1,0,0))
plot(ar_1_CO$residuals)
plot(data.air$CO,type="l")
lines(data.air$CO-ar_1_CO$residuals,col="red")
fit_CO<-fitted(ar_1_CO)

data.air$CO %>% diff(.,differences = 1)%>% ur.kpss() %>% summary()
data.air$CO %>% diff(.,differences = 1)%>%adf.test() 
data.air$CO %>% diff(.,differences = 1)%>%stats::acf()
data.air$CO %>% diff(.,differences = 1)%>%acf()
data.air$CO %>% diff(.,differences = 1)%>%pacf()
data.air$CO %>% diff(.,differences = 1)%>%eacf()
Box.test(diff(data.air$CO,differences = 1),lag = 7, type="Ljung-Box")#ACF TEST
plot(x=data.air$date[1:248],y=diff(data.air$CO,differences = 1),type="l")

ar_dCO<-arima(diff(data.air$CO,differences = 1), order=c(1,0,1))
plot(ar_dCO$residuals)
plot(data.air$CO,type="l")
lines(data.air$CO[1:248]-ar_dCO$residuals,col="red")
#SO2
ts.plot(data.air$SO2)
data.air$SO2 %>% ur.kpss() %>% summary()
data.air$SO2 %>%adf.test() 
acf(data.air$SO2)
pacf(data.air$SO2)
Box.test(data.air$SO2,lag =7,type="Ljung-Box")#ACF TEST
plot(x=data.air$date,y=data.air$SO2,type="l")
ndiffs(data.air$SO2)

eacf(data.air$SO2)
#create ar model and fit values
ar_1_SO2<-arima(data.air$SO2, order=c(1,0,0))
plot(ar_1_SO2$residuals)
plot(data.air$SO2,type="l")
lines(data.air$SO2-ar_1_SO2$residuals,col="red")
fit_SO2<-fitted(ar_1_SO2)

data.air$SO2 %>% diff(.,differences = 1)%>% ur.kpss() %>% summary()
data.air$SO2 %>% diff(.,differences = 1)%>%adf.test() 
data.air$SO2 %>% diff(.,differences = 1)%>%acf()
data.air$SO2 %>% diff(.,differences = 1)%>%pacf()
Box.test(diff(data.air$SO2,differences = 1),lag = 7, type="Ljung-Box")#ACF TEST
plot(x=data.air$date[1:248],y=diff(data.air$SO2,differences = 1),type="l")

#O3
ts.plot(data.air$O3)
data.air$O3 %>% ur.kpss() %>% summary()
data.air$O3 %>%adf.test() 
acf(data.air$O3,lag.max = 30)
pacf(data.air$O3,lag.max = 30)
Box.test(data.air$O3,lag =7,type="Ljung-Box")#ACF TEST
plot(x=data.air$date,y=data.air$O3,type="l")
ndiffs(data.air$O3)

eacf(data.air$O3)
#create ar model and fit values
ar_1_O3<-arima(data.air$O3, order=c(1,0,1))
plot(ar_1_O3$residuals)
plot(data.air$O3,type="l")
lines(data.air$O3-ar_1_O3$residuals,col="red")
fit_O3<-fitted(ar_1_O3)
#data.air$O3 %>% diff(.,differences = 1)%>% ur.kpss() %>% summary()
#data.air$O3 %>% diff(.,differences = 1)%>%adf.test() 
#data.air$O3 %>% diff(.,differences = 1)%>%acf()
#Box.test(diff(data.air$O3,differences = 1),lag = 7, type="Ljung-Box")#ACF TEST
#plot(x=data.air$date[1:248],y=diff(data.air$O3,differences = 1),type="l")

#PM2.5
ts.plot(data.air$PM2.5)
data.air$PM2.5 %>% ur.kpss() %>% summary()
data.air$PM2.5 %>%adf.test() 
acf(data.air$PM2.5,lag.max = 50)
pacf(data.air$PM2.5,lag.max = 50)
Box.test(data.air$PM2.5,lag =7,type="Ljung-Box")#ACF TEST
plot(x=data.air$date,y=data.air$PM2.5,type="l")
ndiffs(data.air$PM2.5)

eacf(data.air$PM2.5)
#create ar model and fit values
ar_1_PM2.5<-arima(data.air$PM2.5, order=c(1,0,2))
plot(ar_1_PM2.5$residuals)
plot(data.air$PM2.5,type="l")
lines(data.air$PM2.5-ar_1_PM2.5$residuals,col="red")
fit_PM2.5<-fitted(ar_1_PM2.5)

data.air$PM2.5 %>% diff(.,differences = 1)%>% ur.kpss() %>% summary()
data.air$PM2.5 %>% diff(.,differences = 1)%>%adf.test() 
data.air$PM2.5 %>% diff(.,differences = 1)%>%acf()
Box.test(diff(data.air$PM2.5,differences = 1),lag = 7, type="Ljung-Box")#ACF TEST
plot(x=data.air$date[1:248],y=diff(data.air$PM2.5,differences = 1),type="l")

#PM10
ts.plot(data.air$PM10)
data.air$PM10 %>% ur.kpss() %>% summary()
data.air$PM10 %>%adf.test() 
acf(data.air$PM10,lag.max = 50)
pacf(data.air$PM10,lag.max = 50)
Box.test(data.air$PM10,lag =7,type="Ljung-Box")#ACF TEST
plot(x=data.air$date,y=data.air$PM10,type="l")
ndiffs(data.air$PM10)

eacf(data.air$PM10)
#create ar model and fit values
ar_1_PM10<-arima(data.air$PM10, order=c(1,0,2))
plot(ar_1_PM10$residuals)
plot(data.air$PM10,type="l")
lines(data.air$PM10-ar_1_PM10$residuals,col="red")
fit_PM10<-fitted(ar_1_PM10)

data.air$PM10 %>% diff(.,differences = 1)%>% ur.kpss() %>% summary()
data.air$PM10 %>% diff(.,differences = 1)%>%adf.test() 
data.air$PM10 %>% diff(.,differences = 1)%>%acf()
Box.test(diff(data.air$PM10,differences = 1),lag = 7, type="Ljung-Box")#ACF TEST
plot(x=data.air$date[1:248],y=diff(data.air$PM10,differences = 1),type="l")

#NO2
ts.plot(data.air$NO2)
data.air$NO2 %>% ur.kpss() %>% summary()
data.air$NO2 %>%adf.test() 
acf(data.air$NO2,lag.max = 50)
pacf(data.air$NO2,lag.max = 50)
Box.test(data.air$NO2,lag =7,type="Ljung-Box")#ACF TEST
plot(x=data.air$date,y=data.air$NO2,type="l")
ndiffs(data.air$NO2)

eacf(data.air$NO2)
#create ar model and fit values
ar_1_NO2<-arima(data.air$NO2, order=c(1,0,1))
plot(ar_1_NO2$residuals)
plot(data.air$NO2,type="l")
lines(data.air$NO2-ar_1_NO2$residuals,col="red")
fit_NO2<-fitted(ar_1_NO2)

data.air$NO2 %>% diff(.,differences = 1)%>% ur.kpss() %>% summary()
data.air$NO2 %>% diff(.,differences = 1)%>%adf.test() 
data.air$NO2 %>% diff(.,differences = 1)%>%acf()
Box.test(diff(data.air$NO2,differences = 1),lag = 7, type="Ljung-Box")#ACF TEST
plot(x=data.air$date[1:248],y=diff(data.air$NO2,differences = 1),type="l")

#NO
data.air$NO %>% ur.kpss() %>% summary()
data.air$NO %>%adf.test() 
acf(data.air$NO)
pacf(data.air$NO)
Box.test(data.air$NO,lag =7,type="Ljung-Box")#ACF TEST
plot(x=data.air$date,y=data.air$NO,type="l")
ndiffs(data.air$NO)

eacf(data.air$NO)
#create ar model and fit values
ar_1_NO<-arima(data.air$NO, order=c(1,0,1))
plot(ar_1_NO$residuals)
plot(data.air$NO,type="l")
lines(data.air$NO-ar_1_NO$residuals,col="red")
fit_NO<-fitted(ar_1_NO)

#data.air$NO %>% diff(.,differences = 1)%>% ur.kpss() %>% summary()
#data.air$NO %>% diff(.,differences = 1)%>%adf.test() 
#data.air$NO %>% diff(.,differences = 1)%>%acf()
#Box.test(diff(data.air$NO,differences = 1),lag = 7, type="Ljung-Box")#ACF TEST
#plot(x=data.air$date[1:248],y=diff(data.air$NO,differences = 1),type="l")

#combined data
names(data.air)
fit_data<-data.frame(date=data.air$date,
                     SO2=fit_SO2,
                     CO=fit_CO,
                     O3=fit_O3,
                     PM2.5=fit_PM2.5,
                     PM10=fit_PM10,
                     NO2=fit_NO2,
                     NO=fit_NO,
                     RAIN=data.air$RAIN,
                     TEMP=data.air$TEMP,
                     RH=data.air$RH,
                     patient=data.air$patient,
                     time=data.air$time,
                     allergy=data.air$allergy,
                     hives=data.air$hives
                     )
