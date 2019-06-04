library(mgcv)
library(forecast)
library(urca)
library(tseries)
# result<-gam(as.numeric(patient[(k+1):length(patient)])~m7[ceiling(k/2):(length(air)-ceiling(k/2))]+s(as.numeric(CO[1:(length(sair)-k)]),bs = "cc",sp = 4),data = data)
# res<-summary(result_CO)
# plot(result_CO,se=T)
# res$p.pv[2]
# k<-1
# k<-2
# length((k+1):length(patient))
# length((1:(length(air)-1)))
# length(1:(length(sair)-k))
# ceiling(k/2)
# floor(k/2)
# patient[i+k:249]
# length(m7)
# air<-m7
# data<-data.air
# sair<-CO
# air[c(1:length(air),length(air):length(patient))]


gamair<-function(patient,airdata,data){
  result<-NULL
  res<-NULL
  z<-data.frame(p.pv=NA)
  zz<-data.frame(s.pv=NA,mv2=NA,mv3=NA,mv4=NA,mv5=NA,mv6=NA,mv7=NA)
  y<-data.frame(LjungBox.pv=NA)
  yy<-data.frame(adftest.pv=NA)
  zzz<-data.frame(res.adftest.pv=NA)
  for(i in 1:length(airdata)){
   air<-airdata[,i] 
   y[1,i]<-Box.test(air, lag=7, type="Ljung-Box")$p.value
   yy[1,i]<-adf.test(na.omit(air))$p.value
  for (k in 0:7){
    result<-gam(as.numeric(patient[(k+1):length(patient)])~
                  s(as.numeric(air[1:(length(patient)-k)]),bs = "cc",sp = 4),
                data = data,
                family = "poisson")
    res<-summary(result)
    z[(k+1),i]<-round(res$p.pv[2],4)
    zz[(k+1),i]<-round(res$s.pv,4)
    zzz[(k+1),i]<-adf.test(result$residuals)$p.value
  }
  }
  return(list(z,zz,y,yy,zzz))
}
mvdata<-function(airdata){
x=ts(airdata)
m2<-na.omit(filter(x,filter=c(rep(1/2,2))))
m3<-na.omit(filter(x,filter=c(rep(1/3,3))))
m4<-na.omit(filter(x,filter=c(rep(1/4,4))))
m5<-na.omit(filter(x,filter=c(rep(1/5,5))))
m6<-na.omit(filter(x,filter=c(rep(1/6,6))))
m7<-na.omit(filter(x,filter=c(rep(1/7,7))))
m<-data.frame(m1=x,m2=m2[1:length(airdata)],m3=m3[1:airdata],m4=m4[1:airdata],m5=m5[1:airdata],m6=m6[1:airdata],m7=m7[1:airdata])
return(m)
}

airdata<-data.air$CO
day<-7
mvdata<-function(airdata,day){
  x=ts(airdata)
  m<-data.frame(x=NA)
  for(i in 1:day){
  m[,i]<-na.omit(filter(x,filter=c(rep(1/i,i))))
  }
  return(m)
}
COm<-mvdata(data.air$CO,7)
filter(c(1:5),filter=c(rep(1/2,2)))

air<-diff(data.air$CO,ndiffs(data.air$CO))
air<-diff(data.air$CO,(ndiffs(data.air$CO)+1))

Box.test(air, lag=7, type="Ljung-Box")

air<-data.air$SO2
air<-diff(data.air$SO2,ndiffs(data.air$SO2))
air<-diff(data.air$SO2,(ndiffs(data.air$SO2)+1))
plot(air)
plot(data.air$SO2)

Box.test(air, lag=7, type="Ljung-Box")

air<-data.air$O3
air<-diff(data.air$O3,1)
Box.test(air, lag=7, type="Ljung-Box")

air<-diff(data.air$PM2.5,ndiffs(data.air$PM2.5))
Box.test(air, lag=7, type="Ljung-Box")

air<-diff(data.air$PM10,ndiffs(data.air$PM10))
Box.test(air, lag=7, type="Ljung-Box")

air<-diff(data.air$NO2,ndiffs(data.air$NO2))
Box.test(air, lag=7, type="Ljung-Box")

air<-data.air$NO
air<-diff(data.air$NO,1)

Box.test(air, lag=7, type="Ljung-Box")


data.air<-airandpatient_weekday
                
#lag&move average
COm<-mvdata(data.air$CO)
SO2m<-mvdata(data.air$SO2)
O3m<-mvdata(data.air$O3)
PM2.5m<-mvdata(data.air$PM2.5)
PM10m<-mvdata(data.air$PM10)
NO2m<-mvdata(data.air$NO2)
NOm<-mvdata(data.air$NO)
RAINm<-mvdata(data.air$RAIN)
TEMPm<-mvdata(data.air$TEMP)
RHm<-mvdata(data.air$RH)


CO_res<-gamair(patient,COm,data.air)
SO2_res<-gamair(patient,SO2m,data.air)
O3_res<-gamair(patient,O3m,data.air)
PM2.5_res<-gamair(patient,PM2.5m,data.air)
PM10_res<-gamair(patient,PM10m,data.air)
NO2_res<-gamair(patient,NO2m,data.air)
NO_res<-gamair(patient,NOm,data.air)
RAIN_res<-gamair(patient,RAINm,data.air)
TEMP_res<-gamair(patient,TEMPm,data.air)
RH_res<-gamair(patient,RHm,data.air)
library(xtable)

xtable(CO_res[[3]],digits=4)
xtable(CO_res[[2]],digits=4)
k=6
air<-COm$m1
sair=CO
result<-gam(as.numeric(patient[(k+1):length(patient)])~s(as.numeric(air[1:(length(patient)-k)]),
                                                         bs = "cc",sp = 4,k =4),
            data = data,
            family = "poisson")

summary(result)
##check knots palce
place.knots(data.air$CO,4)
which(sort(data.air$CO)==place.knots(data.air$CO,4)[3])
range(data.air$CO)
##
gam.check(result)
plot(result)


xtable(SO2_res[[3]],digits=4)
xtable(SO2_res[[2]],digits=4)

k=2
air<-SO2m$m1
sair=SO2
result<-gam(as.numeric(patient[(k+1):length(patient)])~s(as.numeric(air[1:(length(patient)-k)]),
                                                         bs = "cc",sp = 4,k = 3),
            data = data,
            family = "poisson")

summary(result)

plot(result)



xtable(O3_res[[3]],digits=4)
xtable(O3_res[[2]],digits=4)

k=5
air<-O3m$m2
sair=O3
result<-gam(as.numeric(patient[(k+1):length(patient)])~s(as.numeric(air[1:(length(patient)-k)]),
                                                         bs = "cc",sp = 4),
            data = data,
            family = "poisson")
gam.check(result)

a<-summary(result)

plot(result)

xtable(PM2.5_res[[3]],digits=4)
xtable(PM2.5_res[[2]],digits=4)

k=2
air<-PM2.5m$m5
sair=PM2.5
result<-gam(as.numeric(patient[(k+1):length(patient)])~s(as.numeric(air[1:(length(patient)-k)]),
                                                         bs = "cc",sp = 4,k = 3),
            data = data,
            family = "poisson")

summary(result)

plot(result)

xtable(PM10_res[[3]],digits=4)
xtable(PM10_res[[2]],digits=4)
k=1
air<-PM10m$m1
sair=PM10
result<-gam(as.numeric(patient[(k+1):length(patient)])~s(as.numeric(air[1:(length(patient)-k)]),
                                                         bs = "cc",sp = 4,k = 3),
            data = data,
            family = "poisson")

summary(result)

plot(result)

xtable(NO2_res[[3]],digits=4)
xtable(NO2_res[[2]],digits=4)
                                        
xtable(NO_res[[3]],digits=4)
xtable(NO_res[[2]],digits=4)

xtable(RAIN_res[[2]],digits=4)
xtable(TEMP_res[[2]],digits=4)
xtable(RH_res[[2]],digits=4)
################################################################

################################################################
LjungBox.test<-rbind(CO=CO_res[[3]],
                     SO2=SO2_res[[3]],
                     O3=O3_res[[3]],
                     PM2.5=PM2.5_res[[3]],
                     PM10=PM10_res[[3]],
                     NO2=NO2_res[[3]],
                     NO=NO_res[[3]])

DickeyFuller.test<-rbind(CO=CO_res[[4]],
                     SO2=SO2_res[[4]],
                     O3=O3_res[[4]],
                     PM2.5=PM2.5_res[[4]],
                     PM10=PM10_res[[4]],
                     NO2=NO2_res[[4]],
                     NO=NO_res[[4]])


CO_res[[5]]
SO2_res[[5]]
O3_res[[5]]
PM2.5_res[[5]]
PM10_res[[5]]
NO2_res[[5]]
NO_res[[5]]
