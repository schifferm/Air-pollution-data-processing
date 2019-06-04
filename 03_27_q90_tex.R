library(mgcv)
library(xtable)

write.csv(data.air_max,"dailyQ90_withpatient.csv",fileEncoding = "utf-8",
          row.names=FALSE)
#lag&move average
COm<-mvdata(data.air_max$CO)
SO2m<-mvdata(data.air_max$SO2)
O3m<-mvdata(data.air_max$O3)
PM2.5m<-mvdata(data.air_max$PM2.5)
PM10m<-mvdata(data.air_max$PM10)
NO2m<-mvdata(data.air_max$NO2)
NOm<-mvdata(data.air_max$NO)


CO_allergy_max<-gamair1(14,COm,data.air_max,TEMPm,RHm)
SO2_allergy_max<-gamair1(14,SO2m,data.air_max,TEMPm,RHm)
O3_allergy_max<-gamair1(14,O3m,data.air_max,TEMPm,RHm)
PM2.5_allergy_max<-gamair1(14,PM2.5m,data.air_max,TEMPm,RHm)
PM10_allergy_max<-gamair1(14,PM10m,data.air_max,TEMPm,RHm)
NO_allergy_max<-gamair1(14,NOm,data.air_max,TEMPm,RHm)
NO2_allergy_max<-gamair1(14,NO2m,data.air_max,TEMPm,RHm)

xtable(CO_allergy_max[[1]],digits =3)
xtable(SO2_allergy_max[[1]],digits =3)
xtable(O3_allergy_max[[1]],digits =3)
xtable(PM2.5_allergy_max[[1]],digits =3)
xtable(PM10_allergy_max[[1]],digits =3)
xtable(NO_allergy_max[[1]],digits =3)
xtable(NO2_allergy_max[[1]],digits =3)

xtable(CO_allergy_max[[2]],digits =3)
xtable(SO2_allergy_max[[2]],digits =3)
xtable(O3_allergy_max[[2]],digits =3)
xtable(PM2.5_allergy_max[[2]],digits =3)
xtable(PM10_allergy_max[[2]],digits =3)
xtable(NO_allergy_max[[2]],digits =3)
xtable(NO2_allergy_max[[2]],digits =3)

CO_hives_max<-gamair1(15,COm,data.air_max,TEMPm,RHm)
SO2_hives_max<-gamair1(15,SO2m,data.air_max,TEMPm,RHm)
O3_hives_max<-gamair1(15,O3m,data.air_max,TEMPm,RHm)
PM2.5_hives_max<-gamair1(15,PM2.5m,data.air_max,TEMPm,RHm)
PM10_hives_max<-gamair1(15,PM10m,data.air_max,TEMPm,RHm)
NO_hives_max<-gamair1(15,NOm,data.air_max,TEMPm,RHm)
NO2_hives_max<-gamair1(15,NO2m,data.air_max,TEMPm,RHm)



xtable(CO_hives_max[[1]],digits =3)
xtable(SO2_hives_max[[1]],digits =3)
xtable(O3_hives_max[[1]],digits =3)
xtable(PM2.5_hives_max[[1]],digits =3)
xtable(PM10_hives_max[[1]],digits =3)
xtable(NO_hives_max[[1]],digits =3)
xtable(NO2_hives_max[[1]],digits =3)

xtable(CO_hives_max[[2]],digits =3)
xtable(SO2_hives_max[[2]],digits =3)
xtable(O3_hives_max[[2]],digits =3)
xtable(PM2.5_hives_max[[2]],digits =3)
xtable(PM10_hives_max[[2]],digits =3)
xtable(NO_hives_max[[2]],digits =3)
xtable(NO2_hives_max[[2]],digits =3)

library(gridExtra)
grid.table(CO_hives_max[[1]])


##############################################
#smooth
#allergy
O3RESs<-gamairs(14,O3m,data.air,TEMPm,RHm)
PM2.5RESs<-gamairs(14,PM2.5m,data.air,TEMPm,RHm)
PM10RESs<-gamairs(14,PM10m,data.air,TEMPm,RHm)
CORESs<-gamairs(14,COm,data.air,TEMPm,RHm)
NORESs<-gamairs(14,NOm,data.air,TEMPm,RHm)
NO2RESs<-gamairs(14,NO2m,data.air,TEMPm,RHm)
SO2RESs<-gamairs(14,SO2m,data.air,TEMPm,RHm)
#hive
O3RESs<-gamairs(15,O3m,data.air,TEMPm,RHm)
PM2.5RESs<-gamairs(15,PM2.5m,data.air,TEMPm,RHm)
PM10RESs<-gamairs(15,PM10m,data.air,TEMPm,RHm)
CORESs<-gamairs(15,COm,data.air,TEMPm,RHm)
NORESs<-gamairs(15,NOm,data.air,TEMPm,RHm)
NO2RESs<-gamairs(15,NO2m,data.air,TEMPm,RHm)
SO2RESs<-gamairs(15,SO2m,data.air,TEMPm,RHm)

library(xtable)
xtable(CORESs[[1]],digits =3)
xtable(SO2RESs[[1]],digits =3)
xtable(O3RESs[[1]],digits =3)
xtable(PM2.5RESs[[1]],digits =3)
xtable(PM10RESs[[1]],digits =3)
xtable(NORESs[[1]],digits =3)
xtable(NO2RESs[[1]],digits =3)

xtable(CORESs[[2]],digits =3)
xtable(SO2RESs[[2]],digits =3)
xtable(O3RESs[[2]],digits =3)
xtable(PM2.5RESs[[2]],digits =3)
xtable(PM10RESs[[2]],digits =3)
xtable(NORESs[[2]],digits =3)
xtable(NO2RESs[[2]],digits =3)

xtable(CORESs[[3]],digits =3)
xtable(SO2RESs[[3]],digits =3)
xtable(O3RESs[[3]],digits =3)
xtable(PM2.5RESs[[3]],digits =3)
xtable(PM10RESs[[3]],digits =3)
xtable(NORESs[[3]],digits =3)
xtable(NO2RESs[[3]],digits =3)

xtable(CORESs[[4]],digits =3)
xtable(SO2RESs[[4]],digits =3)
xtable(O3RESs[[4]],digits =3)
xtable(PM2.5RESs[[4]],digits =3)
xtable(PM10RESs[[4]],digits =3)
xtable(NORESs[[4]],digits =3)
xtable(NO2RESs[[4]],digits =3)



#choose one 
k=3
air1<-PM2.5m$m1
data<-data.air
temp<-TEMPm$m1
rh<-RHm$m1
p<-data.air$allergy
results<-gam(as.numeric(p[(k+1):length(p)])~
               s(as.numeric(air1[1:(length(p)-k)]),bs = "cc",sp = 4,k =4)
             +s(as.numeric(temp[1:(length(p)-k)]),bs = "cc",sp = 3,k =4)
             +s(as.numeric(rh[1:(length(p)-k)]),bs = "cc",sp = 4,k =4)
             +s(as.numeric(data$time[(k+1):length(p)]),bs = "cc",sp =4,k=4)
             ,
             data = data,
             family = "poisson")
summary(results)
plot(results,xlab="ppb",ylab="",main = "smooth curve of PM2.5")
plot(results,xlab="°C",ylab="",main = "smooth curve of temperature")
plot(results,xlab="Relative humidity",ylab="",main = "smooth curve of relative humidity")
plot(results,xlab="2017/1/1~2017/12/31 weekdays",ylab="",main = "smooth curve of time trends")

