library(mgcv)
library(xtable)

write.csv(data.air_dow,"daily_withpatient_k.csv",fileEncoding = "utf-8",
          row.names=FALSE)
#lag&move average
COm<-mvdata(data.air_dow$CO)
SO2m<-mvdata(data.air_dow$SO2)
O3m<-mvdata(data.air_dow$O3)
PM2.5m<-mvdata(data.air_dow$PM2.5)
PM10m<-mvdata(data.air_dow$PM10)
NO2m<-mvdata(data.air_dow$NO2)
NOm<-mvdata(data.air_dow$NO)

TEMPm<-mvdata(data.air_dow$TEMP)
RHm<-mvdata(data.air_dow$RH)
RAINm<-mvdata(data.air_dow$rain)
WINDm<-mvdata(data.air_dow$WIND)
WS_HRm<-mvdata(data.air_dow$WS_HR)
######
gamair0<-function(patientcol,airdata1,data,t,r){
  result<-NULL
  res<-NULL
  p<-data[,patientcol]
  z1<-data.frame(p.pv=NA,mv2=NA,mv3=NA,mv4=NA,mv5=NA,mv6=NA,mv7=NA)
  b1<-data.frame(beta=NA,mv2=NA,mv3=NA,mv4=NA,mv5=NA,mv6=NA,mv7=NA)
  zz1<-data.frame(s.pv=NA,mv2=NA,mv3=NA,mv4=NA,mv5=NA,mv6=NA,mv7=NA)
  zz2<-data.frame(s.pv=NA,mv2=NA,mv3=NA,mv4=NA,mv5=NA,mv6=NA,mv7=NA)
  zz3<-data.frame(s.pv=NA,mv2=NA,mv3=NA,mv4=NA,mv5=NA,mv6=NA,mv7=NA)
  for(i in 1:length(airdata1)){
    air1<-airdata1[,i] 
    temp<-t[,1]
    rh<-r[,1]
    k<-0
    for (k in 0:7){
      result<-gam(as.numeric(p[(k+1):length(p)])~air1[1:(length(p)-k)]
                  +s(as.numeric(temp[1:(length(p)-k)]),bs = "cc",sp = 3,k =4)
                  +s(as.numeric(rh[1:(length(p)-k)]),bs = "cc",sp = 4,k =4)
                  +s(as.numeric(data$time[(k+1):length(p)]),bs = "cc",sp =4,k=4)
                  ,
                  data = data,
                  family = "poisson")
      res<-summary(result)
      z1[(k+1),i]<-round(res$p.pv[2],4)
      b1[(k+1),i]<-round(res$p.coeff[2],4)
      zz1[(k+1),i]<-round(res$s.pv[1],4)
      zz2[(k+1),i]<-round(res$s.pv[2],4)
      zz3[(k+1),i]<-round(res$s.pv[3],4)
    }
  }
  return(list(z1,b1,zz1,zz2,zz3))
}
#######NO RAIN WIND
CO_allergy_dow_NORW<-gamair0(10,COm,data.air_dow,TEMPm,RHm)
SO2_allergy_dow_NORW<-gamair0(10,SO2m,data.air_dow,TEMPm,RHm)
O3_allergy_dow_NORW<-gamair0(10,O3m,data.air_dow,TEMPm,RHm)
PM2.5_allergy_dow_NORW<-gamair0(10,PM2.5m,data.air_dow,TEMPm,RHm)
PM10_allergy_dow_NORW<-gamair0(10,PM10m,data.air_dow,TEMPm,RHm)
NO_allergy_dow_NORW<-gamair0(10,NOm,data.air_dow,TEMPm,RHm)
NO2_allergy_dow_NORW<-gamair0(10,NO2m,data.air_dow,TEMPm,RHm)

xtable(CO_allergy_dow_NORW[[1]],digits =3)
xtable(SO2_allergy_dow_NORW[[1]],digits =3)
xtable(O3_allergy_dow_NORW[[1]],digits =3)
xtable(PM2.5_allergy_dow_NORW[[1]],digits =3)
xtable(PM10_allergy_dow_NORW[[1]],digits =3)
xtable(NO_allergy_dow_NORW[[1]],digits =3)
xtable(NO2_allergy_dow_NORW[[1]],digits =3)

CO_hives_dow_NORW<-gamair0(11,COm,data.air_dow,TEMPm,RHm)
SO2_hives_dow_NORW<-gamair0(11,SO2m,data.air_dow,TEMPm,RHm)
O3_hives_dow_NORW<-gamair0(11,O3m,data.air_dow,TEMPm,RHm)
PM2.5_hives_dow_NORW<-gamair0(11,PM2.5m,data.air_dow,TEMPm,RHm)
PM10_hives_dow_NORW<-gamair0(11,PM10m,data.air_dow,TEMPm,RHm)
NO_hives_dow_NORW<-gamair0(11,NOm,data.air_dow,TEMPm,RHm)
NO2_hives_dow_NORW<-gamair0(11,NO2m,data.air_dow,TEMPm,RHm)


xtable(CO_hives_dow_NORW[[1]],digits =3)
xtable(SO2_hives_dow_NORW[[1]],digits =3)
xtable(O3_hives_dow_NORW[[1]],digits =3)
xtable(PM2.5_hives_dow_NORW[[1]],digits =3)
xtable(PM10_hives_dow_NORW[[1]],digits =3)
xtable(NO_hives_dow_NORW[[1]],digits =3)
xtable(NO2_hives_dow_NORW[[1]],digits =3)
#######
gamair1<-function(patientcol,airdata1,data,t,r,ra,ws){
  result<-NULL
  res<-NULL
  p<-data[,patientcol]
  z1<-data.frame(p.pv=NA,mv2=NA,mv3=NA,mv4=NA,mv5=NA,mv6=NA,mv7=NA)
  b1<-data.frame(beta=NA,mv2=NA,mv3=NA,mv4=NA,mv5=NA,mv6=NA,mv7=NA)
  zz1<-data.frame(s.pv=NA,mv2=NA,mv3=NA,mv4=NA,mv5=NA,mv6=NA,mv7=NA)
  zz2<-data.frame(s.pv=NA,mv2=NA,mv3=NA,mv4=NA,mv5=NA,mv6=NA,mv7=NA)
  zz3<-data.frame(s.pv=NA,mv2=NA,mv3=NA,mv4=NA,mv5=NA,mv6=NA,mv7=NA)
  zz4<-data.frame(s.pv=NA,mv2=NA,mv3=NA,mv4=NA,mv5=NA,mv6=NA,mv7=NA)
  zz5<-data.frame(s.pv=NA,mv2=NA,mv3=NA,mv4=NA,mv5=NA,mv6=NA,mv7=NA)
  for(i in 1:length(airdata1)){
    air1<-airdata1[,i] 
    temp<-t[,1]
    rh<-r[,1]
    rain<-ra[,1]
    wind<-ws[,1]
    k<-0
    for (k in 0:7){
      result<-gam(as.numeric(p[(k+1):length(p)])~air1[1:(length(p)-k)]
                  +s(as.numeric(temp[1:(length(p)-k)]),bs = "cc",sp = 3,k =4)
                  +s(as.numeric(rh[1:(length(p)-k)]),bs = "cc",sp = 4,k =4)
                  +s(as.numeric(data$time[(k+1):length(p)]),bs = "cc",sp =4,k=4)
                  +s(as.numeric(rain[1:(length(p)-k)]),bs = "cc",sp =4,k=4)
                  +s(as.numeric(wind[1:(length(p)-k)]),bs = "cc",sp =4,k=4)
                  ,
                  data = data,
                  family = "poisson")
      res<-summary(result)
      z1[(k+1),i]<-round(res$p.pv[2],4)
      b1[(k+1),i]<-round(res$p.coeff[2],4)
      zz1[(k+1),i]<-round(res$s.pv[1],4)
      zz2[(k+1),i]<-round(res$s.pv[2],4)
      zz3[(k+1),i]<-round(res$s.pv[3],4)
      zz4[(k+1),i]<-round(res$s.pv[4],4)
      zz5[(k+1),i]<-round(res$s.pv[5],4)
    }
  }
  return(list(z1,b1,zz1,zz2,zz3,zz4,zz5))
}
######
#ALL covariates
CO_allergy_dow<-gamair1(10,COm,data.air_dow,TEMPm,RHm,RAINm,WS_HRm)
SO2_allergy_dow<-gamair1(10,SO2m,data.air_dow,TEMPm,RHm,RAINm,WS_HRm)
O3_allergy_dow<-gamair1(10,O3m,data.air_dow,TEMPm,RHm,RAINm,WS_HRm)
PM2.5_allergy_dow<-gamair1(10,PM2.5m,data.air_dow,TEMPm,RHm,RAINm,WS_HRm)
PM10_allergy_dow<-gamair1(10,PM10m,data.air_dow,TEMPm,RHm,RAINm,WS_HRm)
NO_allergy_dow<-gamair1(10,NOm,data.air_dow,TEMPm,RHm,RAINm,WS_HRm)
NO2_allergy_dow<-gamair1(10,NO2m,data.air_dow,TEMPm,RHm,RAINm,WS_HRm)

xtable(CO_allergy_dow[[1]],digits =3)
xtable(SO2_allergy_dow[[1]],digits =3)
xtable(O3_allergy_dow[[1]],digits =3)
xtable(PM2.5_allergy_dow[[1]],digits =3)
xtable(PM10_allergy_dow[[1]],digits =3)
xtable(NO_allergy_dow[[1]],digits =3)
xtable(NO2_allergy_dow[[1]],digits =3)

CO_hives_dow<-gamair1(11,COm,data.air_dow,TEMPm,RHm,RAINm,WS_HRm)
SO2_hives_dow<-gamair1(11,SO2m,data.air_dow,TEMPm,RHm,RAINm,WS_HRm)
O3_hives_dow<-gamair1(11,O3m,data.air_dow,TEMPm,RHm,RAINm,WS_HRm)
PM2.5_hives_dow<-gamair1(11,PM2.5m,data.air_dow,TEMPm,RHm,RAINm,WS_HRm)
PM10_hives_dow<-gamair1(11,PM10m,data.air_dow,TEMPm,RHm,RAINm,WS_HRm)
NO_hives_dow<-gamair1(11,NOm,data.air_dow,TEMPm,RHm,RAINm,WS_HRm)
NO2_hives_dow<-gamair1(11,NO2m,data.air_dow,TEMPm,RHm,RAINm,WS_HRm)


xtable(CO_hives_dow[[1]],digits =3)
xtable(SO2_hives_dow[[1]],digits =3)
xtable(O3_hives_dow[[1]],digits =3)
xtable(PM2.5_hives_dow[[1]],digits =3)
xtable(PM10_hives_dow[[1]],digits =3)
xtable(NO_hives_dow[[1]],digits =3)
xtable(NO2_hives_dow[[1]],digits =3)


##############################################
#smooth
#allergy
O3RESs<-gamairs(10,O3m,data.air,TEMPm,RHm)
PM2.5RESs<-gamairs(10,PM2.5m,data.air,TEMPm,RHm)
PM10RESs<-gamairs(10,PM10m,data.air,TEMPm,RHm)
CORESs<-gamairs(10,COm,data.air,TEMPm,RHm)
NORESs<-gamairs(10,NOm,data.air,TEMPm,RHm)
NO2RESs<-gamairs(10,NO2m,data.air,TEMPm,RHm)
SO2RESs<-gamairs(10,SO2m,data.air,TEMPm,RHm)
#hive
O3RESs<-gamairs(11,O3m,data.air,TEMPm,RHm)
PM2.5RESs<-gamairs(11,PM2.5m,data.air,TEMPm,RHm)
PM10RESs<-gamairs(11,PM10m,data.air,TEMPm,RHm)
CORESs<-gamairs(11,COm,data.air,TEMPm,RHm)
NORESs<-gamairs(11,NOm,data.air,TEMPm,RHm)
NO2RESs<-gamairs(11,NO2m,data.air,TEMPm,RHm)
SO2RESs<-gamairs(11,SO2m,data.air,TEMPm,RHm)

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

plot(data.air_dow[,c(2:8)])
cor(data.air_dow[,c(2:8)], method = c("pearson", "kendall", "spearman"))
library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = cor(data.air_dow[,c(2:8)], method = c("pearson", "kendall", "spearman")), col = col, symm = TRUE)
heatmap(x = cor(data.air_dow[,c(12:14,16)], method = c("pearson", "kendall", "spearman")), col = col, symm = TRUE)

#choose one 

k=4
air1<-SO2m$m1
data<-data.air_dow
temp<-TEMPm[,1]
rh<-RHm[,1]
rain<-RAINm[,1]
wind<-WS_HRm[,1]
p<-data.air_dow$allergy
result<-gam(as.numeric(p[(k+1):length(p)])~air1[1:(length(p)-k)]
            +s(as.numeric(temp[1:(length(p)-k)]),bs = "cc",sp = 3,k =4)
            +s(as.numeric(rh[1:(length(p)-k)]),bs = "cc",sp = 4,k =4)
            +s(as.numeric(data$time[(k+1):length(p)]),bs = "cc",sp =4,k=4)
            +s(as.numeric(rain[1:(length(p)-k)]),bs = "cc",sp =4,k=4)
            +s(as.numeric(wind[1:(length(p)-k)]),bs = "cc",sp =4,k=4)
            ,
            data = data,
            family = "poisson")
summary(result)
plot(result)

plot(result,xlab="ppb",ylab="",main = "smooth curve of CO")
plot(result,xlab="°C",ylab="",main = "smooth curve of temperature")
plot(result,xlab="Relative humidity",ylab="",main = "smooth curve of relative humidity")
plot(result,xlab="2017/1/1~2017/12/31 weekdays",ylab="",main = "smooth curve of time trends")
plot(result,xlab="mm",ylab="",main = "smooth curve of RAINFALL")
plot(result,xlab="m/sec",ylab="",main = "smooth curve of wind speed")

results<-gam(as.numeric(p[(k+1):length(p)])~
               s(as.numeric(air1[1:(length(p)-k)]),bs = "cc",sp = 4,k =4)
             +s(as.numeric(temp[1:(length(p)-k)]),bs = "cc",sp = 3,k =4)
             +s(as.numeric(rh[1:(length(p)-k)]),bs = "cc",sp = 4,k =4)
             +s(as.numeric(data$time[(k+1):length(p)]),bs = "cc",sp =4,k=4)
             ,
             data = data,
             family = "poisson")
summary(results)