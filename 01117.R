library(mgcv)
data.air$time<-1:249
lag=2
air1<-O3m$m1
air2<-PM2.5m$m1
temp<-TEMPm$m1
rh<-RHm$m1
data.air
time<-data.air$time
length(time[1:(length(patient)-lag)])
length(patient[(lag+1):length(patient)])
length(rh[(lag+1):(length(patient))])
result<-gam(as.numeric(patient[(lag+1):length(patient)])~air2[1:(length(patient)-lag)]
              +s(as.numeric(temp[(lag+1):(length(patient))]),bs = "cc",sp = 3,k =4)
              +s(as.numeric(rh[(lag+1):(length(patient))]),bs = "cc",sp = 4,k =4)
              +s(as.numeric(time[(lag+1):length(patient)]),bs = "cc",sp =4,k=4),
            data = data,
            family = "poisson")

a<-summary(result)
plot(result,ylab="RR")
a$p.pv

##check knots palce
place.knots(data.air$TEMP,4)
place.knots(data.air$RH,4)

##
gam.check(result)
aa<-plot(result)
a
airdata1<-O3m
airdata2<-PM2.5m

gamair2<-function(patient,airdata1,airdata2,data,t,r){
  result<-NULL
  res<-NULL
  z1<-data.frame(p.pv=NA,mv2=NA,mv3=NA,mv4=NA,mv5=NA,mv6=NA,mv7=NA)
  z2<-data.frame(p.pv=NA,mv2=NA,mv3=NA,mv4=NA,mv5=NA,mv6=NA,mv7=NA)
  zz1<-data.frame(s.pv=NA,mv2=NA,mv3=NA,mv4=NA,mv5=NA,mv6=NA,mv7=NA)
  zz2<-data.frame(s.pv=NA,mv2=NA,mv3=NA,mv4=NA,mv5=NA,mv6=NA,mv7=NA)
  zz3<-data.frame(s.pv=NA,mv2=NA,mv3=NA,mv4=NA,mv5=NA,mv6=NA,mv7=NA)
  for(i in 1:length(airdata1)){
    air1<-airdata1[,i] 
    air2<-airdata2[,i] 
    temp<-t[,i]
    rh<-r[,i]
    for (k in 0:7){
      result<-gam(as.numeric(patient[(k+1):length(patient)])~air1[1:(length(patient)-k)]+air2[1:(length(patient)-k)]
                  +s(as.numeric(temp[1:(length(patient)-k)]),bs = "cc",sp = 3,k =3)
                  +s(as.numeric(rh[1:(length(patient)-k)]),bs = "cc",sp = 4,k =4)
                  +s(as.numeric(data$time[(k+1):length(patient)]),bs = "cc",sp =4,k=4)
                  ,
                  data = data,
                  family = "poisson")
      res<-summary(result)
      z1[(k+1),i]<-round(res$p.pv[2],4)
      z2[(k+1),i]<-round(res$p.pv[3],4)
      zz1[(k+1),i]<-round(res$s.pv[1],4)
      zz2[(k+1),i]<-round(res$s.pv[2],4)
      zz3[(k+1),i]<-round(res$s.pv[3],4)
    }
  }
  return(list(z1,z2,zz1,zz2,zz3))
}

res<-gamair1(patient,O3m,PM2.5m,data.air,TEMPm,RHm)

xtable(res[[1]])
xtable(res[[2]])
xtable(res[[3]])
xtable(res[[4]])
res[[1]]+res[[2]]
res[[3]]+res[[4]]

#########################################################univariate
gamair1<-function(patientcol,airdata1,data,t,r){
  result<-NULL
  res<-NULL
  p<-data.air[,patientcol]
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
############################################################

result1<-gam(as.numeric(patient[(k+1):length(patient)])~air1[1:(length(patient)-k)]
            +s(as.numeric(temp[1:(length(patient)-k)]),bs = "cc",sp = 3,k =4)
            +s(as.numeric(rh[1:(length(patient)-k)]),bs = "cc",sp = 4,k =4)
            +s(as.numeric(data$time[(k+1):length(patient)]),bs = "cc",sp =4,k=4)
            ,
            data = data,
            family = "poisson")
summary(result1)
plot(result1,xlab="°C",ylab="",main = "smooth curve of temperature")
plot(result1,xlab="Relative humidity",ylab="",main = "smooth curve of relative humidity")
plot(result1,xlab="2017/1/1~2017/12/31 weekdays",ylab="",main = "smooth curve of time trends")


k=3
air1<-O3m$m3
data<-data.air
temp<-TEMPm$m3
rh<-RHm$m3

result3<-gam(as.numeric(patient[(k+1):length(patient)])~air1[1:(length(patient)-k)]
             +s(as.numeric(temp[1:(length(patient)-k)]),bs = "cc",sp = 3,k =4)
             +s(as.numeric(rh[1:(length(patient)-k)]),bs = "cc",sp = 4,k =4)
             +s(as.numeric(data$time[(k+1):length(patient)]),bs = "cc",sp =4,k=4)
             ,
             data = data,
             family = "poisson")
summary(result3)



resultlinear<-gam(as.numeric(patient[(k+1):length(patient)])~air1[1:(length(patient)-k)]
            ,
            data = data,
            family = "poisson")

summary(resultlinear)

O3RES<-gamair1(patient,O3m,data.air,TEMPm,RHm)
PM2.5RES<-gamair1(patient,PM2.5m,data.air,TEMPm,RHm)
PM10RES<-gamair1(patient,PM10m,data.air,TEMPm,RHm)
CORES<-gamair1(patient,COm,data.air,TEMPm,RHm)
NORES<-gamair1(patient,NOm,data.air,TEMPm,RHm)
NO2RES<-gamair1(patient,NO2m,data.air,TEMPm,RHm)
SO2RES<-gamair1(patient,SO2m,data.air,TEMPm,RHm)

library(xtable)
xtable(CORES[[1]],digits =3)
xtable(SO2RES[[1]],digits =3)
xtable(O3RES[[1]],digits =3)
xtable(PM2.5RES[[1]],digits =3)
xtable(PM10RES[[1]],digits =3)
xtable(NORES[[1]],digits =3)
xtable(NO2RES[[1]],digits =3)

xtable(CORES[[2]],digits =3)
xtable(SO2RES[[2]],digits =3)
xtable(O3RES[[2]],digits =3)
xtable(PM2.5RES[[2]],digits =3)
xtable(PM10RES[[2]],digits =3)
xtable(NORES[[2]],digits =3)
xtable(NO2RES[[2]],digits =3)

xtable(CORES[[3]],digits =3)
xtable(SO2RES[[3]],digits =3)
xtable(O3RES[[3]],digits =3)
xtable(PM2.5RES[[3]],digits =3)
xtable(PM10RES[[3]],digits =3)
xtable(NORES[[3]],digits =3)
xtable(NO2RES[[3]],digits =3)

############################allsmooth
gamairs<-function(patientcol,airdata1,data,t,r){
  result<-NULL
  res<-NULL
  p<-data.air[,patientcol]
  z1<-data.frame(s.pv=NA,mv2=NA,mv3=NA,mv4=NA,mv5=NA,mv6=NA,mv7=NA)
  zz1<-data.frame(s.pv=NA,mv2=NA,mv3=NA,mv4=NA,mv5=NA,mv6=NA,mv7=NA)
  zz2<-data.frame(s.pv=NA,mv2=NA,mv3=NA,mv4=NA,mv5=NA,mv6=NA,mv7=NA)
  zz3<-data.frame(s.pv=NA,mv2=NA,mv3=NA,mv4=NA,mv5=NA,mv6=NA,mv7=NA)
  for(i in 1:length(airdata1)){
    air1<-airdata1[,i] 
    temp<-t[,1]
    rh<-r[,1]
    k<-0
    for (k in 0:7){
      result<-gam(as.numeric(p[(k+1):length(p)])~
                   s(as.numeric(air1[1:(length(p)-k)]),bs = "cc",sp = 4,k =4)
                  +s(as.numeric(temp[1:(length(p)-k)]),bs = "cc",sp = 3,k =4)
                  +s(as.numeric(rh[1:(length(p)-k)]),bs = "cc",sp = 4,k =4)
                  +s(as.numeric(data$time[(k+1):length(p)]),bs = "cc",sp =4,k=4)
                  ,
                  data = data,
                  family = "poisson")
      res<-summary(result)
      z1[(k+1),i]<-round(res$s.pv[1],4)
      zz1[(k+1),i]<-round(res$s.pv[2],4)
      zz2[(k+1),i]<-round(res$s.pv[3],4)
      zz3[(k+1),i]<-round(res$s.pv[4],4)
    }
  }
  return(list(z1,zz1,zz2,zz3))
}

O3RESs<-gamairs(12,O3m,data.air,TEMPm,RHm)
PM2.5RESs<-gamairs(12,PM2.5m,data.air,TEMPm,RHm)
PM10RESs<-gamairs(12,PM10m,data.air,TEMPm,RHm)
CORESs<-gamairs(12,COm,data.air,TEMPm,RHm)
NORESs<-gamairs(12,NOm,data.air,TEMPm,RHm)
NO2RESs<-gamairs(12,NO2m,data.air,TEMPm,RHm)
SO2RESs<-gamairs(12,SO2m,data.air,TEMPm,RHm)
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



