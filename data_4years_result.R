library(mgcv)
library(xtable)

airdata<-data_4years$O3
mvdata<-function(airdata){
  x=ts(airdata)
  m2<-na.omit(filter(x,filter=c(rep(1/2,2))))
  m3<-na.omit(filter(x,filter=c(rep(1/3,3))))
  m4<-na.omit(filter(x,filter=c(rep(1/4,4))))
  m5<-na.omit(filter(x,filter=c(rep(1/5,5))))
  m6<-na.omit(filter(x,filter=c(rep(1/6,6))))
  m7<-na.omit(filter(x,filter=c(rep(1/7,7))))
  m<-data.frame(m1=x,m2=m2[1:1673],m3=m3[1:1673],m4=m4[1:1673],m5=m5[1:1673],m6=m6[1:1673],m7=m7[1:1673])
  return(m)
}

#lag&move average
COm<-mvdata(data_4years$CO)
SO2m<-mvdata(data_4years$SO2)
O3m<-mvdata(data_4years$O3)
PM2.5m<-mvdata(data_4years$PM2.5)
PM10m<-mvdata(data_4years$PM10)
NO2m<-mvdata(data_4years$NO2)
NOm<-mvdata(data_4years$NO)

TEMPm<-mvdata(data_4years$TEMP)
RHm<-mvdata(data_4years$RH)
RAINm<-mvdata(data_4years$RAIN)
WS_HRm<-mvdata(data_4years$WS_HR)
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
                  +s(as.numeric(temp[1:(length(p)-k)]),bs="cs",sp = 3,k =4)
                  +s(as.numeric(rh[1:(length(p)-k)]),bs="cs",sp = 4,k =4)
                  +s(as.numeric(data$time[(k+1):length(p)]),bs="cs",sp =4,k=-1)
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

CO_urticaria_dow_NORW<-gamair0(11,COm,data.air_dow,TEMPm,RHm)
SO2_urticaria_dow_NORW<-gamair0(11,SO2m,data.air_dow,TEMPm,RHm)
O3_urticaria_dow_NORW<-gamair0(11,O3m,data.air_dow,TEMPm,RHm)
PM2.5_urticaria_dow_NORW<-gamair0(11,PM2.5m,data.air_dow,TEMPm,RHm)
PM10_urticaria_dow_NORW<-gamair0(11,PM10m,data.air_dow,TEMPm,RHm)
NO_urticaria_dow_NORW<-gamair0(11,NOm,data.air_dow,TEMPm,RHm)
NO2_urticaria_dow_NORW<-gamair0(11,NO2m,data.air_dow,TEMPm,RHm)


xtable(CO_urticaria_dow_NORW[[1]],digits =3)
xtable(SO2_urticaria_dow_NORW[[1]],digits =3)
xtable(O3_urticaria_dow_NORW[[1]],digits =3)
xtable(PM2.5_urticaria_dow_NORW[[1]],digits =3)
xtable(PM10_urticaria_dow_NORW[[1]],digits =3)
xtable(NO_urticaria_dow_NORW[[1]],digits =3)
xtable(NO2_urticaria_dow_NORW[[1]],digits =3)
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
                  +s(as.numeric(temp[1:(length(p)-k)]),bs="cs",sp = 3,k =4)
                  +s(as.numeric(rh[1:(length(p)-k)]),bs="cs",sp = 4,k =4)
                  +s(as.numeric(data$yday[(k+1):length(p)]),bs="cs",sp =4,k=-1)
                  +s(as.numeric(rain[1:(length(p)-k)]),bs="cs",sp =4,k=-1)
                  +s(as.numeric(wind[1:(length(p)-k)]),bs="cs",sp =4,k=-1)
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
CO_allergy_dow<-gamair1(4,COm,data_4years,TEMPm,RHm,RAINm,WS_HRm)
SO2_allergy_dow<-gamair1(4,SO2m,data_4years,TEMPm,RHm,RAINm,WS_HRm)
O3_allergy_dow<-gamair1(4,O3m,data_4years,TEMPm,RHm,RAINm,WS_HRm)
PM2.5_allergy_dow<-gamair1(4,PM2.5m,data_4years,TEMPm,RHm,RAINm,WS_HRm)
PM10_allergy_dow<-gamair1(4,PM10m,data_4years,TEMPm,RHm,RAINm,WS_HRm)
NO_allergy_dow<-gamair1(4,NOm,data_4years,TEMPm,RHm,RAINm,WS_HRm)
NO2_allergy_dow<-gamair1(4,NO2m,data_4years,TEMPm,RHm,RAINm,WS_HRm)

xtable(CO_allergy_dow[[1]],digits =3)
xtable(SO2_allergy_dow[[1]],digits =3)
xtable(O3_allergy_dow[[1]],digits =3)
xtable(PM2.5_allergy_dow[[1]],digits =3)
xtable(PM10_allergy_dow[[1]],digits =3)
xtable(NO_allergy_dow[[1]],digits =3)
xtable(NO2_allergy_dow[[1]],digits =3)

CO_urticaria_dow<-gamair1(3,COm,data_4years,TEMPm,RHm,RAINm,WS_HRm)
SO2_urticaria_dow<-gamair1(3,SO2m,data_4years,TEMPm,RHm,RAINm,WS_HRm)
O3_urticaria_dow<-gamair1(3,O3m,data_4years,TEMPm,RHm,RAINm,WS_HRm)
PM2.5_urticaria_dow<-gamair1(3,PM2.5m,data_4years,TEMPm,RHm,RAINm,WS_HRm)
PM10_urticaria_dow<-gamair1(3,PM10m,data_4years,TEMPm,RHm,RAINm,WS_HRm)
NO_urticaria_dow<-gamair1(3,NOm,data_4years,TEMPm,RHm,RAINm,WS_HRm)
NO2_urticaria_dow<-gamair1(3,NO2m,data_4years,TEMPm,RHm,RAINm,WS_HRm)


xtable(CO_urticaria_dow[[1]],digits =3)
xtable(SO2_urticaria_dow[[1]],digits =3)
xtable(O3_urticaria_dow[[1]],digits =3)
xtable(PM2.5_urticaria_dow[[1]],digits =3)
xtable(PM10_urticaria_dow[[1]],digits =3)
xtable(NO_urticaria_dow[[1]],digits =3)
xtable(NO2_urticaria_dow[[1]],digits =3)


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
#urticaria
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

k=2
air1<-lag1_daily$SO2
data<-lag1_daily
temp<-lag1_daily$TEMP
rh<-lag1_daily$RH
rain<-lag1_daily$RAIN
wind<-lag1_daily$WS_HR
p<-lag1_daily$allergy
result<-gam(as.numeric(p)~air1
            +s(as.numeric(temp),bs="cs",sp = 3,k =4)
            +s(as.numeric(rh),bs="cs",sp = 4,k =4)
            +s(as.numeric(data$yday),bs="cs",sp =4,k=-1)
            +s(as.numeric(rain),bs="cs",sp =4,k=-1)
            +s(as.numeric(wind),bs="cs",sp =4,k=-1)
            ,
            data = data,
            family = "poisson")
summary(result)
plot(result)
daily_2017<-data_2017[which(data_2017$wday!=6&data_2017$wday!=7),]
plot(x=data_2017$yday,y=data_2017$allergy,xlab="date",ylab="patient",main = "patient~yday",type = "l")
plot(x=data_2017$yday,y=data_2017$O3,xlab="date",ylab="O3(PPB)",main = "O3~yday",type = "l")
plot(x=data_2017$O3,y=data_2017$allergy,xlab="O3(PPB)",ylab="patient",main = "patiet~O3",type = "p")

plot(result,xlab="ppb",ylab="",main = "smooth curve of CO")
plot(result,xlab="°C",ylab="",main = "smooth curve of temperature")
plot(result,xlab="Relative humidity",ylab="",main = "smooth curve of relative humidity")
plot(result,xlab="2017/1/1~2017/12/31 weekdays",ylab="",main = "smooth curve of time trends")
plot(result,xlab="mm",ylab="",main = "smooth curve of RAINFALL")
plot(result,xlab="m/sec",ylab="",main = "smooth curve of wind speed")

patientcol<-4
aircol<- c(6:12)
data<-lag0_daily
covcol<-c(13:16)

lag_gam<-function(patientcol,aircol,data,covcol){
  result<-NULL
  res<-NULL
  p<-names(data)[patientcol]
  air<-names(data)[aircol]
  z1<-data.frame()
  formula0<-list()
  for (j in 1:length(air)){
    formula0[[j]]<-paste(p,"~",air[[j]],'+s(as.numeric(TEMP),bs="cs",sp=3,k=4)+s(as.numeric(RH),bs="cs",sp=4,k=4)+s(as.numeric(yday),bs="cs",sp=4,k=4)+s(as.numeric(WS_HR),bs="cs",sp=4,k=4)+s(as.numeric(RAIN),bs="cs",sp=4,k=4)',sep="")
  }
  for(k in 1:length(formula0)){  
    res[[k]]<-gam(formula(formula0[[k]]),data = data,family = "poisson",method="REML",select=TRUE)
    result<-summary(res[[k]])
    z1[k,1]<-round(result$p.pv[2],5)
    z1[k,2]<-round(result$p.coeff[2],5)
    z1[k,3]<-round(result$s.pv[1],5)
    z1[k,4]<-round(result$s.pv[2],5)
    z1[k,5]<-round(result$s.pv[3],5)
    z1[k,6]<-round(result$s.pv[4],5)
    z1[k,7]<-round(result$s.pv[5],5)
  }
  
  colnames(z1)<-c("air_p.pv","air_coeff","TEMP","RH","day","WS_HR","RAIN")
  row.names(z1)<-names(data)[aircol]
  return(z1)
}

lag_gam<-function(patientcol,aircol,data,covcol){
  result<-NULL
  res<-NULL
  p<-names(data)[patientcol]
  air<-names(data)[aircol]
  z1<-data.frame()
  z2<-data.frame()
  formula0<-list()
  for (j in 1:length(air)){
    formula0[[j]]<-paste(p,"~",air[[j]],"+as.factor(wday)",'+s(as.numeric(TEMP),bs="cs",sp=3,k=4)+s(as.numeric(RH),bs="cs",sp=4,k=4)+s(as.numeric(yday),bs="cs",sp=4,k=4)+s(as.numeric(WS_HR),bs="cs",sp=4,k=4)+s(as.numeric(RAIN),bs="cs",sp=4,k=4)',sep="")
  }
  for(k in 1:length(formula0)){  
    res[[k]]<-gam(formula(formula0[[k]]),data = data,family = "poisson",method="REML",select=TRUE)
    result<-summary(res[[k]])
    z1[k,1]<-round(result$p.pv[2],5)
    z1[k,2]<-round(result$p.coeff[2],5)
    z1[k,3]<-round(result$s.pv[1],5)
    z1[k,4]<-round(result$s.pv[2],5)
    z1[k,5]<-round(result$s.pv[3],5)
    z1[k,6]<-round(result$s.pv[4],5)
    z1[k,7]<-round(result$s.pv[5],5)
    z2[k,1]<-round(result$p.pv[1],5)
    z2[k,2]<-round(result$p.coeff[1],5)
    z2[k,3]<-round(result$p.pv[3],5)
    z2[k,4]<-round(result$p.coeff[3],5)
    z2[k,5]<-round(result$p.pv[4],5)
    z2[k,6]<-round(result$p.coeff[4],5)
    z2[k,7]<-round(result$p.pv[5],5)
    z2[k,8]<-round(result$p.coeff[5],5)
    z2[k,9]<-round(result$p.pv[6],5)
    z2[k,10]<-round(result$p.coeff[6],5)
  }

  colnames(z1)<-c("air_p.pv","air_coeff","TEMP","RH","day","WS_HR","RAIN")
  row.names(z1)<-names(data)[aircol]
  colnames(z2)<-c("int_p.pv","int_coeff","2_p.pv","2_coeff","3_p.pv","3_coeff","4_p.pv","4_coeff","5_p.pv","5_coeff")
  row.names(z1)<-names(data)[aircol]
  return(list(z1,z2))
}

a<-list()
a[[1]]<-lag_gam(patientcol = 4,aircol = c(6:12),data = lag0_daily[which(lag0_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
a[[2]]<-lag_gam(patientcol = 4,aircol = c(6:12),data = lag1_daily[which(lag1_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
a[[3]]<-lag_gam(patientcol = 4,aircol = c(6:12),data = lag2_daily[which(lag2_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
a[[4]]<-lag_gam(patientcol = 4,aircol = c(6:12),data = lag3_daily[which(lag3_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
a[[5]]<-lag_gam(patientcol = 4,aircol = c(6:12),data = lag4_daily[which(lag4_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
a[[6]]<-lag_gam(patientcol = 4,aircol = c(6:12),data = lag5_daily[which(lag5_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
a[[7]]<-lag_gam(patientcol = 4,aircol = c(6:12),data = lag6_daily[which(lag6_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
a[[8]]<-lag_gam(patientcol = 4,aircol = c(6:12),data = lag7_daily[which(lag7_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))

a1<-data.frame(a)
res_allergy1<-list()
for(i in 1:7){
  res_allergy1[[i]]<-matrix(data = a1[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("air_p.pv","air_coeff","TEMP","RH","day","WS_HR","RAIN")))
  
}
res_allergy1[[1]]
res_allergy[[1]]
aa<-data.frame(a[[1]][1],a[[2]][1],a[[3]][1],a[[4]][1],
               a[[5]][1],a[[6]][1],a[[7]][1],a[[8]][1])
aaa<-data.frame(a[[1]][2],a[[2]][2],a[[3]][2],a[[4]][2],
               a[[5]][2],a[[6]][2],a[[7]][2],a[[8]][2])
res_allergy<-list()
for(i in 1:7){
  res_allergy[[i]]<-matrix(data = aa[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("air_p.pv","air_coeff","TEMP","RH","day","WS_HR","RAIN")))
  
}
res_allergy_week<-list()
for(i in 1:7){
  res_allergy_week[[i]]<-matrix(data = aaa[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("int_p.pv","int_coeff","2_p.pv","2_coeff","3_p.pv","3_coeff","4_p.pv","4_coeff","5_p.pv","5_coeff")))
  
}


rownames(aa)
names(res_allergy)<-row.names(aa)
names(res_allergy_week)<-row.names(aa)
res_allergy
res_allergy_week

b<-list()
b[[1]]<-lag_gam(patientcol = 3,aircol = c(6:12),data = lag0_daily[which(lag0_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
b[[2]]<-lag_gam(patientcol = 3,aircol = c(6:12),data = lag1_daily[which(lag1_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
b[[3]]<-lag_gam(patientcol = 3,aircol = c(6:12),data = lag2_daily[which(lag2_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
b[[4]]<-lag_gam(patientcol = 3,aircol = c(6:12),data = lag3_daily[which(lag3_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
b[[5]]<-lag_gam(patientcol = 3,aircol = c(6:12),data = lag4_daily[which(lag4_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
b[[6]]<-lag_gam(patientcol = 3,aircol = c(6:12),data = lag5_daily[which(lag5_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
b[[7]]<-lag_gam(patientcol = 3,aircol = c(6:12),data = lag6_daily[which(lag6_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
b[[8]]<-lag_gam(patientcol = 3,aircol = c(6:12),data = lag7_daily[which(lag7_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
b1<-data.frame(b)
res_urticaria1<-list()
for(i in 1:7){
  res_urticaria1[[i]]<-matrix(data = b1[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("air_p.pv","air_coeff","TEMP","RH","day","WS_HR","RAIN")))
  
}

bb<-data.frame(b[[1]][1],b[[2]][1],b[[3]][1],b[[4]][1],
               b[[5]][1],b[[6]][1],b[[7]][1],b[[8]][1])
bbb<-data.frame(b[[1]][2],b[[2]][2],b[[3]][2],b[[4]][2],
                b[[5]][2],b[[6]][2],b[[7]][2],b[[8]][2])
res_urticaria<-list()
for(i in 1:7){
  res_urticaria[[i]]<-matrix(data = bb[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("air_p.pv","air_coeff","TEMP","RH","day","WS_HR","RAIN")))
  
}
res_urticaria_week<-list()
for(i in 1:7){
  res_urticaria_week[[i]]<-matrix(data = bbb[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("int_p.pv","int_coeff","2_p.pv","2_coeff","3_p.pv","3_coeff","4_p.pv","4_coeff","5_p.pv","5_coeff")))
  
}
names(res_urticaria)<-rownames(bb)
names(res_urticaria_week)<-rownames(bb)
res_urticaria
res_urticaria_week
########patients by sex
#M
c<-list()
c[[1]]<-lag_gam(patientcol = 17,aircol = c(6:12),data = lag0_daily[which(lag0_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
c[[2]]<-lag_gam(patientcol = 17,aircol = c(6:12),data = lag1_daily[which(lag1_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
c[[3]]<-lag_gam(patientcol = 17,aircol = c(6:12),data = lag2_daily[which(lag2_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
c[[4]]<-lag_gam(patientcol = 17,aircol = c(6:12),data = lag3_daily[which(lag3_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
c[[5]]<-lag_gam(patientcol = 17,aircol = c(6:12),data = lag4_daily[which(lag4_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
c[[6]]<-lag_gam(patientcol = 17,aircol = c(6:12),data = lag5_daily[which(lag5_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
c[[7]]<-lag_gam(patientcol = 17,aircol = c(6:12),data = lag6_daily[which(lag6_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
c[[8]]<-lag_gam(patientcol = 17,aircol = c(6:12),data = lag7_daily[which(lag7_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))

cc<-data.frame(c[[1]][1],c[[2]][1],c[[3]][1],c[[4]][1],
               c[[5]][1],c[[6]][1],c[[7]][1],c[[8]][1])
ccc<-data.frame(c[[1]][2],c[[2]][2],c[[3]][2],c[[4]][2],
                c[[5]][2],c[[6]][2],c[[7]][2],c[[8]][2])
res_urticaria_M<-list()
for(i in 1:7){
  res_urticaria_M[[i]]<-matrix(data = cc[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("air_p.pv","air_coeff","TEMP","RH","day","WS_HR","RAIN")))
}
res_urticaria_M_week<-list()
for(i in 1:7){
  res_urticaria_M_week[[i]]<-matrix(data = ccc[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("int_p.pv","int_coeff","2_p.pv","2_coeff","3_p.pv","3_coeff","4_p.pv","4_coeff","5_p.pv","5_coeff")))
}

names(res_urticaria_M)<-row.names(cc)
names(res_urticaria_M_week)<-row.names(cc)
res_urticaria_M
res_urticaria_M_week

#F
d<-list()
d[[1]]<-lag_gam(patientcol = 18,aircol = c(6:12),data = lag0_daily[which(lag0_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
d[[2]]<-lag_gam(patientcol = 18,aircol = c(6:12),data = lag1_daily[which(lag1_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
d[[3]]<-lag_gam(patientcol = 18,aircol = c(6:12),data = lag2_daily[which(lag2_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
d[[4]]<-lag_gam(patientcol = 18,aircol = c(6:12),data = lag3_daily[which(lag3_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
d[[5]]<-lag_gam(patientcol = 18,aircol = c(6:12),data = lag4_daily[which(lag4_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
d[[6]]<-lag_gam(patientcol = 18,aircol = c(6:12),data = lag5_daily[which(lag5_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
d[[7]]<-lag_gam(patientcol = 18,aircol = c(6:12),data = lag6_daily[which(lag6_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
d[[8]]<-lag_gam(patientcol = 18,aircol = c(6:12),data = lag7_daily[which(lag7_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))

dd<-data.frame(d[[1]][1],d[[2]][1],d[[3]][1],d[[4]][1],
               d[[5]][1],d[[6]][1],d[[7]][1],d[[8]][1])
ddd<-data.frame(d[[1]][2],d[[2]][2],d[[3]][2],d[[4]][2],
                d[[5]][2],d[[6]][2],d[[7]][2],d[[8]][2])
res_urticaria_F<-list()
for(i in 1:7){
  res_urticaria_F[[i]]<-matrix(data = dd[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("air_p.pv","air_coeff","TEMP","RH","day","WS_HR","RAIN")))
}
res_urticaria_F_week<-list()
for(i in 1:7){
  res_urticaria_F_week[[i]]<-matrix(data = ddd[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("int_p.pv","int_coeff","2_p.pv","2_coeff","3_p.pv","3_coeff","4_p.pv","4_coeff","5_p.pv","5_coeff")))
}

names(res_urticaria_F)<-row.names(dd)
names(res_urticaria_F_week)<-row.names(dd)
res_urticaria_F
res_urticaria_F_week
#######################################
#allergy
#M
e<-list()
e[[1]]<-lag_gam(patientcol = 19,aircol = c(6:12),data = lag0_daily[which(lag0_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
e[[2]]<-lag_gam(patientcol = 19,aircol = c(6:12),data = lag1_daily[which(lag1_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
e[[3]]<-lag_gam(patientcol = 19,aircol = c(6:12),data = lag2_daily[which(lag2_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
e[[4]]<-lag_gam(patientcol = 19,aircol = c(6:12),data = lag3_daily[which(lag3_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
e[[5]]<-lag_gam(patientcol = 19,aircol = c(6:12),data = lag4_daily[which(lag4_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
e[[6]]<-lag_gam(patientcol = 19,aircol = c(6:12),data = lag5_daily[which(lag5_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
e[[7]]<-lag_gam(patientcol = 19,aircol = c(6:12),data = lag6_daily[which(lag6_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
e[[8]]<-lag_gam(patientcol = 19,aircol = c(6:12),data = lag7_daily[which(lag7_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))

ee<-data.frame(e[[1]][1],e[[2]][1],e[[3]][1],e[[4]][1],
               e[[5]][1],e[[6]][1],e[[7]][1],e[[8]][1])
eee<-data.frame(e[[1]][2],e[[2]][2],e[[3]][2],e[[4]][2],
                e[[5]][2],e[[6]][2],e[[7]][2],e[[8]][2])
res_allergy_M<-list()
for(i in 1:7){
  res_allergy_M[[i]]<-matrix(data = ee[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("air_p.pv","air_coeff","TEMP","RH","day","WS_HR","RAIN")))
}
res_allergy_M_week<-list()
for(i in 1:7){
  res_allergy_M_week[[i]]<-matrix(data = eee[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("int_p.pv","int_coeff","2_p.pv","2_coeff","3_p.pv","3_coeff","4_p.pv","4_coeff","5_p.pv","5_coeff")))
}

names(res_allergy_M)<-row.names(ee)
names(res_allergy_M_week)<-row.names(ee)
res_allergy_M
res_allergy_M_week
#F
f<-list()
f[[1]]<-lag_gam(patientcol = 20,aircol = c(6:12),data = lag0_daily[which(lag0_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
f[[2]]<-lag_gam(patientcol = 20,aircol = c(6:12),data = lag1_daily[which(lag1_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
f[[3]]<-lag_gam(patientcol = 20,aircol = c(6:12),data = lag2_daily[which(lag2_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
f[[4]]<-lag_gam(patientcol = 20,aircol = c(6:12),data = lag3_daily[which(lag3_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
f[[5]]<-lag_gam(patientcol = 20,aircol = c(6:12),data = lag4_daily[which(lag4_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
f[[6]]<-lag_gam(patientcol = 20,aircol = c(6:12),data = lag5_daily[which(lag5_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
f[[7]]<-lag_gam(patientcol = 20,aircol = c(6:12),data = lag6_daily[which(lag6_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
f[[8]]<-lag_gam(patientcol = 20,aircol = c(6:12),data = lag7_daily[which(lag7_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))

ff<-data.frame(f[[1]][1],f[[2]][1],f[[3]][1],f[[4]][1],
               f[[5]][1],f[[6]][1],f[[7]][1],f[[8]][1])
fff<-data.frame(f[[1]][2],f[[2]][2],f[[3]][2],f[[4]][2],
                f[[5]][2],f[[6]][2],f[[7]][2],f[[8]][2])
res_allergy_F<-list()
for(i in 1:7){
  res_allergy_F[[i]]<-matrix(data = ff[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("air_p.pv","air_coeff","TEMP","RH","day","WS_HR","RAIN")))
}
res_allergy_F_week<-list()
for(i in 1:7){
  res_allergy_F_week[[i]]<-matrix(data = fff[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("int_p.pv","int_coeff","2_p.pv","2_coeff","3_p.pv","3_coeff","4_p.pv","4_coeff","5_p.pv","5_coeff")))
}

names(res_allergy_F)<-row.names(ff)
names(res_allergy_F_week)<-row.names(ff)
res_allergy_F
res_allergy_F_week

#G urticaria_20
g<-list()
g[[1]]<-lag_gam(patientcol = 21,aircol = c(6:12),data = lag0_daily[which(lag0_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
g[[2]]<-lag_gam(patientcol = 21,aircol = c(6:12),data = lag1_daily[which(lag1_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
g[[3]]<-lag_gam(patientcol = 21,aircol = c(6:12),data = lag2_daily[which(lag2_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
g[[4]]<-lag_gam(patientcol = 21,aircol = c(6:12),data = lag3_daily[which(lag3_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
g[[5]]<-lag_gam(patientcol = 21,aircol = c(6:12),data = lag4_daily[which(lag4_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
g[[6]]<-lag_gam(patientcol = 21,aircol = c(6:12),data = lag5_daily[which(lag5_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
g[[7]]<-lag_gam(patientcol = 21,aircol = c(6:12),data = lag6_daily[which(lag6_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
g[[8]]<-lag_gam(patientcol = 21,aircol = c(6:12),data = lag7_daily[which(lag7_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))

gg<-data.frame(g[[1]][1],g[[2]][1],g[[3]][1],g[[4]][1],
               g[[5]][1],g[[6]][1],g[[7]][1],g[[8]][1])
ggg<-data.frame(g[[1]][2],g[[2]][2],g[[3]][2],g[[4]][2],
                g[[5]][2],g[[6]][2],g[[7]][2],g[[8]][2])
res_urticaria_20<-list()
for(i in 1:7){
  res_urticaria_20[[i]]<-matrix(data = gg[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("air_p.pv","air_coeff","TEMP","RH","day","WS_HR","RAIN")))
}
res_urticaria_20_week<-list()
for(i in 1:7){
  res_urticaria_20_week[[i]]<-matrix(data = ggg[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("int_p.pv","int_coeff","2_p.pv","2_coeff","3_p.pv","3_coeff","4_p.pv","4_coeff","5_p.pv","5_coeff")))
}

names(res_urticaria_20)<-row.names(gg)
names(res_urticaria_20_week)<-row.names(gg)
res_urticaria_20
res_urticaria_20_week

#h urticaria_65
h<-list()
h[[1]]<-lag_gam(patientcol = 22,aircol = c(6:12),data = lag0_daily[which(lag0_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
h[[2]]<-lag_gam(patientcol = 22,aircol = c(6:12),data = lag1_daily[which(lag1_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
h[[3]]<-lag_gam(patientcol = 22,aircol = c(6:12),data = lag2_daily[which(lag2_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
h[[4]]<-lag_gam(patientcol = 22,aircol = c(6:12),data = lag3_daily[which(lag3_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
h[[5]]<-lag_gam(patientcol = 22,aircol = c(6:12),data = lag4_daily[which(lag4_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
h[[6]]<-lag_gam(patientcol = 22,aircol = c(6:12),data = lag5_daily[which(lag5_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
h[[7]]<-lag_gam(patientcol = 22,aircol = c(6:12),data = lag6_daily[which(lag6_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
h[[8]]<-lag_gam(patientcol = 22,aircol = c(6:12),data = lag7_daily[which(lag7_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))

hh<-data.frame(h[[1]][1],h[[2]][1],h[[3]][1],h[[4]][1],
               h[[5]][1],h[[6]][1],h[[7]][1],h[[8]][1])
hhh<-data.frame(h[[1]][2],h[[2]][2],h[[3]][2],h[[4]][2],
                h[[5]][2],h[[6]][2],h[[7]][2],h[[8]][2])
res_urticaria_65<-list()
for(i in 1:7){
  res_urticaria_65[[i]]<-matrix(data = hh[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("air_p.pv","air_coeff","TEMP","RH","day","WS_HR","RAIN")))
}
res_urticaria_65_week<-list()
for(i in 1:7){
  res_urticaria_65_week[[i]]<-matrix(data = hhh[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("int_p.pv","int_coeff","2_p.pv","2_coeff","3_p.pv","3_coeff","4_p.pv","4_coeff","5_p.pv","5_coeff")))
}

names(res_urticaria_65)<-row.names(hh)
names(res_urticaria_65_week)<-row.names(hh)
res_urticaria_65
res_urticaria_65_week

#i allergy_20
i<-list()
i[[1]]<-lag_gam(patientcol = 23,aircol = c(6:12),data = lag0_daily[which(lag0_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
i[[2]]<-lag_gam(patientcol = 23,aircol = c(6:12),data = lag1_daily[which(lag1_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
i[[3]]<-lag_gam(patientcol = 23,aircol = c(6:12),data = lag2_daily[which(lag2_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
i[[4]]<-lag_gam(patientcol = 23,aircol = c(6:12),data = lag3_daily[which(lag3_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
i[[5]]<-lag_gam(patientcol = 23,aircol = c(6:12),data = lag4_daily[which(lag4_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
i[[6]]<-lag_gam(patientcol = 23,aircol = c(6:12),data = lag5_daily[which(lag5_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
i[[7]]<-lag_gam(patientcol = 23,aircol = c(6:12),data = lag6_daily[which(lag6_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
i[[8]]<-lag_gam(patientcol = 23,aircol = c(6:12),data = lag7_daily[which(lag7_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))

ii<-data.frame(i[[1]][1],i[[2]][1],i[[3]][1],i[[4]][1],
               i[[5]][1],i[[6]][1],i[[7]][1],i[[8]][1])
iii<-data.frame(i[[1]][2],i[[2]][2],i[[3]][2],i[[4]][2],
                i[[5]][2],i[[6]][2],i[[7]][2],i[[8]][2])
res_allergy_20<-list()
for(i in 1:7){
  res_allergy_20[[i]]<-matrix(data = ii[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("air_p.pv","air_coeff","TEMP","RH","day","WS_HR","RAIN")))
}
res_allergy_20_week<-list()
for(i in 1:7){
  res_allergy_20_week[[i]]<-matrix(data = iii[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("int_p.pv","int_coeff","2_p.pv","2_coeff","3_p.pv","3_coeff","4_p.pv","4_coeff","5_p.pv","5_coeff")))
}

names(res_allergy_20)<-row.names(ii)
names(res_allergy_20_week)<-row.names(ii)
res_allergy_20
res_allergy_20_week

#j allergy_65
j<-list()
j[[1]]<-lag_gam(patientcol = 24,aircol = c(6:12),data = lag0_daily[which(lag0_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
j[[2]]<-lag_gam(patientcol = 24,aircol = c(6:12),data = lag1_daily[which(lag1_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
j[[3]]<-lag_gam(patientcol = 24,aircol = c(6:12),data = lag2_daily[which(lag2_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
j[[4]]<-lag_gam(patientcol = 24,aircol = c(6:12),data = lag3_daily[which(lag3_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
j[[5]]<-lag_gam(patientcol = 24,aircol = c(6:12),data = lag4_daily[which(lag4_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
j[[6]]<-lag_gam(patientcol = 24,aircol = c(6:12),data = lag5_daily[which(lag5_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
j[[7]]<-lag_gam(patientcol = 24,aircol = c(6:12),data = lag6_daily[which(lag6_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
j[[8]]<-lag_gam(patientcol = 24,aircol = c(6:12),data = lag7_daily[which(lag7_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))

jj<-data.frame(j[[1]][1],j[[2]][1],j[[3]][1],j[[4]][1],
               j[[5]][1],j[[6]][1],j[[7]][1],j[[8]][1])
jjj<-data.frame(j[[1]][2],j[[2]][2],j[[3]][2],j[[4]][2],
                j[[5]][2],j[[6]][2],j[[7]][2],j[[8]][2])
res_allergy_65<-list()
for(i in 1:7){
  res_allergy_65[[i]]<-matrix(data = jj[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("air_p.pv","air_coeff","TEMP","RH","day","WS_HR","RAIN")))
}
res_allergy_65_week<-list()
for(i in 1:7){
  res_allergy_65_week[[i]]<-matrix(data = jjj[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("int_p.pv","int_coeff","2_p.pv","2_coeff","3_p.pv","3_coeff","4_p.pv","4_coeff","5_p.pv","5_coeff")))
}

names(res_allergy_65)<-row.names(jj)
names(res_allergy_65_week)<-row.names(jj)
res_allergy_65
res_allergy_65_week
####################
p<-names(lag1_daily)[4]

formula1<-paste(p,"~",names(lag2_daily)[7],"+as.factor(wday)",'+s(as.numeric(TEMP),bs="cs",sp=3,k=4)+s(as.numeric(RH),bs="cs",sp=4,k=4)+s(as.numeric(yday),bs="cs",sp=4,k=4)+s(as.numeric(WS_HR),bs="cs",sp=4,k=4)+s(as.numeric(RAIN),bs="cs",sp=4,k=4)',sep="")
res<-gam(formula(formula1),data = lag0_daily,family  = "poisson")

sres<-summary(res)
sres$p.pv
plot(res)
gam.check(res)

data.frame(res_allergy[[2]])
formulaCO<-paste(p,"~",air[[2]],"as.factor(wday)",'+s(as.numeric(TEMP),bs="cs",sp=3,k=-1)+s(as.numeric(RH),bs="cs",sp=4,k=-1)+s(as.numeric(WS_HR),bs="cs",sp=4,k=-1)+s(as.numeric(RAIN),bs="cs",sp=4,k=-1)',sep="")





data0<-lag0_daily
data0<-lag0_daily[which(data0$date<as.Date("2018-01-01")),]
summary(data0[which(data0$wday==1),3])
summary(data0[which(data0$wday==2),3])
summary(data0[which(data0$wday==3),3])
summary(data0[which(data0$wday==4),3])
summary(data0[which(data0$wday==5),3])
summary(data0[which(data0$wday==1),4])
summary(data0[which(data0$wday==2),4])
summary(data0[which(data0$wday==3),4])
summary(data0[which(data0$wday==4),4])
summary(data0[which(data0$wday==5),4])


#data0<-data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),]
dim(data0)
p<-names(data0[3])
air<-names(data0[7])
formula0<-paste(p,"~",air,'+s(as.numeric(TEMP),bs="cs",sp=3,k=-1)+s(as.numeric(RH),bs="cs",sp=4,k=-1)+s(as.numeric(wday),bs="cs",sp=4,k=4)+s(as.numeric(WS_HR),bs="cs",sp=4,k=-1)',sep="")
formula0<-paste(p,"~","s(as.numeric(",air,'),bs="cs",sp=4,k=-1)',sep="")


res<-gam(formula(formula0),data = data0,family = "poisson")
summary(res)
gam.check(res)
for(i in 1:4){
plot(res, select = i, shade = TRUE, scale =0 , seWithMean = TRUE)
}
#urticaria
par(mfrow = c(1,1))
plot(data0$date,data0$urticaria,type="l",ylab="patient",xlab="date",col="black")
par(new = TRUE)
plot(y=fitted(res), type = "l", xaxt = 'n', yaxt='n', ylab = '', xlab = '',
     x=data0$date,
     lty = 1, col = "red")
plot(data0$urticaria-fitted(res),type = "l")
#allergy
par(mfrow = c(1,1))
plot(data0$date,data0$allergy,type="l",ylab="patient",xlab="date",col="black")
par(new = TRUE)
plot(y=fitted(res), type = "l", xaxt = 'n', yaxt='n', ylab = '', xlab = '',
     x=data0$date,
     lty = 1, col = "red")
plot(data0$allergy-fitted(res),type = "l")
predict(res,type = "terms")
plot(y=predict(res,type = "terms")[,1],x=1:length(predict(res,type = "terms")[,1]), type = "l", xaxt = 'n', yaxt='n', ylab = '', xlab = '',lty = 1, col = "red")
write.csv(lag0_daily,"lag0_daily.csv",fileEncoding = "utf-8")
