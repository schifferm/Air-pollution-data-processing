data_final<-list()
data_final[[1]]<-lag0_daily
data_final[[2]]<-lag1_daily
data_final[[3]]<-lag2_daily
data_final[[4]]<-lag3_daily
data_final[[5]]<-lag4_daily
data_final[[6]]<-lag5_daily
data_final[[7]]<-lag6_daily
data_final[[8]]<-lag7_daily



#urt_20_35
k<-list()
for(i in 1:8){
  k[[i]]<-lag_gam(patientcol = 21,aircol = c(6:12),data = data_final[[i]],covcol = c(13:16))
}
kk<-data.frame(k[[1]][1],k[[2]][1],k[[3]][1],k[[4]][1],
               k[[5]][1],k[[6]][1],k[[7]][1],k[[8]][1])
kkk<-data.frame(k[[1]][2],k[[2]][2],k[[3]][2],k[[4]][2],
                k[[5]][2],k[[6]][2],k[[7]][2],k[[8]][2])
res_urt_20_35<-list()
for(i in 1:7){
  res_urt_20_35[[i]]<-matrix(data = kk[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("air_p.pv","air_coeff","TEMP","RH","day","WS_HR","RAIN")))
}
res_urt_20_35_week<-list()
for(i in 1:7){
  res_urt_20_35_week[[i]]<-matrix(data = kkk[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("int_p.pv","int_coeff","2_p.pv","2_coeff","3_p.pv","3_coeff","4_p.pv","4_coeff","5_p.pv","5_coeff")))
}

kkkk<-data.frame(k[[1]][3],k[[2]][3],k[[3]][3],k[[4]][3],
                 k[[5]][3],k[[6]][3],k[[7]][3],k[[8]][3])
res_urt_20_35_rr<-list()
for(i in 1:7){
  res_urt_20_35_rr[[i]]<-matrix(data = kkkk[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep=""))))
  
}
names(res_urt_20_35_rr)<-row.names(kk)
res_urt_20_35_rr

names(res_urt_20_35)<-row.names(kk)
names(res_urt_20_35_week)<-row.names(kk)
res_urt_20_35
res_urt_20_35_week

#urt_35_50
k<-list()
for(i in 1:8){
  k[[i]]<-lag_gam(patientcol = 22,aircol = c(6:12),data = data_final[[i]],covcol = c(13:16))
}
kk<-data.frame(k[[1]][1],k[[2]][1],k[[3]][1],k[[4]][1],
               k[[5]][1],k[[6]][1],k[[7]][1],k[[8]][1])
kkk<-data.frame(k[[1]][2],k[[2]][2],k[[3]][2],k[[4]][2],
                k[[5]][2],k[[6]][2],k[[7]][2],k[[8]][2])
res_urt_35_50<-list()
for(i in 1:7){
  res_urt_35_50[[i]]<-matrix(data = kk[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("air_p.pv","air_coeff","TEMP","RH","day","WS_HR","RAIN")))
}
res_urt_35_50_week<-list()
for(i in 1:7){
  res_urt_35_50_week[[i]]<-matrix(data = kkk[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("int_p.pv","int_coeff","2_p.pv","2_coeff","3_p.pv","3_coeff","4_p.pv","4_coeff","5_p.pv","5_coeff")))
}

kkkk<-data.frame(k[[1]][3],k[[2]][3],k[[3]][3],k[[4]][3],
                 k[[5]][3],k[[6]][3],k[[7]][3],k[[8]][3])
res_urt_35_50_rr<-list()
for(i in 1:7){
  res_urt_35_50_rr[[i]]<-matrix(data = kkkk[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep=""))))
  
}
names(res_urt_35_50_rr)<-row.names(kk)
res_urt_35_50_rr

names(res_urt_35_50)<-row.names(kk)
names(res_urt_35_50_week)<-row.names(kk)
res_urt_35_50
res_urt_35_50_week

#urt_50_65
k<-list()
for(i in 1:8){
  k[[i]]<-lag_gam(patientcol = 23,aircol = c(6:12),data = data_final[[i]],covcol = c(13:16))
}
kk<-data.frame(k[[1]][1],k[[2]][1],k[[3]][1],k[[4]][1],
               k[[5]][1],k[[6]][1],k[[7]][1],k[[8]][1])
kkk<-data.frame(k[[1]][2],k[[2]][2],k[[3]][2],k[[4]][2],
                k[[5]][2],k[[6]][2],k[[7]][2],k[[8]][2])
res_urt_50_65<-list()
for(i in 1:7){
  res_urt_50_65[[i]]<-matrix(data = kk[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("air_p.pv","air_coeff","TEMP","RH","day","WS_HR","RAIN")))
}
res_urt_50_65_week<-list()
for(i in 1:7){
  res_urt_50_65_week[[i]]<-matrix(data = kkk[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("int_p.pv","int_coeff","2_p.pv","2_coeff","3_p.pv","3_coeff","4_p.pv","4_coeff","5_p.pv","5_coeff")))
}

kkkk<-data.frame(k[[1]][3],k[[2]][3],k[[3]][3],k[[4]][3],
                 k[[5]][3],k[[6]][3],k[[7]][3],k[[8]][3])
res_urt_50_65_rr<-list()
for(i in 1:7){
  res_urt_50_65_rr[[i]]<-matrix(data = kkkk[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep=""))))
  
}
names(res_urt_50_65_rr)<-row.names(kk)
res_urt_50_65_rr

names(res_urt_50_65)<-row.names(kk)
names(res_urt_50_65_week)<-row.names(kk)
res_urt_50_65
res_urt_50_65_week

#urt_65
k<-list()
for(i in 1:8){
  k[[i]]<-lag_gam(patientcol = 24,aircol = c(6:12),data = data_final[[i]],covcol = c(13:16))
}
kk<-data.frame(k[[1]][1],k[[2]][1],k[[3]][1],k[[4]][1],
               k[[5]][1],k[[6]][1],k[[7]][1],k[[8]][1])
kkk<-data.frame(k[[1]][2],k[[2]][2],k[[3]][2],k[[4]][2],
                k[[5]][2],k[[6]][2],k[[7]][2],k[[8]][2])
res_urt_65<-list()
for(i in 1:7){
  res_urt_65[[i]]<-matrix(data = kk[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("air_p.pv","air_coeff","TEMP","RH","day","WS_HR","RAIN")))
}
res_urt_65_week<-list()
for(i in 1:7){
  res_urt_65_week[[i]]<-matrix(data = kkk[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("int_p.pv","int_coeff","2_p.pv","2_coeff","3_p.pv","3_coeff","4_p.pv","4_coeff","5_p.pv","5_coeff")))
}

kkkk<-data.frame(k[[1]][3],k[[2]][3],k[[3]][3],k[[4]][3],
                 k[[5]][3],k[[6]][3],k[[7]][3],k[[8]][3])
res_urt_65_rr<-list()
for(i in 1:7){
  res_urt_65_rr[[i]]<-matrix(data = kkkk[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep=""))))
  
}
names(res_urt_65_rr)<-row.names(kk)
res_urt_65_rr

names(res_urt_65)<-row.names(kk)
names(res_urt_65_week)<-row.names(kk)
res_urt_65
res_urt_65_week

#all_20_35
k<-list()
for(i in 1:8){
  k[[i]]<-lag_gam(patientcol = 25,aircol = c(6:12),data = data_final[[i]],covcol = c(13:16))
}
kk<-data.frame(k[[1]][1],k[[2]][1],k[[3]][1],k[[4]][1],
               k[[5]][1],k[[6]][1],k[[7]][1],k[[8]][1])
kkk<-data.frame(k[[1]][2],k[[2]][2],k[[3]][2],k[[4]][2],
                k[[5]][2],k[[6]][2],k[[7]][2],k[[8]][2])
res_all_20_35<-list()
for(i in 1:7){
  res_all_20_35[[i]]<-matrix(data = kk[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("air_p.pv","air_coeff","TEMP","RH","day","WS_HR","RAIN")))
}
res_all_20_35_week<-list()
for(i in 1:7){
  res_all_20_35_week[[i]]<-matrix(data = kkk[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("int_p.pv","int_coeff","2_p.pv","2_coeff","3_p.pv","3_coeff","4_p.pv","4_coeff","5_p.pv","5_coeff")))
}

kkkk<-data.frame(k[[1]][3],k[[2]][3],k[[3]][3],k[[4]][3],
                 k[[5]][3],k[[6]][3],k[[7]][3],k[[8]][3])
res_all_20_35_rr<-list()
for(i in 1:7){
  res_all_20_35_rr[[i]]<-matrix(data = kkkk[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep=""))))
  
}
names(res_all_20_35_rr)<-row.names(kk)
res_all_20_35_rr

names(res_all_20_35)<-row.names(kk)
names(res_all_20_35_week)<-row.names(kk)
res_all_20_35
res_all_20_35_week

#all_35_50
k<-list()
for(i in 1:8){
  k[[i]]<-lag_gam(patientcol = 26,aircol = c(6:12),data = data_final[[i]],covcol = c(13:16))
}
kk<-data.frame(k[[1]][1],k[[2]][1],k[[3]][1],k[[4]][1],
               k[[5]][1],k[[6]][1],k[[7]][1],k[[8]][1])
kkk<-data.frame(k[[1]][2],k[[2]][2],k[[3]][2],k[[4]][2],
                k[[5]][2],k[[6]][2],k[[7]][2],k[[8]][2])
res_all_35_50<-list()
for(i in 1:7){
  res_all_35_50[[i]]<-matrix(data = kk[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("air_p.pv","air_coeff","TEMP","RH","day","WS_HR","RAIN")))
}
res_all_35_50_week<-list()
for(i in 1:7){
  res_all_35_50_week[[i]]<-matrix(data = kkk[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("int_p.pv","int_coeff","2_p.pv","2_coeff","3_p.pv","3_coeff","4_p.pv","4_coeff","5_p.pv","5_coeff")))
}

kkkk<-data.frame(k[[1]][3],k[[2]][3],k[[3]][3],k[[4]][3],
                 k[[5]][3],k[[6]][3],k[[7]][3],k[[8]][3])
res_all_35_50_rr<-list()
for(i in 1:7){
  res_all_35_50_rr[[i]]<-matrix(data = kkkk[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep=""))))
  
}
names(res_all_35_50_rr)<-row.names(kk)
res_all_35_50_rr

names(res_all_35_50)<-row.names(kk)
names(res_all_35_50_week)<-row.names(kk)
res_all_35_50
res_all_35_50_week

#all_50_65
k<-list()
for(i in 1:8){
  k[[i]]<-lag_gam(patientcol = 27,aircol = c(6:12),data = data_final[[i]],covcol = c(13:16))
}
kk<-data.frame(k[[1]][1],k[[2]][1],k[[3]][1],k[[4]][1],
               k[[5]][1],k[[6]][1],k[[7]][1],k[[8]][1])
kkk<-data.frame(k[[1]][2],k[[2]][2],k[[3]][2],k[[4]][2],
                k[[5]][2],k[[6]][2],k[[7]][2],k[[8]][2])
res_all_50_65<-list()
for(i in 1:7){
  res_all_50_65[[i]]<-matrix(data = kk[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("air_p.pv","air_coeff","TEMP","RH","day","WS_HR","RAIN")))
}
res_all_50_65_week<-list()
for(i in 1:7){
  res_all_50_65_week[[i]]<-matrix(data = kkk[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("int_p.pv","int_coeff","2_p.pv","2_coeff","3_p.pv","3_coeff","4_p.pv","4_coeff","5_p.pv","5_coeff")))
}

kkkk<-data.frame(k[[1]][3],k[[2]][3],k[[3]][3],k[[4]][3],
                 k[[5]][3],k[[6]][3],k[[7]][3],k[[8]][3])
res_all_50_65_rr<-list()
for(i in 1:7){
  res_all_50_65_rr[[i]]<-matrix(data = kkkk[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep=""))))
  
}
names(res_all_50_65_rr)<-row.names(kk)
res_all_50_65_rr

names(res_all_50_65)<-row.names(kk)
names(res_all_50_65_week)<-row.names(kk)
res_all_50_65
res_all_50_65_week

#all_65
k<-list()
for(i in 1:8){
  k[[i]]<-lag_gam(patientcol = 28,aircol = c(6:12),data = data_final[[i]],covcol = c(13:16))
}
kk<-data.frame(k[[1]][1],k[[2]][1],k[[3]][1],k[[4]][1],
               k[[5]][1],k[[6]][1],k[[7]][1],k[[8]][1])
kkk<-data.frame(k[[1]][2],k[[2]][2],k[[3]][2],k[[4]][2],
                k[[5]][2],k[[6]][2],k[[7]][2],k[[8]][2])
res_all_65<-list()
for(i in 1:7){
  res_all_65[[i]]<-matrix(data = kk[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("air_p.pv","air_coeff","TEMP","RH","day","WS_HR","RAIN")))
}
res_all_65_week<-list()
for(i in 1:7){
  res_all_65_week[[i]]<-matrix(data = kkk[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("int_p.pv","int_coeff","2_p.pv","2_coeff","3_p.pv","3_coeff","4_p.pv","4_coeff","5_p.pv","5_coeff")))
}

kkkk<-data.frame(k[[1]][3],k[[2]][3],k[[3]][3],k[[4]][3],
                 k[[5]][3],k[[6]][3],k[[7]][3],k[[8]][3])
res_all_65_rr<-list()
for(i in 1:7){
  res_all_65_rr[[i]]<-matrix(data = kkkk[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep=""))))
  
}
names(res_all_65_rr)<-row.names(kk)
res_all_65_rr

names(res_all_65)<-row.names(kk)
names(res_all_65_week)<-row.names(kk)
res_all_65
res_all_65_week
#################################################################
#MUTI ANALYSIS
#urt_20_35
min_urt_20_35<-data.frame()
for(i in 1:7){
  min_urt_20_35[i,1]<-names(which.min(res_urt_20_35[[i]][,1]))
  min_urt_20_35[i,2]<-res_urt_20_35[[i]][which.min(res_urt_20_35[[i]][,1]),1]
  min_urt_20_35[i,3]<-res_urt_20_35[[i]][which.min(res_urt_20_35[[i]][,1]),2]
}
rownames(min_urt_20_35)<-rownames(kk)
colnames(min_urt_20_35)<-c("lag","p.pv","coeff")
min_urt_20_35
mutidata<-data.frame(PM10=lag7_daily$PM10,
                      lag7_daily[,-6:-12])
mutiair<-NULL
for(i in c(5)){
  mutiair<-paste(rownames(min_urt_20_35)[i],"+",mutiair,sep="")
}
p<-names(lag1_daily)[21]

formula_urt_20_35<-paste(p,"~",mutiair,"+as.factor(wday)",'+s(as.numeric(TEMP),bs="cs",sp=3,k=4)+s(as.numeric(RH),bs="cs",sp=4,k=4)+s(as.numeric(yday),bs="cs",sp=4,k=4)+s(as.numeric(WS_HR),bs="cs",sp=4,k=4)+s(as.numeric(RAIN),bs="cs",sp=4,k=4)',sep="")
mutires_urt_20_35<-gam(formula(formula_urt_20_35),data =mutidata,family  = ziP() )
summary(mutires_urt_20_35)

#urt_35_50
min_urt_35_50<-data.frame()
for(i in 1:7){
  min_urt_35_50[i,1]<-names(which.min(res_urt_35_50[[i]][,1]))
  min_urt_35_50[i,2]<-res_urt_35_50[[i]][which.min(res_urt_35_50[[i]][,1]),1]
  min_urt_35_50[i,3]<-res_urt_35_50[[i]][which.min(res_urt_35_50[[i]][,1]),2]
}
rownames(min_urt_35_50)<-rownames(kk)
colnames(min_urt_35_50)<-c("lag","p.pv","coeff")
min_urt_35_50
mutidata<-data.frame(SO2=lag1_daily$SO2,
                     PM10=lag0_daily$PM10[-1],
                     NO2=lag0_daily$NO2[-1],
                     lag1_daily[,-6:-12])
mutiair<-NULL
for(i in c(1,5,7)){
  mutiair<-paste(rownames(min_urt_35_50)[i],"+",mutiair,sep="")
}
p<-names(lag1_daily)[22]

formula_urt_35_50<-paste(p,"~",mutiair,"+as.factor(wday)",'+s(as.numeric(TEMP),bs="cs",sp=3,k=4)+s(as.numeric(RH),bs="cs",sp=4,k=4)+s(as.numeric(yday),bs="cs",sp=4,k=4)+s(as.numeric(WS_HR),bs="cs",sp=4,k=4)+s(as.numeric(RAIN),bs="cs",sp=4,k=4)',sep="")
mutires_urt_35_50<-gam(formula(formula_urt_35_50),data =mutidata,family  = ziP() )
summary(mutires_urt_35_50)

#urt_50_65
min_urt_50_65<-data.frame()
for(i in 1:7){
  min_urt_50_65[i,1]<-names(which.min(res_urt_50_65[[i]][,1]))
  min_urt_50_65[i,2]<-res_urt_50_65[[i]][which.min(res_urt_50_65[[i]][,1]),1]
  min_urt_50_65[i,3]<-res_urt_50_65[[i]][which.min(res_urt_50_65[[i]][,1]),2]
}
rownames(min_urt_50_65)<-rownames(kk)
colnames(min_urt_50_65)<-c("lag","p.pv","coeff")
min_urt_50_65
mutidata<-data.frame(PM10=lag1_daily$PM10[-1:-2],
                     NO=lag5_daily$NO,
                     NO2=lag0_daily$NO2[-1:-3],
                     lag5_daily[,-6:-12])
mutiair<-NULL
for(i in c(5,6,7)){
  mutiair<-paste(rownames(min_urt_50_65)[i],"+",mutiair,sep="")
}
p<-names(lag1_daily)[23]

formula_urt_50_65<-paste(p,"~",mutiair,"+as.factor(wday)",'+s(as.numeric(TEMP),bs="cs",sp=3,k=4)+s(as.numeric(RH),bs="cs",sp=4,k=4)+s(as.numeric(yday),bs="cs",sp=4,k=4)+s(as.numeric(WS_HR),bs="cs",sp=4,k=4)+s(as.numeric(RAIN),bs="cs",sp=4,k=4)',sep="")
mutires_urt_50_65<-gam(formula(formula_urt_50_65),data =mutidata,family  = ziP() )
summary(mutires_urt_50_65)

#urt_65
min_urt_65<-data.frame()
for(i in 1:7){
  min_urt_65[i,1]<-names(which.min(res_urt_65[[i]][,1]))
  min_urt_65[i,2]<-res_urt_65[[i]][which.min(res_urt_65[[i]][,1]),1]
  min_urt_65[i,3]<-res_urt_65[[i]][which.min(res_urt_65[[i]][,1]),2]
}
rownames(min_urt_65)<-rownames(kk)
colnames(min_urt_65)<-c("lag","p.pv","coeff")
min_urt_65
mutidata<-data.frame(SO2=lag1_daily$SO2[-1],
                     CO=lag0_daily$CO[-1:-2],
                     NO=lag2_daily$NO,
                     NO2=lag0_daily$NO2[-1:-2],
                     lag2_daily[,-6:-12])
mutiair<-NULL
for(i in c(1,2,6,7)){
  mutiair<-paste(rownames(min_urt_65)[i],"+",mutiair,sep="")
}
p<-names(lag1_daily)[24]

formula_urt_65<-paste(p,"~",mutiair,"+as.factor(wday)",'+s(as.numeric(TEMP),bs="cs",sp=3,k=4)+s(as.numeric(RH),bs="cs",sp=4,k=4)+s(as.numeric(yday),bs="cs",sp=4,k=4)+s(as.numeric(WS_HR),bs="cs",sp=4,k=4)+s(as.numeric(RAIN),bs="cs",sp=4,k=4)',sep="")
mutires_urt_65<-gam(formula(formula_urt_65),data =mutidata,family  = ziP() )
summary(mutires_urt_65)

#all_20_35
min_all_20_35<-data.frame()
for(i in 1:7){
  min_all_20_35[i,1]<-names(which.min(res_all_20_35[[i]][,1]))
  min_all_20_35[i,2]<-res_all_20_35[[i]][which.min(res_all_20_35[[i]][,1]),1]
  min_all_20_35[i,3]<-res_all_20_35[[i]][which.min(res_all_20_35[[i]][,1]),2]
}
rownames(min_all_20_35)<-rownames(kk)
colnames(min_all_20_35)<-c("lag","p.pv","coeff")
min_all_20_35
mutidata<-data.frame(PM2.5=lag3_daily$PM2.5,
                     NO=lag4_daily$NO,
                     lag4_daily[,-6:-12])
mutiair<-NULL
for(i in c(4,6)){
  mutiair<-paste(rownames(min_all_20_35)[i],"+",mutiair,sep="")
}
p<-names(lag1_daily)[25]

formula_all_20_35<-paste(p,"~",mutiair,"+as.factor(wday)",'+s(as.numeric(TEMP),bs="cs",sp=3,k=4)+s(as.numeric(RH),bs="cs",sp=4,k=4)+s(as.numeric(yday),bs="cs",sp=4,k=4)+s(as.numeric(WS_HR),bs="cs",sp=4,k=4)+s(as.numeric(RAIN),bs="cs",sp=4,k=4)',sep="")
mutires_all_20_35<-gam(formula(formula_all_20_35),data =mutidata,family  = ziP() )
summary(mutires_all_20_35)

#all_35_50
min_all_35_50<-data.frame()
for(i in 1:7){
  min_all_35_50[i,1]<-names(which.min(res_all_35_50[[i]][,1]))
  min_all_35_50[i,2]<-res_all_35_50[[i]][which.min(res_all_35_50[[i]][,1]),1]
  min_all_35_50[i,3]<-res_all_35_50[[i]][which.min(res_all_35_50[[i]][,1]),2]
}
rownames(min_all_35_50)<-rownames(kk)
colnames(min_all_35_50)<-c("lag","p.pv","coeff")
min_all_35_50
mutidata<-data.frame(SO2=lag6_daily$SO2,
                     CO=lag2_daily$CO[-1:-2],
                     O3=lag0_daily$O3[-1:-4],
                     PM2.5=lag3_daily$PM2.5[-1],
                     PM10=lag2_daily$PM10[-1:-2],
                     NO=lag0_daily$NO[-1:-4],
                     NO2=lag2_daily$NO2[-1:-2],
                     lag6_daily[,-6:-12])
mutiair<-NULL
for(i in c(2:7)){
  mutiair<-paste(rownames(min_all_35_50)[i],"+",mutiair,sep="")
}
p<-names(lag1_daily)[26]

formula_all_35_50<-paste(p,"~",mutiair,"+as.factor(wday)",'+s(as.numeric(TEMP),bs="cs",sp=3,k=4)+s(as.numeric(RH),bs="cs",sp=4,k=4)+s(as.numeric(yday),bs="cs",sp=4,k=4)+s(as.numeric(WS_HR),bs="cs",sp=4,k=4)+s(as.numeric(RAIN),bs="cs",sp=4,k=4)',sep="")
mutires_all_35_50<-gam(formula(formula_all_35_50),data =mutidata,family  = ziP() )
summary(mutires_all_35_50)

#all_50_65
min_all_50_65<-data.frame()
for(i in 1:7){
  min_all_50_65[i,1]<-names(which.min(res_all_50_65[[i]][,1]))
  min_all_50_65[i,2]<-res_all_50_65[[i]][which.min(res_all_50_65[[i]][,1]),1]
  min_all_50_65[i,3]<-res_all_50_65[[i]][which.min(res_all_50_65[[i]][,1]),2]
}
rownames(min_all_50_65)<-rownames(kk)
colnames(min_all_50_65)<-c("lag","p.pv","coeff")
min_all_50_65
mutidata<-data.frame(O3=lag5_daily$O3[-1],
                     NO2=lag6_daily$NO2,
                     lag6_daily[,-6:-12])
mutiair<-NULL
for(i in c(3,7)){
  mutiair<-paste(rownames(min_all_50_65)[i],"+",mutiair,sep="")
}
p<-names(lag1_daily)[27]

formula_all_50_65<-paste(p,"~",mutiair,"+as.factor(wday)",'+s(as.numeric(TEMP),bs="cs",sp=3,k=4)+s(as.numeric(RH),bs="cs",sp=4,k=4)+s(as.numeric(yday),bs="cs",sp=4,k=4)+s(as.numeric(WS_HR),bs="cs",sp=4,k=4)+s(as.numeric(RAIN),bs="cs",sp=4,k=4)',sep="")
mutires_all_50_65<-gam(formula(formula_all_50_65),data =mutidata,family  = ziP() )
summary(mutires_all_50_65)

#all_65
min_all_65<-data.frame()
for(i in 1:7){
  min_all_65[i,1]<-names(which.min(res_all_65[[i]][,1]))
  min_all_65[i,2]<-res_all_65[[i]][which.min(res_all_65[[i]][,1]),1]
  min_all_65[i,3]<-res_all_65[[i]][which.min(res_all_65[[i]][,1]),2]
}
rownames(min_all_65)<-rownames(kk)
colnames(min_all_65)<-c("lag","p.pv","coeff")
min_all_65
mutidata<-data.frame(SO2=lag1_daily$SO2[-1:-3],
                     PM2.5=lag6_daily$PM2.5,
                     NO=lag1_daily$NO[-1:-3],
                     NO2=lag3_daily$NO2[-1],
                     lag6_daily[,-6:-12])
mutiair<-NULL
for(i in c(1,4,6,7)){
  mutiair<-paste(rownames(min_all_65)[i],"+",mutiair,sep="")
}
p<-names(lag1_daily)[28]

formula_all_65<-paste(p,"~",mutiair,"+as.factor(wday)",'+s(as.numeric(TEMP),bs="cs",sp=3,k=4)+s(as.numeric(RH),bs="cs",sp=4,k=4)+s(as.numeric(yday),bs="cs",sp=4,k=4)+s(as.numeric(WS_HR),bs="cs",sp=4,k=4)+s(as.numeric(RAIN),bs="cs",sp=4,k=4)',sep="")
mutires_all_65<-gam(formula(formula_all_65),data =mutidata,family  = ziP() )
summary(mutires_all_65)

##rr &ci in subgroup ana
na.omit(rrcul(mutires_urt_20_35))
na.omit(rrcul(mutires_urt_35_50))
na.omit(rrcul(mutires_urt_50_65))
na.omit(rrcul(mutires_urt_65))

na.omit(rrcul(mutires_all_20_35))
na.omit(rrcul(mutires_all_35_50))
na.omit(rrcul(mutires_all_50_65))
na.omit(rrcul(mutires_all_65))
