library(mgcv)
#airandpatient_weekday
loess(patient~SO2,data = airandpatient_weekday,span=0.25)
result_SO2<-gam(patient~s(SO2,bs = "cc",sp = 4)+SO2,data = airandpatient_weekday)
summary(result_SO2)
plot(result_SO2,se=T)

fit_SO2<-predict.gam(result_SO2,data=airandpatient_weekday$SO2)
plot(x=airandpatient_weekday$date,y=airandpatient_weekday$patient
     ,pch=20,col="#ff6666",ylab ="就診人數", xlab = "SO2",ylim = c(0,34))
par(new=T)
plot(x=airandpatient_weekday$date,y=fit_SO2
     ,pch=20,col="#6699ff",ylab ="就診人數", xlab = "SO2",ylim = c(0,34))

 

 

result_CO<-gam(patient~s(CO,bs = "cc",sp = 4),data = airandpatient_weekday)
summary(result_CO)
plot(result_CO,se=T)

fit_CO<-predict.gam(result_CO,data=airandpatient_weekday$CO)
plot(x=airandpatient_weekday$date,y=airandpatient_weekday$patient
     ,pch=20,col="#ff6666",ylab ="就診人數", xlab = "CO",ylim = c(0,34))
par(new=T)
plot(x=airandpatient_weekday$date,y=fit_CO
     ,pch=20,col="#6699ff",ylab ="就診人數", xlab = "CO",ylim = c(0,34))

 

 

result_NO<-gam(patient~s(NO,bs = "cc",sp = 4),data = airandpatient_weekday)
summary(result_NO)
plot(result_NO,se=T)

fit_NO<-predict.gam(result_NO,data=airandpatient_weekday$NO)
plot(x=airandpatient_weekday$date,y=airandpatient_weekday$patient
     ,pch=20,col="#ff6666",ylab ="就診人數", xlab = "NO",ylim = c(0,34))
par(new=T)
plot(x=airandpatient_weekday$date,y=fit_NO
     ,pch=20,col="#6699ff",ylab ="就診人數", xlab = "NO",ylim = c(0,34))

 

 

result_NO2<-gam(patient~s(NO2,bs = "cc",sp = 4),data = airandpatient_weekday)
summary(result_NO2)
plot(result_NO2,se=T)

fit_NO2<-predict.gam(result_NO2,data=airandpatient_weekday$NO2)
plot(x=airandpatient_weekday$date,y=airandpatient_weekday$patient
     ,pch=20,col="#ff6666",ylab ="就診人數", xlab = "NO2",ylim = c(0,34))
par(new=T)
plot(x=airandpatient_weekday$date,y=fit_NO2
     ,pch=20,col="#6699ff",ylab ="就診人數", xlab = "NO2",ylim = c(0,34))

 

 

result_O3<-gam(patient~s(O3,bs = "cc",sp = 4),data = airandpatient_weekday)
summary(result_O3)
plot(result_O3,se=T)

fit_O3<-predict.gam(result_O3,data=airandpatient_weekday$O3)
plot(x=airandpatient_weekday$date,y=airandpatient_weekday$patient
     ,pch=20,col="#ff6666",ylab ="就診人數", xlab = "O3",ylim = c(0,34))
par(new=T)
plot(x=airandpatient_weekday$date,y=fit_O3
     ,pch=20,col="#6699ff",ylab ="就診人數", xlab = "O3",ylim = c(0,34))

 

 

result_PM10<-gam(patient~s(PM10,bs = "cc",sp = 4),data = airandpatient_weekday)
summary(result_PM10)
plot(result_PM10,se=T)

fit_PM10<-predict.gam(result_PM10,data=airandpatient_weekday$PM10)
plot(x=airandpatient_weekday$date,y=airandpatient_weekday$patient
     ,pch=20,col="#ff6666",ylab ="就診人數", xlab = "PM10",ylim = c(0,34))
par(new=T)
plot(x=airandpatient_weekday$date,y=fit_PM10
     ,pch=20,col="#6699ff",ylab ="就診人數", xlab = "PM10",ylim = c(0,34))

 

 

result_PM2.5<-gam(patient~s(PM2.5,bs = "cc",sp = 4),data = airandpatient_weekday)
summary(result_PM2.5)
plot(result_PM2.5,se=T)

fit_PM2.5<-predict.gam(result_PM2.5,data=airandpatient_weekday$PM2.5)
plot(x=airandpatient_weekday$date,y=airandpatient_weekday$patient
     ,pch=20,col="#ff6666",ylab ="就診人數", xlab = "PM2.5",ylim = c(0,34))
par(new=T)
plot(x=airandpatient_weekday$date,y=fit_PM2.5
     ,pch=20,col="#6699ff",ylab ="就診人數", xlab = "PM2.5",ylim = c(0,34))

 


 
result_air<-gam(patient~s(SO2,bs = "cc",sp=4)+s(O3,bs = "cc",sp = 4)+s(CO,bs = "cc",sp = 4)+s(NO,bs = "cc",sp = 4)+s(NO2,bs = "cc",sp = 4)+s(PM2.5,bs = "cc",sp = 4)+s(PM10,bs = "cc",sp = 4)
                ,data = airandpatient_weekday)
summary(result_air)
plot(result_air,se=T)

fit_air<-predict.gam(result_air,data=airandpatient_weekday)
plot(x=airandpatient_weekday$date,y=airandpatient_weekday$patient
     ,pch=20,col="#ff6666",ylab ="就診人數", xlab = "PM2.5",ylim = c(0,34))
par(new=T)
plot(x=airandpatient_weekday$date,y=fit_PM2.5
     ,pch=20,col="#6699ff",ylab ="就診人數", xlab = "PM2.5",ylim = c(0,34))
 

 

 

 

result_SO2<-gam(patient[5:249]~s(SO2[1:245],bs = "cc",sp = 4),data = airandpatient_weekday)
summary(result_SO2)
plot(result_SO2,se=T)

fit_SO2<-predict.gam(result_SO2,data=airandpatient_weekday$SO2)
plot(x=airandpatient_weekday$date,y=airandpatient_weekday$patient
     ,pch=20,col="#ff6666",ylab ="就診人數", xlab = "SO2",ylim = c(0,34))
par(new=T)
plot(x=airandpatient_weekday$date[5:249],y=fit_SO2
     ,pch=20,col="#6699ff",ylab ="就診人數", xlab = "SO2",ylim = c(0,34))

 

 

result_CO<-gam(patient[3:249]~s(CO[1:247],bs = "cc",sp = 4),data = airandpatient_weekday)
summary(result_CO)
plot(result_CO,se=T)

fit_CO<-predict.gam(result_CO,data=airandpatient_weekday$CO)
plot(x=airandpatient_weekday$date,y=airandpatient_weekday$patient
     ,pch=20,col="#ff6666",ylab ="就診人數", xlab = "CO",ylim = c(0,34))
par(new=T)
plot(x=airandpatient_weekday$date[3:249],y=fit_CO
     ,pch=20,col="#6699ff",ylab ="就診人數", xlab = "CO",ylim = c(0,34))

 

 

result_NO<-gam(patient[3:249]~s(NO[1:247],bs = "cc",sp = 4),data = airandpatient_weekday)
summary(result_NO)
plot(result_NO,se=T)

fit_NO<-predict.gam(result_NO,data=airandpatient_weekday$NO)
plot(x=airandpatient_weekday$date,y=airandpatient_weekday$patient
     ,pch=20,col="#ff6666",ylab ="就診人數", xlab = "NO",ylim = c(0,34))
par(new=T)
plot(x=airandpatient_weekday$date[3:249],y=fit_NO
     ,pch=20,col="#6699ff",ylab ="就診人數", xlab = "NO",ylim = c(0,34))

 

 

result_NO2<-gam(patient[4:249]~s(NO2[1:246],bs = "cc",sp = 4),data = airandpatient_weekday)
summary(result_NO2)
plot(result_NO2,se=T)

fit_NO2<-predict.gam(result_NO2,data=airandpatient_weekday$NO2)
plot(x=airandpatient_weekday$date,y=airandpatient_weekday$patient
     ,pch=20,col="#ff6666",ylab ="就診人數", xlab = "NO2",ylim = c(0,34))
par(new=T)
plot(x=airandpatient_weekday$date[4:249],y=fit_NO2
     ,pch=20,col="#6699ff",ylab ="就診人數", xlab = "NO2",ylim = c(0,34))

 

 

result_O3<-gam(patient[3:249]~s(O3[1:247],bs = "cc",sp = 4),data = airandpatient_weekday)
summary(result_O3)
plot(result_O3,se=T)

fit_O3<-predict.gam(result_O3,data=airandpatient_weekday$O3)
plot(x=airandpatient_weekday$date,y=airandpatient_weekday$patient
     ,pch=20,col="#ff6666",ylab ="就診人數", xlab = "O3",ylim = c(0,34))
par(new=T)
plot(x=airandpatient_weekday$date[3:249],y=fit_O3
     ,pch=20,col="#6699ff",ylab ="就診人數", xlab = "O3",ylim = c(0,34))

 

 

result_PM10<-gam(patient[3:249]~s(PM10[1:247],bs = "cc",sp = 4),data = airandpatient_weekday)
summary(result_PM10)
plot(result_PM10,se=T)

fit_PM10<-predict.gam(result_PM10,data=airandpatient_weekday$PM10)
plot(x=airandpatient_weekday$date,y=airandpatient_weekday$patient
     ,pch=20,col="#ff6666",ylab ="就診人數", xlab = "PM10",ylim = c(0,34))
par(new=T)
plot(x=airandpatient_weekday$date[3:249],y=fit_PM10
     ,pch=20,col="#6699ff",ylab ="就診人數", xlab = "PM10",ylim = c(0,34))

 

 

result_PM2.5<-gam(patient[5:249]~s(PM2.5[1:245],bs = "cc",sp = 4),data = airandpatient_weekday)
summary(result_PM2.5)
plot(result_PM2.5,se=T)

fit_PM2.5<-predict.gam(result_PM2.5,data=airandpatient_weekday$PM2.5)
plot(x=airandpatient_weekday$date,y=airandpatient_weekday$patient
     ,pch=20,col="#ff6666",ylab ="就診人數", xlab = "PM2.5",ylim = c(0,34))
par(new=T)
plot(x=airandpatient_weekday$date[5:249],y=fit_PM2.5
     ,pch=20,col="#6699ff",ylab ="就診人數", xlab = "PM2.5",ylim = c(0,34))

 


 
result_air<-gam(patient[3:249]~s(SO2[1:247],bs = "cc",sp=4)+s(O3[1:247],bs = "cc",sp = 4)+s(CO[1:247],bs = "cc",sp = 4)+s(NO[1:247],bs = "cc",sp = 4)+s(NO2[1:247],bs = "cc",sp = 4)+s(PM2.5[1:247],bs = "cc",sp = 4)+s(PM10[1:247],bs = "cc",sp = 4)
                ,data = airandpatient_weekday)
summary(result_air)
plot(result_air,se=T)

fit_air<-predict.gam(result_air,data=airandpatient_weekday)
plot(x=airandpatient_weekday$date,y=airandpatient_weekday$patient
     ,pch=20,col="#ff6666",ylab ="就診人數", xlab = "PM2.5",ylim = c(0,34))
par(new=T)
plot(x=airandpatient_weekday$date[3:249],y=fit_air
     ,pch=20,col="#6699ff",ylab ="就診人數", xlab = "PM2.5",ylim = c(0,34))
 