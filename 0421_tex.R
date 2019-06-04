
data_plot<-data_2017[which(data_2017$wday!=6&data_2017$wday!=7),]
plot(x=data_plot$date,y=data_plot$urticaria,type="l")
plot(x=data_plot$date,y=data_plot$CO,type="l")
plot(x=data_plot$date,y=data_plot$allergy,type="l")


#air&patient
data0<-lag0_daily
data0<-lag1_daily
data0<-lag2_daily
data0<-lag3_daily
data0<-lag4_daily
data0<-lag5_daily
data0<-lag6_daily
data0<-lag7_daily
data0<-data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),]

##2017
gam_allergy<-list()
gam_allergy[[1]]<-lag_gam(patientcol = 4,aircol = c(6:12),data = lag0_daily[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),],covcol = c(13:16))
gam_allergy[[2]]<-lag_gam(patientcol = 4,aircol = c(6:12),data = lag1_daily[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),],covcol = c(13:16))
gam_allergy[[3]]<-lag_gam(patientcol = 4,aircol = c(6:12),data = lag2_daily[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),],covcol = c(13:16))
gam_allergy[[4]]<-lag_gam(patientcol = 4,aircol = c(6:12),data = lag3_daily[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),],covcol = c(13:16))
gam_allergy[[5]]<-lag_gam(patientcol = 4,aircol = c(6:12),data = lag4_daily[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),],covcol = c(13:16))
gam_allergy[[6]]<-lag_gam(patientcol = 4,aircol = c(6:12),data = lag5_daily[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),],covcol = c(13:16))
gam_allergy[[7]]<-lag_gam(patientcol = 4,aircol = c(6:12),data = lag6_daily[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),],covcol = c(13:16))
gam_allergy[[8]]<-lag_gam(patientcol = 4,aircol = c(6:12),data = lag7_daily[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),],covcol = c(13:16))

gam_allergy0<-data.frame(gam_allergy)
rownames(gam_allergy0)
res_allergy<-list()
for(i in 1:7){
  res_allergy[[i]]<-matrix(data = gam_allergy0[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("air_p.pv","air_coeff","TEMP","RH","day","WS_HR","RAIN")))
}
for (i in 1:7){
  print(rownames(gam_allergy0)[i])
  print(xtable(data.frame(res_allergy[[i]]),digits = 5))
}
gam_urticaria<-list()
gam_urticaria[[1]]<-lag_gam(patientcol = 3,aircol = c(6:12),data = lag0_daily[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),],covcol = c(13:16))
gam_urticaria[[2]]<-lag_gam(patientcol = 3,aircol = c(6:12),data = lag1_daily[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),],covcol = c(13:16))
gam_urticaria[[3]]<-lag_gam(patientcol = 3,aircol = c(6:12),data = lag2_daily[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),],covcol = c(13:16))
gam_urticaria[[4]]<-lag_gam(patientcol = 3,aircol = c(6:12),data = lag3_daily[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),],covcol = c(13:16))
gam_urticaria[[5]]<-lag_gam(patientcol = 3,aircol = c(6:12),data = lag4_daily[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),],covcol = c(13:16))
gam_urticaria[[6]]<-lag_gam(patientcol = 3,aircol = c(6:12),data = lag5_daily[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),],covcol = c(13:16))
gam_urticaria[[7]]<-lag_gam(patientcol = 3,aircol = c(6:12),data = lag6_daily[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),],covcol = c(13:16))
gam_urticaria[[8]]<-lag_gam(patientcol = 3,aircol = c(6:12),data = lag7_daily[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),],covcol = c(13:16))
gam_urticaria0<-data.frame(gam_urticaria)

res_urticaria<-list()
for(i in 1:7){
  res_urticaria[[i]]<-matrix(data = gam_urticaria0[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("air_p.pv","air_coeff","TEMP","RH","day","WS_HR","RAIN")))
}

for (i in 1:7){
  print(rownames(gam_urticaria0)[i])
  print(xtable(data.frame(res_urticaria[[i]]),digits = 5))
}
##
#PATIENT
for(i in 3:4){
  pdf(paste("2017_",names(data0)[i],"daily",".pdf",sep=""),family="GB1",width=6,height = 4) 
  plot(data0$date,data0[,i],type="l",ylab="ppb",xlab="date",main=paste("2017 daily count of ",names(data0)[i],sep=""))
  dev.off()
}
#air
for(i in 6:12){
  pdf(paste("2017_",names(data0)[i],"daily",".pdf",sep=""),family="GB1",width=6,height = 4) 
  plot(data0$date,data0[,i],type="l",ylab="ppb",xlab="date",main=paste("2017 daily mean of ",names(data0)[i],sep=""))
  dev.off()
}

for(j in 3:4){
  for(i in 6:12){
    pdf(paste(names(data0)[i],"and",names(data0)[j],"4year",".pdf",sep=""),family="GB1",width=6,height = 4) 
    plot(data0[,i],data0[,j],type="p",ylab="patient",xlab=paste(names(data0)[i],"observations"),main=paste("daily mean of lag0 ",names(data0)[i],sep=""))
    a<-line(data0[,j]~data0[,i])
    abline(a=a$coefficients[1],b=a$coefficients[2],col="red")
    dev.off()
  }
}

data0<-lag7_daily
plot(data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),9],
     data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),3],type="p",ylab="patient",xlab=paste(names(lag7_daily)[9],"observations"),main=paste("daily mean of ",names(data_2017)[9],sep=""))
a<-line(data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),3]~
          data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),9])
abline(a=a$coefficients[1],b=a$coefficients[2],col="red")

##
i=6
j=3
par(mar = c(5,5,3,5))
plot(y=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),i], type = "l", ylab = paste("daily mean of ",names(data0)[i],sep = ""),
     x=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),1],
     main = "", xlab = "date",
     col = "blue")
par(new = TRUE)
plot(y=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),j], type = "l", xaxt = 'n', yaxt='n', ylab = '', xlab = '',
     x=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),1],
     lty = 2, col = "red")
axis(side = 4)
mtext(paste(names(data0)[j]), side = 4, line = 3) 
legend("topright", c(paste(names(data0)[i]), paste(names(data0)[j])),
       col = c("blue", "red"), lty = c(1, 2),lwd = 1)
##
data0<-lag0_daily
data0<-lag1_daily
data0<-lag2_daily
data0<-lag3_daily
data0<-lag4_daily
data0<-lag5_daily
data0<-lag6_daily
data0<-lag7_daily
data0<-data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),]

for(j in 3:4){
  for(i in 6:12){
    pdf(paste(names(data0)[i],"and",names(data0)[j],"with 2017 daily lag7",".pdf",sep=""),family="GB1",width=6,height = 4) 
    par(mar = c(5,5,3,5))
    plot(y=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),i], type = "l", ylab = paste("daily mean of ",names(data0)[i],sep = ""),
         x=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),1],
         main = "", xlab = "month",
         col = "blue")
    par(new = TRUE)
    plot(y=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),j], type = "l", xaxt = 'n', yaxt='n', ylab = '', xlab = '',
         x=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),1],
         lty = 2, col = "red")
    axis(side = 4)
    mtext(paste(names(data0)[j]), side = 4, line = 3) 
    legend("topright", c(paste(names(data0)[i]), paste(names(data0)[j])),
           col = c("blue", "red"), lty = c(1, 2),lwd = 1)
    dev.off()
  }
}

