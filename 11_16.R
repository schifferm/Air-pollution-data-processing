setwd("C:/Users/Schiffer/Documents/GitHub/Air-pollution-data-processing")
airandpatient_weekday$date<-as.Date(airandpatient_weekday$date)
airandpatient<-data.frame(cbind(data.frame(outpatient_alldate$date),
                                SO2_databind$Mean,
                                CO_databind$Mean,
                                O3_databind$Mean,
                                PM2.5_databind$Mean,
                                PM10_databind$Mean,
                                NO2_databind$Mean,
                                NO_databind$Mean,
                                outpatient_alldate$`708&995.3`))
airandpatient<-data.frame(airandpatient[[1]],round(airandpatient[2:9],digits = 4))
colnames(airandpatient)<-c("date","SO2","CO","O3",
                           "PM2.5","PM10","NO2","NO","patient")
write.csv(airandpatient,"airandpatient.csv",fileEncoding = "utf-8",
          row.names=FALSE)
a<-read.csv("airandpatient.csv")

##########################################

airandpatient_weekday<-data.frame(cbind(data.frame(outpatient_csv$date),
                         SO2_weekday$Mean,
                         CO_weekday$Mean,
                         O3_weekday$Mean,
                         PM2.5_weekday$Mean,
                         PM10_weekday$Mean,
                         NO2_weekday$Mean,
                         NO_weekday$Mean,
                         RAIN_weekday$Mean,
                         TEMP_weekday$Mean,
                         RH_weekday$Mean,
                         outpatient_csv$`708&995.3`))
airandpatient_weekday<-data.frame(airandpatient_weekday[[1]],
                                  round(airandpatient_weekday[2:12],digits = 4))
colnames(airandpatient_weekday)<-c("date","SO2","CO","O3",
                                   "PM2.5","PM10","NO2","NO","RAIN","TEMP","RH","patient")
write.csv(airandpatient_weekday,"airandpatient_weekday.csv",fileEncoding = "utf-8",
          row.names=FALSE)
a<-read.csv("DATA/airandpatient_weekday.csv")
x<-"SO2"
y="patient"
plot(x=data$date,y=data[[x]],pch=20,col="#ff6666",
     main = paste("全年",x,"趨勢"), xlab ="date", ylab =paste(x),ylim=c(0,max(data[[x]])))



plot(x=a$date,a[[x]],pch=20,col="#ff6666",
     main = "全年SO2趨勢", xlab ="date", ylab = "SO2",ylim=c(0,max(data[[x]])))

plot.scatter<-function(data,x,y,o=999,n=999,g=999){
  data<-data
  par(mfrow = c(1, 2))
  plot(x=data[["date"]],y=data[[x]],type = "p",pch=20,col="#1a53ff",
       main = paste(x,"每日監測平均值"), xlab ="date", ylab = "AQI",ylim=c(0,max(data[[x]])))
  abline(n,0,                          
         lwd=1,col="#ffff00")
  abline(g,0,                          
         lwd=1,col="green")
  abline(o,0,                          
         lwd=1,col="#ff8c1a")
  plot(x=data[["date"]],data[[y]],pch=20,col="black",
       main = "就診人數", xlab ="date", ylab = "人數",ylim=c(0,max(data[[y]])))
}
setwd("E:/碩二上/空汙")
pdf("airtest.pdf",family="GB1", height =5,width=8)
plot.scatter(airandpatient_weekday,x="SO2",y="patient",n=35)
plot.scatter(airandpatient_weekday,x="CO",y="patient",n=4.4)
plot.scatter(airandpatient_weekday,x="O3",y="patient",n=54)
plot.scatter(airandpatient_weekday,x="NO",y="patient",n=53)
plot.scatter(airandpatient_weekday,x="NO2",y="patient",n=54)
plot.scatter(airandpatient_weekday,x="PM10",y="patient",254,125,54)
plot.scatter(airandpatient_weekday,x="PM2.5",y="patient",54.4,35.4,15.4)
dev.off()

#################################################################
plot.scatter1<-function(data,x,y){
  data<-data
  par(mfrow = c(1,1))
  plot(x=data[[x]],data[[y]],type = "p",col="#1a53ff",pch=20,
       main =  paste(x,"V.S.patients"), xlab ="AQI", ylab = "就診人數",ylim=c(0,max(data[[y]])))
  abline(lm(data[[y]]~data[[x]], data),                          
         lwd=1,col="red")
}
pdf("airvspatients.pdf",family="GB1", height =5,width=8)
plot.scatter1(airandpatient_weekday,x="SO2",y="patient")
plot.scatter1(airandpatient_weekday,x="CO",y="patient")
plot.scatter1(airandpatient_weekday,x="O3",y="patient")
plot.scatter1(airandpatient_weekday,x="NO",y="patient")
plot.scatter1(airandpatient_weekday,x="NO2",y="patient")
plot.scatter1(airandpatient_weekday,x="PM10",y="patient")
plot.scatter1(airandpatient_weekday,x="PM2.5",y="patient")
dev.off()
max(airandpatient_weekday[["patient"]])



loess.plot<-function(data,x,title="Loess Smoothing",xlab,n=1){
  attach(data)
  data$index <- 1:nrow(data)  # create index variable
  # retail weeks for better graphical understanding
  loessMod25 <- loess(x ~ index, data=data, span=0.25) # 25% smoothing span
  loessMod50 <- loess(x ~ index, data=data, span=0.50)# 50% smoothing span
  # get smoothed output
  smoothed25 <- predict(loessMod25) 
  smoothed50 <- predict(loessMod50) 
  loessModp <- loess(patient ~ index, data=data, span=0.25)
  smoothedp <- predict(loessModp) 
  # Plot it
  plot(data$patient, x=data$date,pch=20,type="p", main=title,xlab=xlab ,ylab="patient",col="black")
  legend("topright", cex=0.7,                             
         pch = "l",                                
         col = c("black","red","blue"), 
         legend = c("true patient", "span=0.25","patient,span=0.25")
  )
  lines(smoothed25*n, x=data$date, col="red")
  #lines(smoothed50*n, x=data$date, col="red")
  lines(smoothedp, x=data$date, col="blue")
}


pdf("loessplot.pdf",family="GB1", height =5,width=8)
loess.plot(airandpatient_weekday,SO2,xlab="SO2 AQI",n=2)
loess.plot(airandpatient_weekday,CO,xlab="CO AQI",n=20)
loess.plot(airandpatient_weekday,NO2,xlab="NO2 AQI",n=0.5)
loess.plot(airandpatient_weekday,NO,xlab="NO AQI",n=2.5)
loess.plot(airandpatient_weekday,O3,xlab="O3 AQI",n=0.3)
loess.plot(airandpatient_weekday,PM10,xlab="PM10 AQI",n=0.2)
loess.plot(airandpatient_weekday,PM2.5,xlab="PM2.5 AQI",n=0.3)
dev.off()
##################################################
air.lag<-function(data,x,y,lag,n){
  par(mfrow = c(1,1))
  data=data
  plot(x=data[["date"]],y=data[[y]],pch=20,xlab="time",ylab="patients"
      ,main=paste(x,"Loess Smoothing"))
  
  mp<-filter(data[[y]],filter=c(rep(1/5,5)))
  lines(x=data[["date"]],y=mp,col="blue",cex=1.5)
  mair<-filter(data[[x]],filter=c(rep(1/lag,lag)))
  lines(x=data[["date"]],y=mair*n,col="red",cex=1.5)
  legend("topright", cex=0.7,                             
         pch = "l",                                
         col = c("blue","red","black"), 
         legend = c("patient average per 5 days","air average per 5 days","true patient")
  )
}
pdf("airlag.pdf",family="GB1", height =5,width=8)
air.lag(airandpatient_weekday,"SO2","patient",5,2)
air.lag(airandpatient_weekday,"CO","patient",5,20)
air.lag(airandpatient_weekday,"O3","patient",5,1/5)
air.lag(airandpatient_weekday,"NO","patient",5,2)
air.lag(airandpatient_weekday,"NO2","patient",5,1/2)
air.lag(airandpatient_weekday,"PM10","patient",5,1/5)
air.lag(airandpatient_weekday,"PM2.5","patient",5,1/3)
dev.off()