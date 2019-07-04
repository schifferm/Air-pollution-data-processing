#plot air data
data0<-lag0_daily
for(i in 6:12){
  pdf(paste(names(data0)[i],"_obs plot",".pdf",sep=""),family="GB1",width=6,height = 4)
  par(mar = c(5,5,5,5))
  plot(y=data0[,i], type = "l", ylab = paste("daily mean of ",names(data0)[i],sep = ""),
       x=data0[,1],
       main = "", xlab = "date",
       col = "skyblue")
  par(new = TRUE)
  loessMod25<-loess(data0[,i] ~ data0[,5], data=data0, span=0.15)
  smoothed25<-predict(loessMod25) 
  lines(smoothed25, x=data0$date, col="red",lwd=2)
  dev.off()
}
#
par(mar = c(4,4,3,3),mfcol=c(1,7))
pdf(paste("air_obs plot",".pdf",sep=""),family="GB1",width=6,height = 4)
for(i in 6:12){
  plot(y=data0[,i], type = "l", ylab = paste("daily mean of ",names(data0)[i],sep = ""),
       x=data0[,1],
       main = "", xlab = "date",
       col = "skyblue")
  par(new = TRUE)
  loessMod25<-loess(data0[,i] ~ data0[,5], data=data0, span=0.15)
  smoothed25<-predict(loessMod25) 
  lines(smoothed25, x=data0$date, col="red",lwd=2)
}
dev.off()
par(mar = c(4,4,3,3),mfcol=c(1,7))
pdf(paste("patient_obs plot",".pdf",sep=""),family="GB1",width=6,height = 4)
for(i in 3:4){
  plot(y=data0[,i], type = "l", ylab = paste("daily mean of ",names(data0)[i],sep = ""),
       x=data0[,1],
       main = "", xlab = "date",
       col = "skyblue")
  par(new = TRUE)
  loessMod25<-loess(data0[,i] ~ data0[,5], data=data0, span=0.15)
  smoothed25<-predict(loessMod25) 
  lines(smoothed25, x=data0$date, col="red",lwd=2)
}
dev.off()

  pdf(paste("patient_obs plot ALL",".pdf",sep=""),family="GB1",width=6,height = 4)
  par(mar = c(2,4,2,3),mfcol=c(2,1))
  plot(y=data0[,3], type = "l", ylab = "每日就診人數",main="蕁麻疹",
       x=data0[,1],
        xlab = "",
       col = "skyblue",cex=0.8)
  par(new = TRUE)
  loessMod25<-loess(data0[,3] ~ data0[,5], data=data0, span=0.15)
  smoothed25<-predict(loessMod25) 
  lines(smoothed25, x=data0$date, col="red",lwd=1)

  plot(y=data0[,4], type = "l", ylab = "每日就診人數",main="濕疹",
       x=data0[,1],
       xlab = "年",
       col = "skyblue",cex=0.8)
  par(new = TRUE)
  loessMod25<-loess(data0[,4] ~ data0[,5], data=data0, span=0.15)
  smoothed25<-predict(loessMod25) 
  lines(smoothed25, x=data0$date, col="red",lwd=1)
  dev.off()

  pdf(paste("patient_cov",".pdf",sep=""),family="GB1",width=6,height = 4)
  for(i in 3:4){
    for(j in 13:16 ){
    plot(y=data0[,i], pch=20, ylab = paste(names(data0)[i],sep = ""),
         x=data0[,j],
         main = "", xlab = paste(names(data0)[j],sep = ""),
         col = "skyblue")
      par(new = TRUE)
      loessMod25<-loess(data0[,i] ~ data0[,j], data=data0, span=0.15)
      smoothed25<-predict(loessMod25) 
      lines(smoothed25, x=data0$date, col="red",lwd=1)  
    }
  }
  dev.off()
  
  plot(y=data0[,3], pch=20,ylab = paste("daily mean of ",names(data0)[3],sep = ""),
       x=data0[,13],
       main = "", xlab = "date",
       col = "skyblue")
  

  