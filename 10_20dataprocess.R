
ptm <- proc.time()

SO2_databind<-databind(SO2_data_cleaned_stat1)
CO_databind<-databind(CO_data_cleaned_stat1)
O3_databind<-databind(O3_data_cleaned_stat1)
PM2.5_databind<-databind(PM2.5_data_cleaned_stat1)
NO2_databind<-databind(NO2_data_cleaned_stat1)
PM10_databind<-databind(PM10_data_cleaned_stat1)
NO_databind<-databind(NO_data_cleaned_stat1)

proc.time() - ptm

##########################################
SO2_weekday<-na.omit(SO2_databind)
CO_weekday<-na.omit(CO_databind)
O3_weekday<-na.omit(O3_databind)
PM2.5_weekday<-na.omit(PM2.5_databind)
NO2_weekday<-na.omit(NO2_databind)
PM10_weekday<-na.omit(PM10_databind)
NO_weekday<-na.omit(NO_databind)

plot.dot<-function(data){
par(mfrow = c(2, 2))
plot(x=data$Max,y=data$`#people`,type = "p",col="#66b3ff",pch=20,
     main = "Max V.S. 就診人數", ylab ="就診人數", xlab = )
abline(lm(`#people`~Max, data),                          
       lwd=1,col="red")
plot(x=data$Min,y=data$`#people`,type = "p",col="#66b3ff",pch=20,
     main = "Min V.S. 就診人數", ylab ="就診人數", xlab = "")
abline(lm(`#people`~Min, data),                          
       lwd=1,col="red")
plot(x=data$Med,y=data$`#people`,type = "p",col="#66b3ff",pch=20,
     main = "Med V.S. 就診人數", ylab ="就診人數", xlab = "")
abline(lm(`#people`~Med, data),                          
       lwd=1,col="red")
plot(x=data$Mean,y=data$`#people`,type = "p",col="#66b3ff",pch=20,
     main = "Mean V.S. 就診人數", ylab ="就診人數", xlab = "")
abline(lm(`#people`~Mean, data),                          
       lwd=1,col="red")
}
##########################################

plot.dot(SO2_weekday)
plot.dot(CO_weekday)
plot.dot(NO2_weekday)
plot.dot(NO_weekday)
plot.dot(O3_weekday)
plot.dot(PM10_weekday)
plot.dot(PM2.5_weekday)

########################################
#function
detach()
sort(O3_weekday$Max)
loess.plot<-function(data,x,title="Loess Smoothing",xlab,n=1){
  attach(data)
  data$index <- 1:nrow(data)  # create index variable
  # retail weeks for better graphical understanding
  loessMod25 <- loess(`#people` ~ x, data=data, span=0.25) # 25% smoothing span
  loessMod50 <- loess(`#people` ~ x, data=data, span=0.50) # 50% smoothing span
  # get smoothed output
  smoothed25 <- predict(loessMod25) 
  smoothed50 <- predict(loessMod50) 
  # Plot it
  plot(data$`#people`, x=data$Max, type="p",pch=20, main=title,xlab=xlab ,ylab="patient",col="#66b3ff")
  legend("topright", cex=0.7,                             
         pch = "l",                                
         col = c("#66b3ff","green","red"), 
         legend = c("true patient", "span=0.25", "span=0.5")
  )
  lines(smoothed25, x=sort(data$Max), col="green")
  lines(smoothed50, x=sort(data$Max), col="red")
}
summary(loessMod25)
########################################
par(mfrow = c(1,1))
loess.plot(SO2_weekday,Max,xlab="SO2") 
loess.plot(CO_weekday,Max,xlab="CO")
loess.plot(NO2_weekday,Max,xlab="NO2")
loess.plot(NO_weekday,Max,xlab="NO")
loess.plot(O3_weekday,Max,xlab="O3")
loess.plot(PM10_weekday,Max,xlab="PM10")
loess.plot(PM2.5_weekday,Max,xlab="PM2.5")

par(mfrow = c(2, 2))
loess.plot(SO2_weekday,Max,"Maximum","SO2")
loess.plot(SO2_weekday,Mean,"Mean","SO2")
loess.plot(SO2_weekday,Med,"Median","SO2")
loess.plot(SO2_weekday,Min,"Minimum","SO2")

par(mfrow = c(2, 2))
loess.plot(CO_weekday,Max,"Maximum","CO")
loess.plot(CO_weekday,Mean,"Mean","CO")
loess.plot(CO_weekday,Med,"Median","CO")
loess.plot(CO_weekday,Min,"Minimum","CO")

par(mfrow = c(2, 2))
loess.plot(NO2_weekday,Max,"Maximum","NO2")
loess.plot(NO2_weekday,Mean,"Mean","NO2")
loess.plot(NO2_weekday,Med,"Median","NO2")
loess.plot(NO2_weekday,Min,"Minimum","NO2")

par(mfrow = c(2, 2))
loess.plot(NO_weekday,Max,"Maximum","NO")
loess.plot(NO_weekday,Mean,"Mean","NO")
loess.plot(NO_weekday,Med,"Median","NO")
loess.plot(NO_weekday,Min,"Minimum","NO")

par(mfrow = c(2, 2))
loess.plot(O3_weekday,Max,"Maximum","O3")
loess.plot(O3_weekday,Mean,"Mean","O3")
loess.plot(O3_weekday,Med,"Median","O3")
loess.plot(O3_weekday,Min,"Minimum","O3")

par(mfrow = c(2, 2))
loess.plot(PM2.5_weekday,Max,"Maximum","PM2.5")
loess.plot(PM2.5_weekday,Mean,"Mean","PM2.5")
loess.plot(PM2.5_weekday,Med,"Median","PM2.5")
loess.plot(PM2.5_weekday,Min,"Minimum","PM2.5")

par(mfrow = c(2, 2))
loess.plot(PM10_weekday,Max,"Maximum","PM10")
loess.plot(PM10_weekday,Mean,"Mean","PM10")
loess.plot(PM10_weekday,Med,"Median","PM10")
loess.plot(PM10_weekday,Min,"Minimum","PM10")

##############################################

loess.plot<-function(data,x,title="Loess Smoothing",xlab,n=1){
  attach(data)
  data$index <- 1:nrow(data)  # create index variable
  # retail weeks for better graphical understanding
  loessMod25 <- loess(x ~ index, data=data, span=0.25) # 25% smoothing span
  loessMod50 <- loess(x ~ index, data=data, span=0.50)# 50% smoothing span
  # get smoothed output
  smoothed25 <- predict(loessMod25) 
  smoothed50 <- predict(loessMod50) 
  loessModp <- loess(`#people` ~ index, data=data, span=0.25)
  smoothedp <- predict(loessModp) 
  # Plot it
  plot(data$`#people`, x=data$date, type="l", main=title,xlab=xlab ,ylab="patient",col="#66b3ff")
  legend("topright", cex=0.7,                             
         pch = "l",                                
         col = c("#66b3ff","green","red","black"), 
         legend = c("true patient", "span=0.25", "span=0.5","patient,span=0.25")
  )
  lines(smoothed25*n, x=data$date, col="green")
  lines(smoothed50*n, x=data$date, col="red")
  lines(smoothedp, x=data$date, col="black")
}

par(mfrow = c(1,1))
loess.plot(SO2_weekday,Max,xlab="SO2")
loess.plot(CO_weekday,Max,xlab="CO",n=10)
loess.plot(NO2_weekday,Max,xlab="NO2",n=0.5)
loess.plot(NO_weekday,Max,xlab="NO")
loess.plot(O3_weekday,Max,xlab="O3",n=0.1)
loess.plot(PM10_weekday,Max,xlab="PM10",n=0.1)
loess.plot(PM2.5_weekday,Max,xlab="PM2.5",n=0.3)
