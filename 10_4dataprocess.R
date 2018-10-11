trans.weeks<-function(data){
  data$week<-rep(1:53, each = 7,len = 365)
  week<-1:53
  patient<-tapply(as.numeric(data[,"#people"]),data[,"week"],function(x)sum(x,na.rm = TRUE)) 
  Max<-tapply(as.numeric(data[,"Max"]),data[,"week"],function(x)max(x,na.rm = TRUE))
  Min<-tapply(as.numeric(data[,"Min"]),data[,"week"],function(x)max(x,na.rm = TRUE))
  Med<-tapply(as.numeric(data[,"Med"]),data[,"week"],function(x)max(x,na.rm = TRUE))
  Mean<-tapply(as.numeric(data[,"Mean"]),data[,"week"],function(x)max(x,na.rm = TRUE))
  res<-data.frame(week[-53],Max[-53],Min[-53],Med[-53],Mean[-53],patient[-53])
  colnames(res)<-c("week","Max", "Min","Med","Mean","patient")
  return(res)
}

SO2_weeks<-trans.weeks(SO2_databind)
CO_weeks<-trans.weeks(CO_databind)
O3_weeks<-trans.weeks(O3_databind)
PM2.5_weeks<-trans.weeks(PM2.5_databind)
NO2_weeks<-trans.weeks(NO2_databind)
PM10_weeks<-trans.weeks(PM10_databind)
NO_weeks<-trans.weeks(NO_databind)
####################################3
 # calcSSE <- function(x){
 #   loessMod <- try(loess(patient ~ Max, data=SO2_weeks, span=x), silent=T)
 #   res <- try(loessMod$residuals, silent=T)
 #   if(class(res)!="try-error"){
 #     if((sum(res, na.rm=T) > 0)){
 #       sse <- sum(res^2)  
 #     }
 #   }else{
 #     sse <- 99999
 #   }
 #  return(sse)
 # }
  
 #optim(par=c(0.5), calcSSE, method="SANN")

########################################################
# load data with SO2_weeks
SO2_weeks$index <- 1:nrow(SO2_weeks)  # create index variable
# retail weeks for better graphical understanding
SO2_loessMod25 <- loess(patient ~ Max, data=SO2_weeks, span=0.25) # 25% smoothing span
SO2_loessMod50 <- loess(patient ~ Max, data=SO2_weeks, span=0.50) # 50% smoothing span

# get smoothed output
SO2_smoothed25 <- predict(SO2_loessMod25) 
SO2_smoothed50 <- predict(SO2_loessMod50) 

# Plot it
par(mfrow = c(1, 1))
plot(SO2_weeks$patient, x=SO2_weeks$week, type="l", main="Loess Smoothing and Prediction", xlab="weeks", ylab="SO2",col="red")
legend("topright", cex=0.7,                             
       pch = "l",                                
       col = c("red","green","blue"), 
       legend = c("true patient", "span=0.25", "span=0.5")
)
lines(SO2_smoothed25, x=SO2_weeks$week, col="green")
lines(SO2_smoothed50, x=SO2_weeks$week, col="blue")

#function
loess.plot<-function(data,x){
  attach(data)
  data$index <- 1:nrow(data)  # create index variable
  # retail weeks for better graphical understanding
 loessMod25 <- loess(patient ~ x, data=data, span=0.25) # 25% smoothing span
 loessMod50 <- loess(patient ~ x, data=data, span=0.50) # 50% smoothing span
  
  # get smoothed output
 smoothed25 <- predict(loessMod25) 
 smoothed50 <- predict(loessMod50) 
  
  # Plot it
  par(mfrow = c(1, 1))
  plot(data$patient, x=data$week, type="l", main="Loess Smoothing and Prediction", xlab="weeks", ylab="patient",col="red")
  legend("topright", cex=0.7,                             
         pch = "l",                                
         col = c("red","green","blue"), 
         legend = c("true patient", "span=0.25", "span=0.5")
  )
  lines(smoothed25, x=data$week, col="green")
  lines(smoothed50, x=data$week, col="blue")
}
loess.plot(SO2_weeks,Max)
loess.plot(CO_weeks,Max)
loess.plot(NO2_weeks,Max)
loess.plot(NO_weeks,Max)
loess.plot(O3_weeks,Max)
loess.plot(PM10_weeks,Max)
loess.plot(PM2.5_weeks,Max)
##########################################

trans.lastweek<-function(data){
  data$week<-rep(1:53, each = 7,len = 365)
  week<-1:51
  patient<-tapply(as.numeric(data[,"#people"]),data[,"week"],function(x)sum(x,na.rm = TRUE)) 
  Max<-tapply(as.numeric(data[,"Max"]),data[,"week"],function(x)max(x,na.rm = TRUE))
  Min<-tapply(as.numeric(data[,"Min"]),data[,"week"],function(x)max(x,na.rm = TRUE))
  Med<-tapply(as.numeric(data[,"Med"]),data[,"week"],function(x)max(x,na.rm = TRUE))
  Mean<-tapply(as.numeric(data[,"Mean"]),data[,"week"],function(x)max(x,na.rm = TRUE))
  res<-data.frame(week[c(-52,-53)],Max[c(-52,-53)],Min[c(-52,-53)],Med[c(-52,-53)],Mean[c(-52,-53)],patient[c(-1,-53)])
  colnames(res)<-c("week","Max", "Min","Med","Mean","patient")
  return(res)
}

SO2_lastweek<-trans.lastweek(SO2_databind)
CO_lastweek<-trans.lastweek(CO_databind)
O3_lastweek<-trans.lastweek(O3_databind)
PM2.5_lastweek<-trans.lastweek(PM2.5_databind)
NO2_lastweek<-trans.lastweek(NO2_databind)
PM10_lastweek<-trans.lastweek(PM10_databind)
NO_lastweek<-trans.lastweek(NO_databind)

#LASTWEEK
loess.plot(SO2_lastweek,Max)
loess.plot(CO_lastweek,Max)
loess.plot(NO2_lastweek,Max)
loess.plot(NO_lastweek,Max)
loess.plot(O3_lastweek,Max)
loess.plot(PM10_lastweek,Max)
loess.plot(PM2.5_lastweek,Max)





SO2_lastweek$index <- 1:nrow(SO2_lastweek)  # create index variable
# retail weeks for better graphical understanding
loessMod10 <- loess(patient ~ Max, data=SO2_lastweek, span=0.10) # 10% smoothing span
loessMod25 <- loess(patient ~ Max, data=SO2_lastweek, span=0.25) # 25% smoothing span
loessMod50 <- loess(patient ~ Max, data=SO2_lastweek, span=0.50) # 50% smoothing span

# get smoothed output
smoothed10 <- predict(loessMod10) 
smoothed25 <- predict(loessMod25) 
smoothed50 <- predict(loessMod50) 

# Plot it
par(mfrow = c(1, 1))
plot(SO2_lastweek$patient, x=SO2_lastweek$week, type="l", main="Loess Smoothing and Prediction", xlab="weeks", ylab="SO2")
lines(smoothed10, x=SO2_lastweek$week, col="red")
lines(smoothed25, x=SO2_lastweek$week, col="green")
lines(smoothed50, x=SO2_lastweek$week, col="blue")

