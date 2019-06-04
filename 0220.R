pdf("CO.pdf",family="GB1",width=6,height = 4) 
plot(data.air$date,data.air[,3],type="l",ylab="ppm",xlab="date",main="一氧化碳每日平均")
dev.off()

pdf("patient.pdf",family="GB1",width=6,height = 4) 
plot(data.air$date,data.air[,12],type="l",ylab="人數",xlab="date",main="每日就診人數")
dev.off()

library(mgcv)
data<-data.air
air1<-data.air$CO
p<-data.air$hives
temp<-data.air$TEMP
rh<-data.air$RH
k=2



results<-gam(as.numeric(p[(k+1):length(p)])~
               s(as.numeric(air1[1:(length(p)-k)]),bs = "cc",sp = 4,k =4)
             +s(as.numeric(temp[1:(length(p)-k)]),bs = "cc",sp = 3,k =4)
             +s(as.numeric(rh[1:(length(p)-k)]),bs = "cc",sp = 4,k =4)
             +s(as.numeric(data$time[(k+1):length(p)]),bs = "cc",sp =4,k=4)
             ,
             data = data,
             family = "poisson")
summary(results)
plot(results,xlab="ppb",ylab="",main = "smooth curve of ozone")
plot(results,xlab="°C",ylab="",main = "smooth curve of temperature")
plot(results,xlab="Relative humidity",ylab="",main = "smooth curve of relative humidity")
plot(results,xlab="2017/1/1~2017/12/31 weekdays",ylab="",main = "smooth curve of time trends")


