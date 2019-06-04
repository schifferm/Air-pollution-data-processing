data.air<-read.csv("DATA/airandpatient.csv",stringsAsFactors = FALSE)
data.air<-read.csv("DATA/airandpatient_weekday.csv",stringsAsFactors = FALSE)

data.air$date<-as.Date(data.air$date)
lag.CO.diff1 <- log(data.air$CO[-1]/data.air$CO[-length(data.air$CO)])

#lag data
x=ts(data.air$CO)
plot(y=x,x=data.air$date,type = "l")
abline(reg=lm(x~data.air$date),col="red")

dx<-diff(x,1)
dp<-diff(data.air$patient,1)
# plot(y=dx5,x=data.air$date[-365],type = "l")
# abline(reg=lm(dx5~data.air$date[-365]),col="red")


plot(y=dx,x=data.air$date[-249],type = "l")
abline(reg=lm(dx~data.air$date[-249]),col="red")

#patient
plot(y=data.air$patient,x=data.air$date,type = "l")
abline(reg=lm(data.air$patient~data.air$date),col="blue")

plot(y=dp,x=data.air$date[-249],type = "l")
abline(reg=lm(dp~data.air$date[-249]),col="blue")

#moving average data

m2<-na.omit(satas::filter(x,satas::filter=c(rep(1/2,2))))
m3<-na.omit(filter(x,filter=c(rep(1/3,3))))
m4<-na.omit(filter(x,filter=c(rep(1/4,4))))
m5<-na.omit(filter(x,filter=c(rep(1/5,5))))
m6<-na.omit(filter(x,filter=c(rep(1/6,6))))
m7<-na.omit(filter(x,filter=c(rep(1/7,7))))
na.omit(m2)
plot(y=x,x=data.air$date,type="l",xlab="time",ylab="ppm",main="CO")
abline(reg=lm(m5~data.air$date),col="red")
plot(y=x,x=data.air$date,type="l",xlab="time",ylab="ppm",main="CO")
abline(reg=lm(m5~data.air$date),col="red")

dm2<-diff(m2)
dm3<-diff(m3)
dm4<-diff(m4)
dm5<-diff(m5)
dm6<-diff(m6)
dm7<-diff(m7)  
# 
# plot(y=dm5,x=data.air$date[-365],type = "l")
# abline(reg=lm(dm5~data.air$date[-365]),col="red")

plot(y=dm5,x=data.air$date[-249],type = "l")
abline(reg=lm(dm5~data.air$date[-249]),col="red")

library(mgcv)

result_CO<-gam(patient[3:249]~dm2[1:247]+s(CO[1:247],bs = "cc",sp = 4),data = data.air)
summary(result_CO)
plot(result_CO,se=T)
result_CO<-gam(patient[4:249]~dm3[2:247]+s(CO[1:246],bs = "cc",sp = 4),data = data.air)
summary(result_CO)
plot(result_CO,se=T)
result_CO<-gam(patient[5:249]~dm4[2:246]+s(CO[1:245],bs = "cc",sp = 4),data = data.air)
summary(result_CO)
plot(result_CO,se=T)
result_CO<-gam(patient[6:249]~dm5[3:246]+s(CO[1:244],bs = "cc",sp = 4),data = data.air)
summary(result_CO)
plot(result_CO,se=T)
result_CO<-gam(patient[7:249]~dm6[3:245]+s(CO[1:243],bs = "cc",sp = 4),data = data.air)
summary(result_CO)
plot(result_CO,se=T)
result_CO<-gam(patient[8:249]~dm7[4:245]+s(CO[1:242],bs = "cc",sp = 4),data = data.air)
summary(result_CO)
plot(result_CO,se=T)

result_CO<-gam(patient[3:249]~m2[1:247]+s(CO[1:247],bs = "cc",sp = 4),data = data.air)
summary(result_CO)
plot(result_CO,se=T)
result_CO<-gam(patient[4:249]~m3[2:247]+s(CO[1:246],bs = "cc",sp = 4),data = data.air)
summary(result_CO)
plot(result_CO,se=T)
result_CO<-gam(patient[5:249]~m4[2:246]+s(CO[1:245],bs = "cc",sp = 4),data = data.air)
summary(result_CO)
plot(result_CO,se=T)
result_CO<-gam(patient[6:249]~m5[3:246]+s(CO[1:244],bs = "cc",sp = 4),data = data.air)
summary(result_CO)
plot(result_CO,se=T)
result_CO<-gam(patient[7:249]~m6[3:245]+s(CO[1:243],bs = "cc",sp = 4),data = data.air)
summary(result_CO)
plot(result_CO,se=T)
result_CO<-gam(patient[8:249]~m7[4:245]+s(CO[1:242],bs = "cc",sp = 4),data = data.air)
summary(result_CO)
plot(result_CO,se=T)

result_CO<-gam(dp[7:248]~dm7[4:245]+s(CO[1:242],bs = "cc",sp = 4),data = data.air)
summary(result_CO)

