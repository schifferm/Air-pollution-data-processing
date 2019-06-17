objects(lag0_daily)
sum(lag0_daily[,"allergy"])
group<-"allergy"
weekday_f<-function(group){
  f<-rep(0,5)
  for (i in 1:5) {
    f[i]<-sum(lag0_daily[which(lag0_daily$wday==i),group])
  }
  d_f<-data.frame(weekday=c(rep(1,f[1]),
                            rep(2,f[2]),
                            rep(3,f[3]),
                            rep(4,f[4]),
                            rep(5,f[5])
  ))
  return(list(d_f,f))
}

allergy_weekday_f<-data.frame(weekday_f("allergy")[[1]])
allergyF_weekday_f<-data.frame(weekday_f("allergy_F")[[1]])
allergyM_weekday_f<-data.frame(weekday_f("allergy_M")[[1]])
allergy20_weekday_f<-data.frame(weekday_f("allergy_20")[[1]])
allergy65_weekday_f<-data.frame(weekday_f("allergy_65")[[1]])
sum(allergy_weekday_f$weekday)

urticaria_weekday_f<-data.frame(weekday_f("urticaria")[[1]])
urticariaF_weekday_f<-data.frame(weekday_f("urticaria_F")[[1]])
urticariaM_weekday_f<-data.frame(weekday_f("urticaria_M")[[1]])
urticaria20_weekday_f<-data.frame(weekday_f("urticaria_20")[[1]])
urticaria65_weekday_f<-data.frame(weekday_f("urticaria_65")[[1]])

urt_s<-data.frame(f=weekday_f("urticaria")[[2]])
pdf("outpatints_wd",family="GB1",width=12,height = 7) 
par(mar = c(5,5,3,5))
barplot(height = urt_s$f,names.arg = c("Mon","Tue","Wed","Thr","Fri"),
        main="outpatients of Weekday in 2014~2017",        
        xlab="Weekday",                       
        ylab="Frequency(outpatients)" )
dev.off()

urt_s1<-data.frame(f=weekday_f("allergy")[[2]])
pdf("outpatients_wd.pdf",family="GB1",width=12,height = 7) 
par(mar = c(5,5,4,4),mfcol=c(1,2))
barplot(height = urt_s1$f,names.arg = c("Mon","Tue","Wed","Thr","Fri"),
        main="過敏",        
        xlab="",                       
        ylab="Frequency(outpatients)" )
barplot(height = urt_s$f,names.arg = c("Mon","Tue","Wed","Thr","Fri"),
        main="蕁麻疹",        
        xlab="",                       
        ylab="Frequency(outpatients)" )
dev.off()

urt_s3<-data.frame(f=lag0_daily$allergy)
urt_s4<-data.frame(f=lag0_daily$urticaria)
hist(urt_s4$f)
pdf("outpatients_zero.pdf",family="GB1",width=12,height = 7) 
par(mar = c(5,5,4,4),mfcol=c(1,2))
hist(  urt_s3$f,
        main="過敏",        
        xlab="就診人數"                      
         )
hist(  urt_s4$f,
       main="蕁麻疹",        
       xlab="就診人數"                      
)
dev.off()

urt_s5<-data.frame(f=lag0_daily$allergy_M)
urt_s6<-data.frame(f=lag0_daily$allergy_F)
urt_s7<-data.frame(f=lag0_daily$allergy_20)
urt_s8<-data.frame(f=lag0_daily$allergy_65)


pdf("outpatients_sub.pdf",family="GB1",width=12,height = 7) 
par(mar = c(4,4,3,3),mfcol=c(2,2))
hist(  urt_s5$f,
       main="男性",        
       xlab="每日就診人數"                      
)
hist(  urt_s6$f,
       main="女性",        
       xlab="每日就診人數"                      
)
hist(  urt_s7$f,
       main="20至65歲",        
       xlab="每日就診人數"                      
)
hist(  urt_s8$f,
       main="65歲以上",        
       xlab="每日就診人數"                      
)
dev.off()

urt_s9<-data.frame(f=lag0_daily$urticaria_M)
urt_s10<-data.frame(f=lag0_daily$urticaria_F)
urt_s11<-data.frame(f=lag0_daily$urticaria_20)
urt_s12<-data.frame(f=lag0_daily$urticaria_65)


pdf("outpatients_sub1.pdf",family="GB1",width=12,height = 7) 
par(mar = c(4,4,3,3),mfcol=c(2,2))
hist(  urt_s9$f,
       main="男性",        
       xlab="每日就診人數"                      
)
hist(  urt_s10$f,
       main="女性",        
       xlab="每日就診人數"                      
)
hist(  urt_s11$f,
       main="20至65歲",        
       xlab="每日就診人數"                      
)
hist(  urt_s12$f,
       main="65歲以上",        
       xlab="每日就診人數"                      
)
dev.off()
pdf("mutires_allergy_1.pdf",family="GB1",width=12,height = 7)
plot(mutires_allergy,select = 1,xlab = "溫度",ylab="就診人數")
dev.off()
plot(x=lag0_daily$SO2,y=lag0_daily$allergy)
abline(lm(lag0_daily$allergy~lag0_daily$SO2),lwd=1,col="red")
par(mar = c(5,5,5,10))
plot(x=lag0_daily$date,y=lag0_daily$urticaria,type="l",
     ylab = "outpatients",xlab="date")
plot(x=lag0_daily$TEMP,y=lag0_daily$urticaria,
     ylab="outpatients",xlab="Temp")

plot(x=lag0_daily$TEMP,y=lag0_daily$O3,
     ylab="Wind(m/sec)",xlab="Ozone(ppb)")
#res_allergy
plot(x=1:8,y=exp(unlist(res_allergy$NO2[,2])),
     ylab="Odds ratio",xlab="lag",ylim = c(0.95,1.05))

plot(x=1:8,y=exp(unlist(res_allergy$NO[,2])),
     ylab="Odds ratio",xlab="lag",ylim = c(0.98,1.06))

plot(x=1:8,y=exp(unlist(res_allergy$CO[,2])),
     ylab="Odds ratio",xlab="lag",ylim = c(0,2))

plot(x=1:8,y=exp(unlist(res_allergy$PM2.5[,2])),
     ylab="Odds ratio",xlab="lag",ylim = c(0.99,1.01))

plot(x=1:8,y=exp(unlist(res_allergy$PM10[,2])),
     ylab="Odds ratio",xlab="lag",ylim = c(0.99,1.01))

plot(x=1:8,y=exp(unlist(res_urticaria$PM10[,2])),
     ylab="Odds ratio",xlab="lag",ylim = c(0.99,1.01))

plot(y=lag0_daily$urticaria,x=lag0_daily$TEMP,
     ylab="蕁麻疹之每日就診人數",
     xlab="溫度")

############CORR
colnames(lag0_daily)
corr <- cor.test(x=lag0_daily[,7], y=lag0_daily[,8], method = 'spearman')
objects(corr)
corr$estimate[[1]]

corr_df<-data.frame()
for(i in 1:7){
  for(j in 1:7){
    corr <- cor.test(x=lag0_daily[,i+5], y=lag0_daily[,j+5], method = 'spearman')
    corr_df[i,j]<-round(corr$estimate[[1]],2)
  }
}

corr_df2<-data.frame()
for(i in 1:7){
  for(j in 1:7){
    corr <- cor.test(x=lag0_daily[,i+5], y=lag0_daily[,j+5], method = 'spearman')
    corr_df2[i,j]<-round(corr$p.value[[1]],5)
  }
}
View(corr_df)
View(corr_df2)
colnames(corr_df)<-colnames(lag0_daily)[6:12]
rownames(corr_df)<-colnames(lag0_daily)[6:12]

cor.test(x=lag0_daily[,7], y=lag0_daily[,6], method = 'spearman')
############################
#exp risk & ci

