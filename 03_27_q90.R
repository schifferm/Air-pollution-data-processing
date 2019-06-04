#CO_data_cleaned
#SO2_data_cleaned
#O3_data_cleaned
#PM2.5_data_cleaned
#PM10_data_cleaned
#NO2_data_cleaned
#NO_data_cleaned

#CO_data_cleaned_stat1  
#SO2_data_cleaned_stat1
#O3_data_cleaned_stat1
#PM2.5_data_cleaned_stat1
#PM10_data_cleaned_stat1
#NO2_data_cleaned_stat1
#NO_data_cleaned_stat1
databind_q90<-function(tmp){
  max<-matrix(apply(tmp[4:27],1,function(x){quantile(x,probs = seq(0,1,0.1),na.rm=TRUE)}),nrow = 11,ncol = 365)[9,]
  obs<-data.frame(outpatient_alldate$date,
             max,outpatient_alldate$`708&995.3`)
  colnames(obs)<-c("date","Max","#people")
  return(obs)
}
CO_Q90<-databind_q90(CO_data_cleaned_stat1)
SO2_Q90<-databind_q90(SO2_data_cleaned_stat1)
O3_Q90<-databind_q90(O3_data_cleaned_stat1)
PM2.5_Q90<-databind_q90(PM2.5_data_cleaned_stat1)
PM10_Q90<-databind_q90(PM10_data_cleaned_stat1)
NO2_Q90<-databind_q90(NO2_data_cleaned_stat1)
NO_Q90<-databind_q90(NO_data_cleaned_stat1)


CO_Q901<-na.omit(CO_Q90)
SO2_Q901<-na.omit(SO2_Q90)
O3_Q901<-na.omit(O3_Q90)
PM2.5_Q901<-na.omit(PM2.5_Q90)
PM10_Q901<-na.omit(PM10_Q90)
NO2_Q901<-na.omit(NO2_Q90)
NO_Q901<-na.omit(NO_Q90)

data.air_max<-data.air

data.air_max$CO<-CO_Q901$Max
data.air_max$SO2<-SO2_Q901$Max
data.air_max$O3<-O3_Q901$Max
data.air_max$PM2.5<-PM2.5_Q901$Max
data.air_max$PM10<-PM10_Q901$Max
data.air_max$NO2<-NO2_Q901$Max  
data.air_max$NO<-NO_Q901$Max
for(i in 2:8){
pdf(paste(names(data.air_max)[i],"quantile90",".pdf",sep=""),family="GB1",width=6,height = 4) 
plot(data.air_max$date,data.air_max[,i],type="l",ylab="value",xlab="date",main=paste("每日",names(data.air_max)[i],"quantile90%",sep=""))
dev.off()
}

for(i in 2:8){
  pdf(paste(names(data.air)[i],"mean",".pdf",sep=""),family="GB1",width=6,height = 4) 
  plot(data.air_max$date,data.air[,i],type="l",ylab="value",xlab="date",main=paste("每日",names(data.air)[i],"mean",sep=""))
  dev.off()
}
