patient_k<-read.xlsx(file="E:/碩二上/空汙/就診資料/2017-4_20yo_只高雄縣市.xlsx",sheetIndex=1,header = T,encoding = "UTF-8")
patient_k_csv<-read.csv("E:/碩二上/空汙/就診資料/2017-4_20yo_只高雄縣市.csv")
names(patient_k_csv)<-c("date","708","995.3","708&995.3")
patient_k_csv<-clean_NAdata(patient_k_csv,0)


patient_k_csv[,1]<-as.Date(patient_k_csv[,1])
colnames(date_2017)<-colnames(patient_k_csv)[1]

patient_k_alldate<-merge(date_2017, patient_k_csv, by.x="date",all.x = TRUE)
write.csv(patient_k_alldate,"patient_k_alldate.csv",fileEncoding = "utf-8")
##############################################################################

airandpatient$SO2<-SO2alldata$Mean
airandpatient$CO<-COalldata$Mean
airandpatient$O3<-O3alldata$Mean
airandpatient$PM2.5<-PM2.5alldata$Mean
airandpatient$PM10<-PM10alldata$Mean
airandpatient$NO2<-NO2alldata$Mean
airandpatient$NO<-NOalldata$Mean

  
airandpatient$allergy<-patient_k_alldate$X995.3
airandpatient$hives<-patient_k_alldate$X708
airandpatient$patient<-outpatient_alldate$`708&995.3`

airandpatient$RH<-RHalldata$Mean
airandpatient$TEMP<-TEMPalldata$Mean
airandpatient$rain<-RAINalldata$Mean
airandpatient$WS_HR<-WS_HRalldata$Mean
airandpatient$WIND<-WINDalldata$Mean

data.air_dow<-data.frame(na.omit(airandpatient))
data.air_dow$patient<-data.air_dow$allergy+data.air_dow$hives
data.air_dow$time<-data.air$time

data.air_dow$allergy<-as.numeric(data.air_dow$allergy)
data.air_dow$hives<-as.numeric(data.air_dow$hives)
write.csv(data.air_dow,"data.air_dow.csv",fileEncoding = "utf-8")
