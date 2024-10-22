#E:/碩二上/空汙/106年 高屏空品區
library(readxl)
library(xlsx)
library(data.table)
setwd("E:/碩二上/空汙/106年 高屏空品區")
path<-("E:/碩二上/空汙/106年 高屏空品區")

#c("SO2","CO","O3","PM2.5","NO2")



##############################load.data#####################################
loaddata<-function(path = "./",confound=NULL,state=NULL){
  setwd(path)
  fns <- list.files(path,pattern = "*.xls")
  res <- NULL
  for(i in fns) {
    data_original<-read_xls(path = i,sheet = 1,col_names = TRUE,col_types  = "guess")
  ifelse(!is.null(confound),
         data_select<-data_original[data_original$測項 %in% confound,],
         data_select<-data_original)
  
  #ifelse(!is.null(state),
  #       data_select2<-data_select[data_selectl$測站 %in% state,],
  #       data_select2<-data_select)
  
  ifelse(!is.null(res),
         res <- rbind(res, data_select), res <- data_select)
  }
  return(res)
}
############################################################################

# ptm <- proc.time()
# 
# all_data<-loaddata(path="E:/碩二上/空汙/106年 高屏空品區",confound=c("SO2","CO","O3","PM2.5","NO2","PM10","NO"))
# 
# proc.time() - ptm
#   user  system elapsed 
#2119.72    6.27 2260.86

ptm <- proc.time()

SO2_data<-loaddata(path="E:/碩二上/空汙/106年 高屏空品區",confound=c("SO2"))
CO_data<-loaddata(path="E:/碩二上/空汙/106年 高屏空品區",confound=c("CO"))
O3_data<-loaddata(path="E:/碩二上/空汙/106年 高屏空品區",confound=c("O3"))
PM2.5_data<-loaddata(path="E:/碩二上/空汙/106年 高屏空品區",confound=c("PM2.5"))
NO2_data<-loaddata(path="E:/碩二上/空汙/106年 高屏空品區",confound=c("NO2"))
PM10_data<-loaddata(path="E:/碩二上/空汙/106年 高屏空品區",confound=c("PM10"))
NO_data<-loaddata(path="E:/碩二上/空汙/106年 高屏空品區",confound=c("NO"))
proc.time() - ptm
##########################################
#3hours
#################clean.#*x###############################################
dataclean<-function(odata){
  pattern = "[#*x]"
  res<-matrix(nrow = dim(odata)[1],ncol = dim(odata)[2])
  for(j in 1:dim(odata)[1]){
    for (i in 1:dim(odata)[2]){
      res[j,i]<-gsub(pattern,replacement="",odata[j,i])
    }
  }
  res_frame<-data.frame(res,stringsAsFactors =FALSE)
  res_frame_num<-data.matrix(res_frame[4:27])
  res_frame_str<-res_frame[2:3]
  res_frame_date<-data.frame(as.Date(res_frame[,1]))
  tmp_data<-cbind(res_frame_date,res_frame_str,res_frame_num)
  return(tmp_data)
}
####################################################################
ptm <- proc.time()

SO2_data_cleaned<-dataclean(SO2_data)
CO_data_cleaned<-dataclean(CO_data)
O3_data_cleaned<-dataclean(O3_data)
PM2.5_data_cleaned<-dataclean(PM2.5_data)
NO2_data_cleaned<-dataclean(NO2_data)
PM10_data_cleaned<-dataclean(PM10_data)
NO_data_cleaned<-dataclean(NO_data)
rain_cleaned<-dataclean(rain)
proc.time() - ptm

# user  system elapsed  
#86.14    0.27   90.90 
##########################################
clean_data <- function(df, impute_value){
  n_rows <- nrow(df)
  na_sum <- rep(NA, times = n_rows)
  for (i in 1:n_rows){
    na_sum[i] <- sum(ifelse(df[i,]<0,TRUE,FALSE)) # 計算每個觀測值有幾個 NA
    df[i, ][ifelse(df[i,]<0,TRUE,FALSE)] <- impute_value # 把 NA 用某個數值取代
  }
  complete_cases <- df[as.logical(!na_sum), ] # 把沒有出現 NA 的觀測值保留下來
  imputed_data <- df
  df_df<-data.frame(imputed_data)
  return(df_df)
}
###########################
SO2_clean<-clean_data(SO2_data_cleaned,0)
NO2_clean<-clean_data(NO2_data_cleaned,0)
CO_clean<-clean_data(CO_data_cleaned,0)
O3_clean<-clean_data(O3_data_cleaned,0)
PM2.5_clean<-clean_data(PM2.5_data_cleaned,0)
PM10_clean<-clean_data(PM10_data_cleaned,0)
NO_clean<-clean_data(NO_data_cleaned,0)
rain_clean<-clean_data(rain_cleaned,0)
###########################
clean_NAdata <- function(df, impute_value){
  n_rows <- nrow(df)
  na_sum <- rep(NA, times = n_rows)
  for (i in 1:n_rows){
    na_sum[i] <- sum(is.na(df[i,])) # 計算每個觀測值有幾個 NA
    df[i, ][is.na(df[i,])] <- impute_value # 把 NA 用某個數值取代
  }
  complete_cases <- df[as.logical(!na_sum), ] # 把沒有出現 NA 的觀測值保留下來
  imputed_data <- df
  df_df<-data.frame(imputed_data)
  return(df_df)
}
###########################
rain_clean<-clean_NAdata(rain_cleaned,0)
###########################
ptm <- proc.time()

SO2_data_cleaned_stat1<-SO2_clean[SO2_clean$X2 %in% "左營",]
CO_data_cleaned_stat1<-CO_clean[CO_clean$X2 %in% "左營",]
O3_data_cleaned_stat1<-O3_clean[O3_clean$X2 %in% "左營",]
PM2.5_data_cleaned_stat1<-PM2.5_clean[PM2.5_clean$X2 %in% "左營",]
NO2_data_cleaned_stat1<-NO2_clean[NO_clean$X2 %in% "左營",]
PM10_data_cleaned_stat1<-PM10_clean[PM10_clean$X2 %in% "左營",]
NO_data_cleaned_stat1<-NO_clean[NO_clean$X2 %in% "左營",]
rain_clean_stat1<-rain_clean[rain_clean$X2 %in% "左營",]
NO_data_cleaned_stat1<-NO_clean[NO_clean$X2 %in% "左營",]
proc.time() - ptm
##creat.date#####
date_2017 = data.frame(seq(from = as.Date("2017-01-01"),to = as.Date("2017-12-31"),by = "day"))
#################
outpatient<-read.xlsx(file="E:/碩二上/空汙/就診資料/20170101-20171231每日人數.xls",sheetIndex=1,header = T,encoding = "UTF-8")
outpatient_csv<-read.csv("E:/碩二上/空汙/就診資料/20170101-20171231每日人數.csv")
outpatient_csv<-outpatient_csv[1:249,1:4]
names(outpatient_csv)<-c("date","708","995.3","708&995.3")
outpatient_csv<-clean_NAdata(outpatient_csv,0)


outpatient_csv[,1]<-as.Date(outpatient_csv[,1])
colnames(date_2017)<-colnames(outpatient_csv)[1]

outpatient_alldate<-merge(date_2017, outpatient_csv, by.x="date",all.x = TRUE)
write.csv(outpatient_alldate,"outpatient_alldate.csv",fileEncoding = "utf-8")

##########Interested information##########
databind<-function(tmp){
  max<-data.frame(apply(tmp[4:27],1,function(x){max(x,na.rm=TRUE)}))
  min<-data.frame(apply(tmp[4:27],1,function(x){min(x,na.rm=TRUE)}))
  mean<-data.frame(apply(tmp[4:27],1,function(x){mean(x,na.rm=TRUE)}))
  med<-data.frame(apply(tmp[4:27],1,function(x){median(x,na.rm=TRUE)}))
  obs<-cbind(patient_k_alldate$date,
             max,mean,med,min,patient_k_alldate$X708,patient_k_alldate$X995.3)
  colnames(obs)<-c("date","Max","Mean","Med","Min","708","995.3")
  return(obs)
  }
#######################################################
#alldata

cal_mean<-function(data){
  tmp<-split(data,data[,1])
  res<-data.frame(matrix(data=NA,nrow = 365,ncol = 15))
  colnames(res)<-as.character(unlist(tmp[[1]][2]))
    for(i in 1:length(tmp)){
      for(j in 1:length(tmp[[1]][,1])){
        res[i,j]<-mean(as.numeric(tmp[[i]][j,4:27]),na.rm=TRUE)
      }
    }
  return(res)
}


databindALL<-function(tmp,patient_year){
  
  mean<-data.frame(apply(tmp,1,function(x){mean(x,na.rm=TRUE)}))
  obs<-cbind(patient_year$date,patient_year$wday,
             mean)
  colnames(obs)<-c("date","wday","Mean")
  return(obs)
}

#######################################################
CO_alldata_mean<-cal_mean(CO_clean)
SO2_alldata_mean<-cal_mean(SO2_clean)
O3_alldata_mean<-cal_mean(O3_clean)
PM2.5_alldata_mean<-cal_mean(PM2.5_clean)
PM10_alldata_mean<-cal_mean(PM10_clean)
NO_alldata_mean<-cal_mean(NO_clean)
NO2_alldata_mean<-cal_mean(NO2_clean)


COalldata<-databindALL(CO_alldata_mean[,c(-8,-9,-14)])
SO2alldata<-databindALL(SO2_alldata_mean[,c(-8,-9,-14)])
O3alldata<-databindALL(O3_alldata_mean[,c(-8,-9,-14)])
PM2.5alldata<-databindALL(PM2.5_alldata_mean[,c(-8,-9,-14)])
PM10alldata<-databindALL(PM10_alldata_mean[,c(-8,-9,-14)])
NOalldata<-databindALL(NO_alldata_mean[,c(-8,-9,-14)])
NO2alldata<-databindALL(NO2_alldata_mean[,c(-8,-9,-14)])
##
RAIN_alldata_mean<-cal_mean(RAIN_clean)
TEMP_alldata_mean<-cal_mean(TEMP_clean)
RH_alldata_mean<-cal_mean(RH_clean)
WIND_alldata_mean<-cal_mean(WIND_clean)
WS_HR_alldata_mean<-cal_mean(WS_HR_clean)

RAINalldata<-databindALL(RAIN_alldata_mean[,c(-8,-9,-14)])
TEMPalldata<-databindALL(TEMP_alldata_mean[,c(-8,-9,-14)])
RHalldata<-databindALL(RH_alldata_mean[,c(-8,-9,-14)])
WINDalldata<-databindALL(WIND_alldata_mean[,c(-8,-9,-14)])
WS_HRalldata<-databindALL(WS_HR_alldata_mean[,c(-8,-9,-14)])

#######################################################
ptm <- proc.time()

SO2_databind<-databind(SO2_data_cleaned_stat1)
CO_databind<-databind(CO_data_cleaned_stat1)
O3_databind<-databind(O3_data_cleaned_stat1)
PM2.5_databind<-databind(PM2.5_data_cleaned_stat1)
NO2_databind<-databind(NO2_data_cleaned_stat1)
PM10_databind<-databind(PM10_data_cleaned_stat1)
NO_databind<-databind(NO_data_cleaned_stat1)
rain_databind<-databind(rain_clean_stat1)
proc.time() - ptm

write.csv(SO2_databind,"SO2_databind.csv",fileEncoding = "utf-8",
          row.names=FALSE)
write.csv(CO_databind,"CO_databind.csv",fileEncoding = "utf-8",
          row.names=FALSE)
write.csv(O3_databind,"O3_databind.csv",fileEncoding = "utf-8",
          row.names=FALSE)
write.csv(PM2.5_databind,"PM2.5_databind.csv",fileEncoding = "utf-8",
          row.names=FALSE)
write.csv(PM10_databind,"PM10_databind.csv",fileEncoding = "utf-8",
          row.names=FALSE)
write.csv(NO2_databind,"NO2_databind.csv",fileEncoding = "utf-8",
          row.names=FALSE)
write.csv(NO_databind,"NO_databind.csv",fileEncoding = "utf-8",
          row.names=FALSE)
#######################################################
ptm <- proc.time()

RAIN_data<-loaddata(path="E:/碩二上/空汙/106年 高屏空品區",confound=c("RAINFALL"))
TEMP_data<-loaddata(path="E:/碩二上/空汙/106年 高屏空品區",confound=c("AMB_TEMP"))
RH_data<-loaddata(path="E:/碩二上/空汙/106年 高屏空品區",confound=c("RH"))
WIND_data<-loaddata(path="E:/碩二上/空汙/106年 高屏空品區",confound=c("WIND_SPEED"))
WS_HR_data<-loaddata(path="E:/碩二上/空汙/106年 高屏空品區",confound=c("WS_HR"))
proc.time() - ptm

ptm <- proc.time()
RAIN_data_cleaned<-dataclean(RAIN_data)
TEMP_data_cleaned<-dataclean(TEMP_data)
RH_data_cleaned<-dataclean(RH_data)
WIND_data_cleaned<-dataclean(WIND_data)
WS_HR_data_cleaned<-dataclean(WS_HR_data)
  
RAIN_clean<-clean_NAdata(RAIN_data_cleaned,0)
TEMP_clean<-clean_data(TEMP_data_cleaned,0)
RH_clean<-clean_data(RH_data_cleaned,0)
WIND_clean<-clean_data(WIND_data_cleaned,0)
WS_HR_clean<-clean_data(WS_HR_data_cleaned,0)

RAIN_data_cleaned_stat1<-RAIN_clean[RAIN_clean$X2 %in% "左營",]
TEMP_data_cleaned_stat1<-TEMP_clean[TEMP_clean$X2 %in% "左營",]
RH_data_cleaned_stat1<-RH_clean[RH_clean$X2 %in% "左營",]

RAIN_databind<-databind(RAIN_data_cleaned_stat1)
TEMP_databind<-databind(TEMP_data_cleaned_stat1)
RH_databind<-databind(RH_data_cleaned_stat1)



proc.time() - ptm

##########################################

##########################################
par(mfrow = c(1, 2))
p2<-plot(x=SO2_databind$date,SO2_databind$Max,type = "l",col="#ff6666",
     main = "全年SO2趨勢", xlab ="date", ylab = "SO2",ylim=c(0,40))
legend("topright", cex=0.5,                             
       pch = "l",                                
       col = c("#ff6666","#99ff66","#6699ff","#ffd633"), 
       legend = c("max", "min", "mean","med")
)
abline(35,0,                          
       lwd=1,col="#ff8c1a")
par(new=T)
p3<-plot(x=SO2_databind$date,y=SO2_databind$Min,type = "l",col="#99ff66",
     main = "全年SO2趨勢", xlab ="date", ylab = "SO2",ylim=c(0,40))
par(new=T)
p4<-plot(x=SO2_databind$date,y=SO2_databind$Mean,type = "l",col="#6699ff",
     main = "全年SO2趨勢", xlab ="date", ylab = "SO2",ylim=c(0,40))
par(new=T)
p5<-plot(x=SO2_databind$date,y=SO2_databind$Med,type = "l",col="#ffd633",
     main = "全年SO2趨勢", xlab ="date", ylab = "SO2",ylim=c(0,40))

p1<-plot(x=SO2_databind$date,y=SO2_databind$`#people`,type = "l",col="#333333",
         main = "就診人數", xlab ="date", ylab = "人數",ylim=c(0,40))

##########################################
par(mfrow = c(2, 2))
plot(x=SO2_databind$Max,y=SO2_databind$`#people`,type = "p",col="#1a53ff",pch=20,
     main = "SO2_Max V.S. 就診人數", ylab ="就診人數", xlab = "SO2")
abline(lm(`#people`~Max, SO2_databind),                          
       lwd=1,col="red")
plot(x=SO2_databind$Min,y=SO2_databind$`#people`,type = "p",col="#1a53ff",pch=20,
     main = "SO2_Min V.S. 就診人數", ylab ="就診人數", xlab = "SO2")
abline(lm(`#people`~Min, SO2_databind),                          
       lwd=1,col="red")
plot(x=SO2_databind$Med,y=SO2_databind$`#people`,type = "p",col="#1a53ff",pch=20,
     main = "SO2_Med V.S. 就診人數", ylab ="就診人數", xlab = "SO2")
abline(lm(`#people`~Med, SO2_databind),                          
       lwd=1,col="red")
plot(x=SO2_databind$Mean,y=SO2_databind$`#people`,type = "p",col="#1a53ff",pch=20,
     main = "SO2_Mean V.S. 就診人數", ylab ="就診人數", xlab = "SO2")
abline(lm(`#people`~Mean, SO2_databind),                          
       lwd=1,col="red")
#############################################################
#CO
##########################################
par(mfrow = c(1, 2))
p2<-plot(x=CO_databind$date,CO_databind$Max,type = "l",col="#ff6666",
         main = "全年CO趨勢", xlab ="date", ylab = "CO",ylim=c(0,5))
legend("topright", cex=0.5,                             
       pch = "l",                                
       col = c("#ff6666","#99ff66","#6699ff","#ffd633"), 
       legend = c("max", "min", "mean","med")
)
abline(4.4,0,                          
       lwd=1,col="#ff8c1a")
par(new=T)
p3<-plot(x=CO_databind$date,y=CO_databind$Min,type = "l",col="#99ff66",
         main = "全年CO趨勢", xlab ="date", ylab = "CO",ylim=c(0,5))
par(new=T)
p4<-plot(x=CO_databind$date,y=CO_databind$Mean,type = "l",col="#6699ff",
         main = "全年CO趨勢", xlab ="date", ylab = "CO",ylim=c(0,5))
par(new=T)
p5<-plot(x=CO_databind$date,y=CO_databind$Med,type = "l",col="#ffd633",
         main = "全年CO趨勢", xlab ="date", ylab = "CO",ylim=c(0,5))

p1<-plot(x=CO_databind$date,y=CO_databind$`#people`,type = "l",col="#333333",
         main = "就診人數", xlab ="date", ylab = "人數",ylim=c(0,40))
##########################################
par(mfrow = c(2, 2))
plot(x=CO_databind$Max,y=CO_databind$`#people`,type = "p",col="#1a53ff",pch=20,
     main = "CO_Max V.S. 就診人數", ylab ="就診人數", xlab = "CO")
abline(lm(`#people`~Max, CO_databind),                          
       lwd=1,col="red")
plot(x=CO_databind$Min,y=CO_databind$`#people`,type = "p",col="#1a53ff",pch=20,
     main = "CO_Min V.S. 就診人數", ylab ="就診人數", xlab = "CO")
abline(lm(`#people`~Min, CO_databind),                          
       lwd=1,col="red")
plot(x=CO_databind$Med,y=CO_databind$`#people`,type = "p",col="#1a53ff",pch=20,
     main = "CO_Med V.S. 就診人數", ylab ="就診人數", xlab = "CO")
abline(lm(`#people`~Med, CO_databind),                          
       lwd=1,col="red")
plot(x=CO_databind$Mean,y=CO_databind$`#people`,type = "p",col="#1a53ff",pch=20,
     main = "CO_Mean V.S. 就診人數", ylab ="就診人數", xlab = "CO")
abline(lm(`#people`~Mean, CO_databind),                          
       lwd=1,col="red")
#############################################################

#O3
par(mfrow = c(1, 2))
p2<-plot(x=O3_databind$date,O3_databind$Max,type = "l",col="#ff6666",
         main = "全年O3趨勢", xlab ="date", ylab = "O3",ylim=c(0,150))
legend("top", cex=0.5,                             
       pch = "l",                                
       col = c("#ff6666","#99ff66","#6699ff","#ffd633"), 
       legend = c("max", "min", "mean","med")
)
abline(125,0,                          
       lwd=1,col="#ff8c1a")
par(new=T)
p3<-plot(x=O3_databind$date,y=O3_databind$Min,type = "l",col="#99ff66",
         main = "全年O3趨勢", xlab ="date", ylab = "O3",ylim=c(0,150))
par(new=T)
p4<-plot(x=O3_databind$date,y=O3_databind$Mean,type = "l",col="#6699ff",
         main = "全年O3趨勢", xlab ="date", ylab = "O3",ylim=c(0,150))
par(new=T)
p5<-plot(x=O3_databind$date,y=O3_databind$Med,type = "l",col="#ffd633",
         main = "全年O3趨勢", xlab ="date", ylab = "O3",ylim=c(0,150))

p1<-plot(x=O3_databind$date,y=O3_databind$`#people`,type = "l",col="#333333",
         main = "就診人數", xlab ="date", ylab = "人數",ylim=c(0,40))
##########################################
par(mfrow = c(2, 2))
plot(x=O3_databind$Max,y=O3_databind$`#people`,type = "p",col="#1a53ff",pch=20,
     main = "O3_Max V.S. 就診人數", ylab ="就診人數", xlab = "O3")
abline(lm(`#people`~Max, O3_databind),                          
       lwd=1,col="red")
plot(x=O3_databind$Min,y=O3_databind$`#people`,type = "p",col="#1a53ff",pch=20,
     main = "O3_Min V.S. 就診人數", ylab ="就診人數", xlab = "O3")
abline(lm(`#people`~Min, O3_databind),                          
       lwd=1,col="red")
plot(x=O3_databind$Med,y=O3_databind$`#people`,type = "p",col="#1a53ff",pch=20,
     main = "O3_Med V.S. 就診人數", ylab ="就診人數", xlab = "O3")
abline(lm(`#people`~Med, O3_databind),                          
       lwd=1,col="red")
plot(x=O3_databind$Mean,y=O3_databind$`#people`,type = "p",col="#1a53ff",pch=20,
     main = "O3_Mean V.S. 就診人數", ylab ="就診人數", xlab = "O3")
abline(lm(`#people`~Mean, O3_databind),                          
       lwd=1,col="red")
#############################################################
#PM2.5
par(mfrow = c(1, 2))
p2<-plot(x=PM2.5_databind$date,PM2.5_databind$Max,type = "l",col="#ff6666",
         main = "全年PM2.5趨勢", xlab ="date", ylab = "PM2.5",ylim=c(0,100))
legend("top", cex=0.5,                             
       pch = "l",                                
       col = c("#ff6666","#99ff66","#6699ff","#ffd633"), 
       legend = c("max", "min", "mean","med")
)
abline(35.5,0,                          
       lwd=1,col="#ff8c1a")
abline(54.5,0,                          
       lwd=1,col="red")
par(new=T)
p3<-plot(x=PM2.5_databind$date,y=PM2.5_databind$Min,type = "l",col="#99ff66",
         main = "全年PM2.5趨勢", xlab ="date", ylab = "PM2.5",ylim=c(0,100))
par(new=T)
p4<-plot(x=PM2.5_databind$date,y=PM2.5_databind$Mean,type = "l",col="#6699ff",
         main = "全年PM2.5趨勢", xlab ="date", ylab = "PM2.5",ylim=c(0,100))
par(new=T)
p5<-plot(x=PM2.5_databind$date,y=PM2.5_databind$Med,type = "l",col="#ffd633",
         main = "全年PM2.5趨勢", xlab ="date", ylab = "PM2.5",ylim=c(0,100))

p1<-plot(x=PM2.5_databind$date,y=PM2.5_databind$`#people`,type = "l",col="#333333",
         main = "就診人數", xlab ="date", ylab = "人數",ylim=c(0,40))
##########################################
par(mfrow = c(2, 2))
plot(x=PM2.5_databind$Max,y=PM2.5_databind$`#people`,type = "p",col="#1a53ff",pch=20,
     main = "PM2.5_Max V.S. 就診人數", ylab ="就診人數", xlab = "PM2.5")
abline(lm(`#people`~Max, PM2.5_databind),                          
       lwd=1,col="red")
plot(x=PM2.5_databind$Min,y=PM2.5_databind$`#people`,type = "p",col="#1a53ff",pch=20,
     main = "PM2.5_Min V.S. 就診人數", ylab ="就診人數", xlab = "PM2.5")
abline(lm(`#people`~Min, PM2.5_databind),                          
       lwd=1,col="red")
plot(x=PM2.5_databind$Med,y=PM2.5_databind$`#people`,type = "p",col="#1a53ff",pch=20,
     main = "PM2.5_Med V.S. 就診人數", ylab ="就診人數", xlab = "PM2.5")
abline(lm(`#people`~Med, PM2.5_databind),                          
       lwd=1,col="red")
plot(x=PM2.5_databind$Mean,y=PM2.5_databind$`#people`,type = "p",col="#1a53ff",pch=20,
     main = "PM2.5_Mean V.S. 就診人數", ylab ="就診人數", xlab = "PM2.5")
abline(lm(`#people`~Mean, PM2.5_databind),                          
       lwd=1,col="red")
#############################################################

#NO2
par(mfrow = c(1, 2))
p2<-plot(x=NO2_databind$date,NO2_databind$Max,type = "l",col="#ff6666",
         main = "全年NO2趨勢", xlab ="date", ylab = "NO2",ylim=c(0,70))
legend("top", cex=0.5,                             
       pch = "l",                                
       col = c("#ff6666","#99ff66","#6699ff","#ffd633"), 
       legend = c("max", "min", "mean","med")
)
abline(54,0,                          
       lwd=1,col="#ff8c1a")
par(new=T)
p3<-plot(x=NO2_databind$date,y=NO2_databind$Min,type = "l",col="#99ff66",
         main = "全年NO2趨勢", xlab ="date", ylab = "NO2",ylim=c(0,70))
par(new=T)
p4<-plot(x=NO2_databind$date,y=NO2_databind$Mean,type = "l",col="#6699ff",
         main = "全年NO2趨勢", xlab ="date", ylab = "NO2",ylim=c(0,70))
par(new=T)
p5<-plot(x=NO2_databind$date,y=NO2_databind$Med,type = "l",col="#ffd633",
         main = "全年NO2趨勢", xlab ="date", ylab = "NO2",ylim=c(0,70))

p1<-plot(x=NO2_databind$date,y=NO2_databind$`#people`,type = "l",col="#333333",
         main = "就診人數", xlab ="date", ylab = "人數",ylim=c(0,40))
##########################################
par(mfrow = c(2, 2))
plot(x=NO2_databind$Max,y=NO2_databind$`#people`,type = "p",col="#1a53ff",pch=20,
     main = "NO2_Max V.S. 就診人數", ylab ="就診人數", xlab = "NO2")
abline(lm(`#people`~Max, NO2_databind),                          
       lwd=1,col="red")
plot(x=NO2_databind$Min,y=NO2_databind$`#people`,type = "p",col="#1a53ff",pch=20,
     main = "NO2_Min V.S. 就診人數", ylab ="就診人數", xlab = "NO2")
abline(lm(`#people`~Min, NO2_databind),                          
       lwd=1,col="red")
plot(x=NO2_databind$Med,y=NO2_databind$`#people`,type = "p",col="#1a53ff",pch=20,
     main = "NO2_Med V.S. 就診人數", ylab ="就診人數", xlab = "NO2")
abline(lm(`#people`~Med, NO2_databind),                          
       lwd=1,col="red")
plot(x=NO2_databind$Mean,y=NO2_databind$`#people`,type = "p",col="#1a53ff",pch=20,
     main = "NO2_Mean V.S. 就診人數", ylab ="就診人數", xlab = "NO2")
abline(lm(`#people`~Mean, NO2_databind),                          
       lwd=1,col="red")
#######################################################
#PM10
par(mfrow = c(1, 2))
p2<-plot(x=PM10_databind$date,PM10_databind$Max,type = "l",col="#ff6666",
         main = "全年PM10趨勢", xlab ="date", ylab = "PM10",ylim=c(0,300))
legend("top", cex=0.5,                             
       pch = "l",                                
       col = c("#ff6666","#99ff66","#6699ff","#ffd633"), 
       legend = c("max", "min", "mean","med")
)
abline(255,0,                          
       lwd=1,col="#800000")
abline(125,0,                          
       lwd=1,col="red")
abline(55,0,                          
       lwd=1,col="#ff8c1a")
par(new=T)
p3<-plot(x=PM10_databind$date,y=PM10_databind$Min,type = "l",col="#99ff66",
         main = "全年PM10趨勢", xlab ="date", ylab = "PM10",ylim=c(0,300))
par(new=T)
p4<-plot(x=PM10_databind$date,y=PM10_databind$Mean,type = "l",col="#6699ff",
         main = "全年PM10趨勢", xlab ="date", ylab = "PM10",ylim=c(0,300))
par(new=T)
p5<-plot(x=PM10_databind$date,y=PM10_databind$Med,type = "l",col="#ffd633",
         main = "全年PM10趨勢", xlab ="date", ylab = "PM10",ylim=c(0,300))

p1<-plot(x=PM10_databind$date,y=PM10_databind$`#people`,type = "l",col="#333333",
         main = "就診人數", xlab ="date", ylab = "人數",ylim=c(0,40))
##########################################
par(mfrow = c(2, 2))
plot(x=PM10_databind$Max,y=PM10_databind$`#people`,type = "p",col="#1a53ff",pch=20,
     main = "PM10_Max V.S. 就診人數", ylab ="就診人數", xlab = "PM10")
abline(lm(`#people`~Max, PM10_databind),                          
       lwd=1,col="red")
plot(x=PM10_databind$Min,y=PM10_databind$`#people`,type = "p",col="#1a53ff",pch=20,
     main = "PM10_Min V.S. 就診人數", ylab ="就診人數", xlab = "PM10")
abline(lm(`#people`~Min, PM10_databind),                          
       lwd=1,col="red")
plot(x=PM10_databind$Med,y=PM10_databind$`#people`,type = "p",col="#1a53ff",pch=20,
     main = "PM10_Med V.S. 就診人數", ylab ="就診人數", xlab = "PM10")
abline(lm(`#people`~Med, PM10_databind),                          
       lwd=1,col="red")
plot(x=PM10_databind$Mean,y=PM10_databind$`#people`,type = "p",col="#1a53ff",pch=20,
     main = "PM10_Mean V.S. 就診人數", ylab ="就診人數", xlab = "PM10")
abline(lm(`#people`~Mean, PM10_databind),                          
       lwd=1,col="red")
#################################################
#NO
##########################################
par(mfrow = c(1, 2))
p2<-plot(x=NO_databind$date,NO_databind$Max,type = "l",col="#ff6666",
         main = "全年NO趨勢", xlab ="date", ylab = "NO",ylim=c(0,70))
legend("topright", cex=0.5,                             
       pch = "l",                                
       col = c("#ff6666","#99ff66","#6699ff","#ffd633"), 
       legend = c("max", "min", "mean","med")
)
abline(55,0,                          
       lwd=1,col="#ff8c1a")
par(new=T)
p3<-plot(x=NO_databind$date,y=NO_databind$Min,type = "l",col="#99ff66",
         main = "全年NO趨勢", xlab ="date", ylab = "NO",ylim=c(0,70))
par(new=T)
p4<-plot(x=NO_databind$date,y=NO_databind$Mean,type = "l",col="#6699ff",
         main = "全年NO趨勢", xlab ="date", ylab = "NO",ylim=c(0,70))
par(new=T)
p5<-plot(x=NO_databind$date,y=NO_databind$Med,type = "l",col="#ffd633",
         main = "全年NO趨勢", xlab ="date", ylab = "NO",ylim=c(0,70))

p1<-plot(x=NO_databind$date,y=NO_databind$`#people`,type = "l",col="#333333",
         main = "就診人數", xlab ="date", ylab = "人數",ylim=c(0,40))
##########################################
par(mfrow = c(2, 2))
plot(x=NO_databind$Max,y=NO_databind$`#people`,type = "p",col="#1a53ff",pch=20,
     main = "NO_Max V.S. 就診人數", ylab ="就診人數", xlab = "NO")
abline(lm(`#people`~Max, NO_databind),                          
       lwd=1,col="red")
plot(x=NO_databind$Min,y=NO_databind$`#people`,type = "p",col="#1a53ff",pch=20,
     main = "NO_Min V.S. 就診人數", ylab ="就診人數", xlab = "NO")
abline(lm(`#people`~Min, NO_databind),                          
       lwd=1,col="red")
plot(x=NO_databind$Med,y=NO_databind$`#people`,type = "p",col="#1a53ff",pch=20,
     main = "NO_Med V.S. 就診人數", ylab ="就診人數", xlab = "NO")
abline(lm(`#people`~Med, NO_databind),                          
       lwd=1,col="red")
plot(x=NO_databind$Mean,y=NO_databind$`#people`,type = "p",col="#1a53ff",pch=20,
     main = "NO_Mean V.S. 就診人數", ylab ="就診人數", xlab = "NO")
abline(lm(`#people`~Mean, NO_databind),                          
       lwd=1,col="red")
