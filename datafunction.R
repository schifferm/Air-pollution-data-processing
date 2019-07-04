
library(readxl)
library(dplyr)
library(xlsx)
library(lubridate)
setwd("E:/碩二上/空汙/106年 高屏空品區")
path<-("E:/碩二上/空汙/106年 高屏空品區")

##############################load.data#####################################
#資料載入環境中，以資料夾的逐一檢索載入
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
#EXAMPLE
ptm <- proc.time()
SO2_data<-loaddata(path="E:/碩二上/空汙/106年 高屏空品區",confound=c("SO2"))
proc.time() - ptm
which(is.na(SO2_data))
#################clean.#*x##################
#將特殊記號刪除，可在pattern中更改
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
#EXAMPLE
ptm <- proc.time()
SO2_data_cleaned<-dataclean(SO2_data)
proc.time() - ptm

####trans NaN to NA##########

SO2_data_cleaned[which(SO2_data_cleaned =="NaN")] <- NA_character_

###################trans Negative to 0#########################
#將負值轉為0或其他數值
clean_data <- function(df, impute_value){
  n_rows <- nrow(df)
  na_sum <- rep(NA, times = n_rows)
  for (i in 1:n_rows){
    na_sum[i] <- sum(ifelse(df[i,]<0,TRUE,FALSE))
    df[i, ][ifelse(df[i,]<0,TRUE,FALSE)] <- impute_value 
  }
  complete_cases <- df[as.logical(!na_sum), ] 
  imputed_data <- df
  df_df<-data.frame(imputed_data)
  return(df_df)
}
#EXAMPLE
SO2_clean<-clean_data(SO2_data_cleaned,0)


################mean#########
#20190628update function
#平均數計算
cal_mean<-function(data){
  tmp<-split(data,data[,1])
  res<-data.frame(matrix(data=NA,nrow = 365,ncol = 15))
  colnames(res)<-as.character(unlist(tmp[[1]][2]))
  for(i in 1:length(tmp)){
    for(j in 1:15){
      k<-colnames(res)[j]
      res[i,k]<-mean(as.numeric(tmp[[i]][which(tmp[[i]]$X2==k),4:27]),na.rm=TRUE)#更新
    }
  }
  return(res)
}
#EXAMPLE
SO2_alldata_mean<-cal_mean(SO2_clean)


##creat.date#####
#新增全年日期
date_2017 = data.frame(date=seq(from = as.Date("2017-01-01"),to = as.Date("2017-12-31"),by = "day"))
date_2017_wday<-data.frame(date=date_2017$date,wday=wday(date_2017$date,week_start = getOption("lubridate.week.start", 1)))
#################
#載入病人資料到環境中
patient_106<-data.frame(read_xlsx(path ="E:/碩二上/空汙/就診資料/2017-4_20yo_只高雄縣市.xlsx",sheet=1,col_names = TRUE,col_types=c("date","numeric","numeric","numeric")))
names(patient_106)<-c("date","urticaria","allergy","both")
patient_106[,1]<-as.Date(patient_106[,1])
patient_2017_wday<-merge(date_2017_wday, patient_106[,c(1:3)], by.x="date",all.x = TRUE)

############databind###
#資料合併，這個函數將日期與空污資料結合
databindALL<-function(tmp,patient_year){
  
  mean<-data.frame(apply(tmp,1,function(x){mean(x,na.rm=TRUE)}))
  obs<-cbind(patient_year$date,patient_year$wday,
             mean)
  colnames(obs)<-c("date","wday","Mean")
  return(obs)
}
#EXAMPLE
SO2_106<-databindALL(SO2_alldata_mean[,c(-8,-9,-14)],patient_2017_wday)

#依上述步驟整理出個空污資料後進行合併
#data_2017<-data.frame(patient_2017_wday,
#                      yday=c(1:dim(patient_2017_wday)[1]),
#                      SO2=SO2_106$Mean,
#                      CO=CO_106$Mean,
#                      O3=O3_106$Mean,
#                      PM2.5=PM2.5_106$Mean,
#                      PM10=PM10_106$Mean,
#                      NO=NO_106$Mean,
#                      NO2=NO2_106$Mean,
#                      TEMP=TEMP_106$Mean,
#                      RH=RH_106$Mean,
#                      RAIN=RAIN_106$Mean,
#                      WS_HR=WS_HR_106$Mean)
#4years data
#data_4years<-rbind(data_2014,data_2015,data_2016,data_2017,data_2018)


####################################gam&glm################
#use lag0_daily 
#glm p-palue & estimate & CI
#批量單污染模型
lag_glm<-function(patientcol,aircol,data,covcol){
  result<-NULL
  res<-NULL
  p<-names(data)[patientcol]
  air<-names(data)[aircol]
  z1<-data.frame()
  zz1<-data.frame()
  formula0<-list()
  for (j in 1:length(air)){
    formula0[[j]]<-paste(p,"~",air[[j]],'+as.factor(wday)+as.numeric(TEMP)+as.numeric(RH)+as.numeric(yday)+as.numeric(WS_HR)+as.numeric(RAIN)',sep="")
  }
  for(k in 1:length(formula0)){  
    res[[k]]<-gam(formula(formula0[[k]]),data = data,family = "poisson",method="REML",select=TRUE)
    result<-summary(res[[k]])
    z1[k,1]<-round(result$p.pv[2],5)
    z1[k,2]<-round(result$p.coeff[2],5)
    z1[k,3]<-round(result$p.pv[3],5)
    z1[k,4]<-round(result$p.pv[4],5)
    z1[k,5]<-round(result$p.pv[5],5)
    z1[k,6]<-round(result$p.pv[6],5)
    z1[k,7]<-round(result$p.pv[7],5)
    beta<-coef(res[[k]])
    Vb <- vcov(res[[k]], unconditional = TRUE)
    se <- sqrt(diag(Vb))
    i <- names(beta)[2]
    zz1[k,1]<-round(exp(res[[k]]$coefficients[[2]]),4)
    zz1[k,2]<-round(exp(beta[i] + (c(-1,1) * (2 * se[i])))[1],4)
    zz1[k,3]<-round(exp(beta[i] + (c(-1,1) * (2 * se[i])))[2],4)
  }
  colnames(z1)<-c("air_p.pv","air_coeff","TEMP","RH","day","WS_HR","RAIN")
  row.names(z1)<-names(data)[aircol]
  row.names(zz1)<-names(data)[aircol]
  return(list(z1,zz1))
}
#批量單污染模型
#gam
lag_gam<-function(patientcol,aircol,data,covcol){
  result<-NULL
  res<-NULL
  p<-names(data)[patientcol]
  air<-names(data)[aircol]
  z1<-data.frame()
  z2<-data.frame()
  zz1<-data.frame()
  formula0<-list()
  for (j in 1:length(air)){
    formula0[[j]]<-paste(p,"~",air[[j]],"+as.factor(wday)",'+s(as.numeric(TEMP),bs="cs",sp=3,k=4)+s(as.numeric(RH),bs="cs",sp=4,k=4)+s(as.numeric(yday),bs="cs",sp=4,k=4)+s(as.numeric(WS_HR),bs="cs",sp=4,k=4)+s(as.numeric(RAIN),bs="cs",sp=4,k=4)',sep="")
  }
  for(k in 1:length(formula0)){  
    res[[k]]<-gam(formula(formula0[[k]]),data = data,family = ziP(),method="REML",select=TRUE)
    result<-summary(res[[k]])
    z1[k,1]<-round(result$p.pv[2],5)
    z1[k,2]<-round(result$p.coeff[2],5)
    z1[k,3]<-round(result$s.pv[1],5)
    z1[k,4]<-round(result$s.pv[2],5)
    z1[k,5]<-round(result$s.pv[3],5)
    z1[k,6]<-round(result$s.pv[4],5)
    z1[k,7]<-round(result$s.pv[5],5)
    z2[k,1]<-round(result$p.pv[1],5)
    z2[k,2]<-round(result$p.coeff[1],5)
    z2[k,3]<-round(result$p.pv[3],5)
    z2[k,4]<-round(result$p.coeff[3],5)
    z2[k,5]<-round(result$p.pv[4],5)
    z2[k,6]<-round(result$p.coeff[4],5)
    z2[k,7]<-round(result$p.pv[5],5)
    z2[k,8]<-round(result$p.coeff[5],5)
    z2[k,9]<-round(result$p.pv[6],5)
    z2[k,10]<-round(result$p.coeff[6],5)
    beta<-coef(res[[k]])
    Vb <- vcov(res[[k]], unconditional = TRUE)
    se <- sqrt(diag(Vb))
    i <- names(beta)[2]
    zz1[k,1]<-round(exp(res[[k]]$coefficients[[2]]),4)
    zz1[k,2]<-round(exp(beta[i] + (c(-1,1) * (2 * se[i])))[1],4)
    zz1[k,3]<-round(exp(beta[i] + (c(-1,1) * (2 * se[i])))[2],4)
  }
  
  colnames(z1)<-c("air_p.pv","air_coeff","TEMP","RH","day","WS_HR","RAIN")
  row.names(z1)<-names(data)[aircol]
  colnames(z2)<-c("int_p.pv","int_coeff","2_p.pv","2_coeff","3_p.pv","3_coeff","4_p.pv","4_coeff","5_p.pv","5_coeff")
  row.names(z1)<-names(data)[aircol]
  row.names(zz1)<-names(data)[aircol]
  return(list(z1,z2,zz1))
}

#example for urticaria with all patients

b<-list()
b[[1]]<-lag_gam(patientcol = 3,aircol = c(6:12),data = lag0_daily[which(lag0_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
b[[2]]<-lag_gam(patientcol = 3,aircol = c(6:12),data = lag1_daily[which(lag1_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
b[[3]]<-lag_gam(patientcol = 3,aircol = c(6:12),data = lag2_daily[which(lag2_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
b[[4]]<-lag_gam(patientcol = 3,aircol = c(6:12),data = lag3_daily[which(lag3_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
b[[5]]<-lag_gam(patientcol = 3,aircol = c(6:12),data = lag4_daily[which(lag4_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
b[[6]]<-lag_gam(patientcol = 3,aircol = c(6:12),data = lag5_daily[which(lag5_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
b[[7]]<-lag_gam(patientcol = 3,aircol = c(6:12),data = lag6_daily[which(lag6_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
b[[8]]<-lag_gam(patientcol = 3,aircol = c(6:12),data = lag7_daily[which(lag7_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))

#以下為重新排序資料用
bb<-data.frame(b[[1]][1],b[[2]][1],b[[3]][1],b[[4]][1],
               b[[5]][1],b[[6]][1],b[[7]][1],b[[8]][1])
bbb<-data.frame(b[[1]][2],b[[2]][2],b[[3]][2],b[[4]][2],
                b[[5]][2],b[[6]][2],b[[7]][2],b[[8]][2])
res_urticaria<-list()
for(i in 1:7){
  res_urticaria[[i]]<-matrix(data = bb[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("air_p.pv","air_coeff","TEMP","RH","day","WS_HR","RAIN")))
  
}
res_urticaria_week<-list()
for(i in 1:7){
  res_urticaria_week[[i]]<-matrix(data = bbb[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("int_p.pv","int_coeff","2_p.pv","2_coeff","3_p.pv","3_coeff","4_p.pv","4_coeff","5_p.pv","5_coeff")))
  
}

bbbb<-data.frame(b[[1]][3],b[[2]][3],b[[3]][3],b[[4]][3],
                 b[[5]][3],b[[6]][3],b[[7]][3],b[[8]][3])
res_urticaria_rr<-list()
for(i in 1:7){
  res_urticaria_rr[[i]]<-matrix(data = bbbb[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep=""))))
  
}
names(res_urticaria_rr)<-row.names(bb)
res_urticaria_rr

names(res_urticaria)<-rownames(bb)
names(res_urticaria_week)<-rownames(bb)
res_urticaria
res_urticaria_week
########
#weight
library(mgcv)
wb<-list()
wb[[1]]<-lag_gam(patientcol = 15,aircol = c(2:8),data = final_allergy,covcol = c(9:12))
wb[[2]]<-lag_gam(patientcol = 3,aircol = c(6:12),data = lag1_daily[which(lag1_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
wb[[3]]<-lag_gam(patientcol = 3,aircol = c(6:12),data = lag2_daily[which(lag2_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
wb[[4]]<-lag_gam(patientcol = 3,aircol = c(6:12),data = lag3_daily[which(lag3_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
wb[[5]]<-lag_gam(patientcol = 3,aircol = c(6:12),data = lag4_daily[which(lag4_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
wb[[6]]<-lag_gam(patientcol = 3,aircol = c(6:12),data = lag5_daily[which(lag5_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
wb[[7]]<-lag_gam(patientcol = 3,aircol = c(6:12),data = lag6_daily[which(lag6_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))
wb[[8]]<-lag_gam(patientcol = 3,aircol = c(6:12),data = lag7_daily[which(lag7_daily$date<as.Date("2018-01-01")),],covcol = c(13:16))

#以下為重新排序資料用
bb<-data.frame(b[[1]][1],b[[2]][1],b[[3]][1],b[[4]][1],
               b[[5]][1],b[[6]][1],b[[7]][1],b[[8]][1])
bbb<-data.frame(b[[1]][2],b[[2]][2],b[[3]][2],b[[4]][2],
                b[[5]][2],b[[6]][2],b[[7]][2],b[[8]][2])
res_urticaria<-list()
for(i in 1:7){
  res_urticaria[[i]]<-matrix(data = bb[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("air_p.pv","air_coeff","TEMP","RH","day","WS_HR","RAIN")))
  
}
res_urticaria_week<-list()
for(i in 1:7){
  res_urticaria_week[[i]]<-matrix(data = bbb[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep="")),c("int_p.pv","int_coeff","2_p.pv","2_coeff","3_p.pv","3_coeff","4_p.pv","4_coeff","5_p.pv","5_coeff")))
  
}

bbbb<-data.frame(b[[1]][3],b[[2]][3],b[[3]][3],b[[4]][3],
                 b[[5]][3],b[[6]][3],b[[7]][3],b[[8]][3])
res_urticaria_rr<-list()
for(i in 1:7){
  res_urticaria_rr[[i]]<-matrix(data = bbbb[i,],nrow =8 ,byrow = TRUE,dimnames = list(c(paste("lag",0:7,sep=""))))
  
}
names(res_urticaria_rr)<-row.names(bb)
res_urticaria_rr

names(res_urticaria)<-rownames(bb)
names(res_urticaria_week)<-rownames(bb)
res_urticaria
res_urticaria_week