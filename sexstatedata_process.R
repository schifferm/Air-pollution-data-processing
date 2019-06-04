library(geosphere)
library(ggmap)
library(httr)
library(rjson)
geocode("802高雄市苓雅區四維三路2號")
URLencode("802高雄市苓雅區四維三路2號")
getLatLng <- function(address){TY615RF
  
  urlData <- GET(paste0("https://maps.googleapis.com/maps/api/geocode/json?language=zh-TW&address=", URLencode(address)))
  jsonResult <- rjson::fromJSON(rawToChar(urlData$content))
  Sys.sleep(1)
  if(jsonResult$status != "OK"){
    print("Google geocode API Error")
    return("error")
  }
  print("LatLng Got")
  lat <<- jsonResult$results[[1]]$geometry$location$lat
  lng <<- jsonResult$results[[1]]$geometry$location$lng
  
  return(paste(lat, lng, sep=","))
}

getLatLng("802高雄市苓雅區四維三路2號")

distm(c( 120.305954,22.759909), c(  120.320138,22.622060), fun = distHaversine)
###########################################
library(readxl)
library(lubridate)
library(dplyr)
library(rio)
# original  patient data

patient<-data.frame(read.csv("E:/碩二上/空汙/就診資料/20170101-20171231每日人數.csv")[,c(1:4)])
patient_103_sex<-data.frame(read_xlsx(path ="E:/碩二上/空汙/就診資料/sex/2014_1-12月高雄市20歲以上分男女生年.xlsx",sheet="分男女",col_names = TRUE,col_types=c("guess","numeric","numeric","numeric")))[-dim(patient_103_sex)[1],]
patient_104_sex<-data.frame(read_xlsx(path ="E:/碩二上/空汙/就診資料/sex/2015_1-12月_高雄市且20歲以上分男女生年20190505.xlsx",sheet="分男女",col_names = TRUE,col_types=c("guess","numeric","numeric","numeric")))[-dim(patient_104_sex)[1],]
patient_105_sex<-data.frame(read_xlsx(path ="E:/碩二上/空汙/就診資料/sex/2016_1-12月_高雄市且20歲以上分男女生年20190505.xlsx",sheet="分男女",col_names = TRUE,col_types=c("guess","numeric","numeric","numeric")))[-dim(patient_105_sex)[1],]
patient_106_sex<-data.frame(read_xlsx(path ="E:/碩二上/空汙/就診資料/sex/2017_1_12_高雄市20歲以上分男女生年區20190508 (1).xlsx",sheet="分男女",col_names = TRUE,col_types=c("guess","numeric","numeric","numeric")))[-dim(patient_106_sex)[1],]
patient_107_7_sex<-data.frame(read_xlsx(path ="E:/碩二上/空汙/就診資料/sex/2018_1-7月_高雄市20歲以上分男女生年20190505.xlsx",sheet="分男女",col_names = TRUE,col_types=c("guess","numeric","numeric","numeric")))[dim(patient_107_7_sex)[1],]
names(patient_103_sex)<-c("date","urticaria","allergy","both")
names(patient_104_sex)<-c("date","urticaria","allergy","both")
names(patient_105_sex)<-c("date","urticaria","allergy","both")
names(patient_106_sex)<-c("date","urticaria","allergy","both")
names(patient_107_7_sex)<-c("date","urticaria","allergy","both")
as_date(as.numeric(patient_105_sex[1,1]),origin = "1899-12-30 UTC")
t<-patient_106_sex
for(i in 1:dim(t)[1]){
  long<-strsplit(t[i,1],split = c("月|日"))
  if(length(long[[1]])>1)
    t[i,1]<-paste("2017-",strsplit(t[i,1],split = c("月|日"))[[1]][1],"-",strsplit(t[i,1],split = c("月|日"))[[1]][2],sep="")
}
tt<-data.frame()
for(i in 1:dim(t)[1]){
  if(!is.na(as_date(t[i,1]))){
  tt[i,1]<-as.numeric(as_date(t[i,1])-as.Date("1899-12-30"))
  }else{
    tt[i,1]<-t[i,1]
  }
}  

patient_106_sex[,1]<-tt
#arrange sex data
patsex<-function(data){
          t<-data
          new_t<-data.frame(date=as.Date(rep(NA,366)),
                            urticaria_M=rep(NA,366),
                            urticaria_F=rep(NA,366),
                            allergy_M=rep(NA,366),
                            allergy_F=rep(NA,366))
          j<-0
          for (i in 1:dim(t)[1]){
            if(!is.na(as.numeric(t[i,1]))){
              j<-j+1      
              new_t[j,1]<-lubridate::as_date(as.numeric(t[i,1]),origin = "1899-12-30 UTC")
            }else{
              switch(t[i,1],
                     "M"={new_t[j,2]<-t[i,2];new_t[j,4]<-t[i,3]},
                     "F"={new_t[j,3]<-t[i,2];new_t[j,5]<-t[i,3]})
            }
          }
          return(new_t)
        }


p103sex<-patsex(patient_103_sex)
p104sex<-patsex(patient_104_sex)
p105sex<-patsex(patient_105_sex)
p106sex<-patsex(patient_106_sex)
p107_7sex<-patsex(patient_107_7_sex)

#
#tras date data to R format
lubridate::as_date(as.numeric(patient_103_sex[1,1]),origin =  "1900-01-01 UTC")
p103sex[,1]<-as.Date(p103sex[,1])
p104sex[,1]<-as.Date(p104sex[,1])
p105sex[,1]<-as.Date(p105sex[,1])
p106sex[,1]<-as.Date(p106sex[,1])
p107_7sex[,1]<-as.Date(p107_7sex[,1])
p103sex[,2:5]<-clean_NAdata(p103sex[,2:5],0)
p104sex[,2:5]<-clean_NAdata(p104sex[,2:5],0)
p105sex[,2:5]<-clean_NAdata(p105sex[,2:5],0)
p106sex[,2:5]<-clean_NAdata(p106sex[,2:5],0)
p107_7sex[,2:5]<-clean_NAdata(p107_7sex[,2:5],0)

#create all date

date_2014 = data.frame(date=seq(from = as.Date("2014-01-01"),to = as.Date("2014-12-31"),by = "day"))
date_2015 = data.frame(date=seq(from = as.Date("2015-01-01"),to = as.Date("2015-12-31"),by = "day"))
date_2016 = data.frame(date=seq(from = as.Date("2016-01-01"),to = as.Date("2016-12-31"),by = "day"))
date_2017 = data.frame(date=seq(from = as.Date("2017-01-01"),to = as.Date("2017-12-31"),by = "day"))
date_2018 = data.frame(date=seq(from = as.Date("2018-01-01"),to = as.Date("2018-7-31"),by = "day"))
#

library(lubridate)
date_2014_wday<-data.frame(date=date_2014$date,wday=wday(date_2014$date,week_start = getOption("lubridate.week.start", 1)))
date_2015_wday<-data.frame(date=date_2015$date,wday=wday(date_2015$date,week_start = getOption("lubridate.week.start", 1)))
date_2016_wday<-data.frame(date=date_2016$date,wday=wday(date_2016$date,week_start = getOption("lubridate.week.start", 1)))
date_2017_wday<-data.frame(date=date_2017$date,wday=wday(date_2017$date,week_start = getOption("lubridate.week.start", 1)))
date_2018_wday<-data.frame(date=date_2018$date,wday=wday(date_2018$date,week_start = getOption("lubridate.week.start", 1)))
####################################################

#cbind date and patient data
p103sex_w<-merge(date_2014_wday, p103sex[,c(1:5)], by.x="date",all.x = TRUE)
p104sex_w<-merge(date_2015_wday, p104sex[,c(1:5)], by.x="date",all.x = TRUE)
p105sex_w<-merge(date_2016_wday, p105sex[,c(1:5)], by.x="date",all.x = TRUE)
p106sex_w<-merge(date_2017_wday, p106sex[,c(1:5)], by.x="date",all.x = TRUE)
p107_7sex_w<-merge(date_2018_wday, p107_7sex[,c(1:5)], by.x="date",all.x = TRUE)
##########################################################
p103sex_w[,c(2:6)]<-clean_NAdata(p103sex_w[,c(2:6)],0)
p104sex_w[,c(2:6)]<-clean_NAdata(p104sex_w[,c(2:6)],0)
p105sex_w[,c(2:6)]<-clean_NAdata(p105sex_w[,c(2:6)],0)
p106sex_w[,c(2:6)]<-clean_NAdata(p106sex_w[,c(2:6)],0)
p107_7sex_w[,c(2:6)]<-clean_NAdata(p107_7sex_w[,c(2:6)],0)
########
psex4year<-rbind(p103sex_w[which(p103sex_w$wday!=6 &p103sex_w$wday!=7),],
                 p104sex_w[which(p104sex_w$wday!=6 &p104sex_w$wday!=7),],
                 p105sex_w[which(p105sex_w$wday!=6 &p105sex_w$wday!=7),],
                 p106sex_w[which(p106sex_w$wday!=6 &p106sex_w$wday!=7),])

lag0_daily[,17:20]<-psex4year[,3:6]
lag1_daily[,17:20]<-psex4year[-1,3:6]
lag2_daily[,17:20]<-psex4year[-2:-1,3:6]
lag3_daily[,17:20]<-psex4year[-3:-1,3:6]
lag4_daily[,17:20]<-psex4year[-3:-1,3:6]
lag5_daily[,17:20]<-psex4year[-3:-1,3:6]
lag6_daily[,17:20]<-psex4year[-4:-1,3:6]
lag7_daily[,17:20]<-psex4year[-5:-1,3:6]
######################################################
#age data
library(readxl)
library(lubridate)
library(dplyr)
clean_NAdata<-function(df, impute_value){
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

patient_103_age<-data.frame(read_xlsx(path ="E:/碩二上/空汙/就診資料/state/2014_1-12月高雄市20歲以上分男女生年區20190507.xlsx",sheet="分生年",col_names = TRUE,col_types=c("guess","numeric","numeric","numeric")))[-dim(patient_103_age)[1],]
patient_104_age<-data.frame(read_xlsx(path ="E:/碩二上/空汙/就診資料/state/2015_1-12月_高雄市且20歲以上分男女生年區20190507.xlsx",sheet="分生年",col_names = TRUE,col_types=c("guess","numeric","numeric","numeric")))[-dim(patient_104_age)[1],]
patient_105_age<-data.frame(read_xlsx(path ="E:/碩二上/空汙/就診資料/state/2016_1-12月_高雄市且20歲以上分男女生年區20190507.xlsx",sheet="分生年",col_names = TRUE,col_types=c("guess","numeric","numeric","numeric")))[-dim(patient_105_age)[1],]
patient_106_age<-data.frame(read_xlsx(path ="E:/碩二上/空汙/就診資料/state/2017_1_12_高雄市20歲以上分男女生年區20190508 (1).xlsx",sheet="分生年",col_names = TRUE,col_types=c("guess","numeric","numeric","numeric")))[-dim(patient_106_age)[1],]
patient_107_7_age<-data.frame(read_xlsx(path ="E:/碩二上/空汙/就診資料/state/2018_1-7月_高雄市20歲以上分男女生年區20190507.xlsx",sheet="分生年",col_names = TRUE,col_types=c("guess","numeric","numeric","numeric")))[-dim(patient_107_7_age)[1],]
names(patient_103_age)<-c("date","urticaria","allergy","both")
names(patient_104_age)<-c("date","urticaria","allergy","both")
names(patient_105_age)<-c("date","urticaria","allergy","both")
names(patient_106_age)<-c("date","urticaria","allergy","both")
names(patient_107_7_age)<-c("date","urticaria","allergy","both")
#change date format
as_date(as.numeric(patient_105_age[1,1]),origin = "1899-12-30 UTC")
t<-patient_106_age
for(i in 1:dim(t)[1]){
  long<-strsplit(t[i,1],split = c("月|日"))
  if(length(long[[1]])>1)
    t[i,1]<-paste("2017-",strsplit(t[i,1],split = c("月|日"))[[1]][1],"-",strsplit(t[i,1],split = c("月|日"))[[1]][2],sep="")
}
tt<-data.frame()
for(i in 1:dim(t)[1]){
  if(!is.na(as_date(t[i,1]))){
    tt[i,1]<-as.numeric(as_date(t[i,1])-as.Date("1899-12-30"))
  }else{
    tt[i,1]<-t[i,1]
  }
}  
#return date with numeric
patient_106_age[,1]<-tt
patient_103_age[,1]<-as.numeric(unlist(strsplit(patient_103_age[,1],split = c("年"))))
patient_104_age[,1]<-as.numeric(unlist(strsplit(patient_104_age[,1],split = c("年"))))
patient_105_age[,1]<-as.numeric(unlist(strsplit(patient_105_age[,1],split = c("年"))))
patient_106_age[,1]<-as.numeric(unlist(strsplit(patient_106_age[,1],split = c("年"))))
#clean NA
patient_103_age0<-clean_NAdata(patient_103_age,0)
patient_104_age0<-clean_NAdata(patient_104_age,0)
patient_105_age0<-clean_NAdata(patient_105_age,0)
patient_106_age0<-clean_NAdata(patient_106_age,0)

#
patage<-function(data){
    t<-data
    new_t<-data.frame(date=as.Date(rep(NA,366)),
                    urticaria_20=rep(0,366),
                    urticaria_65=rep(0,366),
                    allergy_20=rep(0,366),
                    allergy_65=rep(0,366))
    j<-0
    for (i in 1:(dim(t)[1]-1)){
    if(as.numeric(t[i,1])>40000){
      j<-j+1      
      new_t[j,1]<-lubridate::as_date(as.numeric(t[i,1]),origin = "1899-12-30 UTC")
    }else if(as.numeric(t[i,1])<40000&&as.numeric(t[i,1])>1954){
      new_t[j,2]<-t[i,2]+new_t[j,2]
      new_t[j,4]<-t[i,3]+new_t[j,4]
    }else if(as.numeric(t[i,1])<=1954&&as.numeric(t[i,1])>1900){
      new_t[j,3]<-t[i,2]+new_t[j,3]
      new_t[j,5]<-t[i,3]+new_t[j,5]
      }
  }
  return(new_t)
}

p103age<-patage(patient_103_age0)
p104age<-patage(patient_104_age0)
p105age<-patage(patient_105_age0)
p106age<-patage(patient_106_age0)
#tras date data to R format
p103age[,1]<-as.Date(p103age[,1])
p104age[,1]<-as.Date(p104age[,1])
p105age[,1]<-as.Date(p105age[,1])
p106age[,1]<-as.Date(p106age[,1])

#create all date

date_2014 = data.frame(date=seq(from = as.Date("2014-01-01"),to = as.Date("2014-12-31"),by = "day"))
date_2015 = data.frame(date=seq(from = as.Date("2015-01-01"),to = as.Date("2015-12-31"),by = "day"))
date_2016 = data.frame(date=seq(from = as.Date("2016-01-01"),to = as.Date("2016-12-31"),by = "day"))
date_2017 = data.frame(date=seq(from = as.Date("2017-01-01"),to = as.Date("2017-12-31"),by = "day"))
date_2018 = data.frame(date=seq(from = as.Date("2018-01-01"),to = as.Date("2018-7-31"),by = "day"))
#

date_2014_wday<-data.frame(date=date_2014$date,wday=wday(date_2014$date,week_start = getOption("lubridate.week.start", 1)))
date_2015_wday<-data.frame(date=date_2015$date,wday=wday(date_2015$date,week_start = getOption("lubridate.week.start", 1)))
date_2016_wday<-data.frame(date=date_2016$date,wday=wday(date_2016$date,week_start = getOption("lubridate.week.start", 1)))
date_2017_wday<-data.frame(date=date_2017$date,wday=wday(date_2017$date,week_start = getOption("lubridate.week.start", 1)))
date_2018_wday<-data.frame(date=date_2018$date,wday=wday(date_2018$date,week_start = getOption("lubridate.week.start", 1)))
####################################################

#cbind date and patient data
p103age_w<-merge(date_2014_wday, p103age[,c(1:5)], by.x="date",all.x = TRUE)
p104age_w<-merge(date_2015_wday, p104age[,c(1:5)], by.x="date",all.x = TRUE)
p105age_w<-merge(date_2016_wday, p105age[,c(1:5)], by.x="date",all.x = TRUE)
p106age_w<-merge(date_2017_wday, p106age[,c(1:5)], by.x="date",all.x = TRUE)
##########################################################
page4year<-rbind(p103age_w[which(p103age_w$wday!=6 &p103age_w$wday!=7),],
                 p104age_w[which(p104age_w$wday!=6 &p104age_w$wday!=7),],
                 p105age_w[which(p105age_w$wday!=6 &p105age_w$wday!=7),],
                 p106age_w[which(p106age_w$wday!=6 &p106age_w$wday!=7),])
page4year<-clean_NAdata(page4year,0)
lag0_daily[,21:24]<-page4year[,3:6]
lag1_daily[,21:24]<-page4year[-1,3:6]
lag2_daily[,21:24]<-page4year[-2:-1,3:6]
lag3_daily[,21:24]<-page4year[-3:-1,3:6]
lag4_daily[,21:24]<-page4year[-3:-1,3:6]
lag5_daily[,21:24]<-page4year[-3:-1,3:6]
lag6_daily[,21:24]<-page4year[-4:-1,3:6]
lag7_daily[,21:24]<-page4year[-5:-1,3:6]
#air data by state
#SO2
SO2_4years<-rbind(SO2_103_mean,
                  SO2_104_mean,
                  SO2_105_mean,
                  SO2_106_mean)
SO2_4years$date<-seq(from = as.Date("2014-01-01"),to = as.Date("2017-12-31"),by = "day")
write.csv(SO2_4years,"SO2_4years.csv",fileEncoding = "utf-8")
#CO
CO_4years<-rbind(CO_103_mean,
                  CO_104_mean,
                  CO_105_mean,
                  CO_106_mean)
CO_4years$date<-seq(from = as.Date("2014-01-01"),to = as.Date("2017-12-31"),by = "day")
write.csv(CO_4years,"CO_4years.csv",fileEncoding = "utf-8")
#O3
O3_4years<-rbind(O3_103_mean,
                  O3_104_mean,
                  O3_105_mean,
                  O3_106_mean)
O3_4years$date<-seq(from = as.Date("2014-01-01"),to = as.Date("2017-12-31"),by = "day")
write.csv(O3_4years,"O3_4years.csv",fileEncoding = "utf-8")
#PM2.5
PM2.5_4years<-rbind(PM2.5_103_mean,
                  PM2.5_104_mean,
                  PM2.5_105_mean,
                  PM2.5_106_mean)
PM2.5_4years$date<-seq(from = as.Date("2014-01-01"),to = as.Date("2017-12-31"),by = "day")
write.csv(PM2.5_4years,"PM2.5_4years.csv",fileEncoding = "utf-8")
#PM10
PM10_4years<-rbind(PM10_103_mean,
                  PM10_104_mean,
                  PM10_105_mean,
                  PM10_106_mean)
PM10_4years$date<-seq(from = as.Date("2014-01-01"),to = as.Date("2017-12-31"),by = "day")
write.csv(PM10_4years,"PM10_4years.csv",fileEncoding = "utf-8")
#NO
NO_4years<-rbind(NO_103_mean,
                  NO_104_mean,
                  NO_105_mean,
                  NO_106_mean)
NO_4years$date<-seq(from = as.Date("2014-01-01"),to = as.Date("2017-12-31"),by = "day")
write.csv(NO_4years,"NO_4years.csv",fileEncoding = "utf-8")
#NO2
NO2_4years<-rbind(NO2_103_mean,
                  NO2_104_mean,
                  NO2_105_mean,
                  NO2_106_mean)
NO2_4years$date<-seq(from = as.Date("2014-01-01"),to = as.Date("2017-12-31"),by = "day")
write.csv(NO2_4years,"NO2_4years.csv",fileEncoding = "utf-8")
#xlsx

export(SO2_4years,"SO2_4years.xlsx")
export(CO_4years,"CO_4years.xlsx")
export(O3_4years,"O3years.xlsx")
export(PM2.5_4years,"PM2.5_4years.xlsx")
export(PM10_4years,"PM10_4years.xlsx")
export(NO_4years,"NO_4years.xlsx")
export(NO2_4years,"NO2_4years.xlsx")
