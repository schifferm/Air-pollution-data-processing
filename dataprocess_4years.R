
path103<-("E:/碩二上/空汙/103年 高屏空品區/")
ptm <- proc.time()

SO2_data_103<-loaddata(path=path103,confound=c("SO2"))
CO_data_103<-loaddata(path=path103,confound=c("CO"))
O3_data_103<-loaddata(path=path103,confound=c("O3"))
PM2.5_data_103<-loaddata(path=path103,confound=c("PM2.5"))
NO2_data_103<-loaddata(path=path103,confound=c("NO2"))
PM10_data_103<-loaddata(path=path103,confound=c("PM10"))
NO_data_103<-loaddata(path=path103,confound=c("NO"))

RAIN_data_103<-loaddata(path=path103,confound=c("RAINFALL"))
TEMP_data_103<-loaddata(path=path103,confound=c("AMB_TEMP"))
RH_data_103<-loaddata(path=path103,confound=c("RH"))
WS_HR_data_103<-loaddata(path=path103,confound=c("WS_HR"))
proc.time() - ptm

path104<-("E:/碩二上/空汙/104年 高屏空品區")
ptm <- proc.time()
SO2_data_104<-loaddata(path=path104,confound=c("SO2"))
CO_data_104<-loaddata(path=path104,confound=c("CO"))
O3_data_104<-loaddata(path=path104,confound=c("O3"))
PM2.5_data_104<-loaddata(path=path104,confound=c("PM2.5"))
NO2_data_104<-loaddata(path=path104,confound=c("NO2"))
PM10_data_104<-loaddata(path=path104,confound=c("PM10"))
NO_data_104<-loaddata(path=path104,confound=c("NO"))

RAIN_data_104<-loaddata(path=path104,confound=c("RAINFALL"))
TEMP_data_104<-loaddata(path=path104,confound=c("AMB_TEMP"))
RH_data_104<-loaddata(path=path104,confound=c("RH"))
WS_HR_data_104<-loaddata(path=path104,confound=c("WS_HR"))
proc.time() - ptm

path105<-("E:/碩二上/空汙/105年 高屏空品區")
ptm <- proc.time()

SO2_data_105<-loaddata(path=path105,confound=c("SO2"))
CO_data_105<-loaddata(path=path105,confound=c("CO"))
O3_data_105<-loaddata(path=path105,confound=c("O3"))
PM2.5_data_105<-loaddata(path=path105,confound=c("PM2.5"))
NO2_data_105<-loaddata(path=path105,confound=c("NO2"))
PM10_data_105<-loaddata(path=path105,confound=c("PM10"))
NO_data_105<-loaddata(path=path105,confound=c("NO"))

RAIN_data_105<-loaddata(path=path105,confound=c("RAINFALL"))
TEMP_data_105<-loaddata(path=path105,confound=c("AMB_TEMP"))
RH_data_105<-loaddata(path=path105,confound=c("RH"))
WS_HR_data_105<-loaddata(path=path105,confound=c("WS_HR"))
proc.time() - ptm


path106<-("E:/碩二上/空汙/106年 高屏空品區")
ptm <- proc.time()
SO2_data_106<-loaddata(path=path106,confound=c("SO2"))
CO_data_106<-loaddata(path=path106,confound=c("CO"))
O3_data_106<-loaddata(path=path106,confound=c("O3"))
PM2.5_data_106<-loaddata(path=path106,confound=c("PM2.5"))
NO2_data_106<-loaddata(path=path106,confound=c("NO2"))
PM10_data_106<-loaddata(path=path106,confound=c("PM10"))
NO_data_106<-loaddata(path=path106,confound=c("NO"))

RAIN_data_106<-loaddata(path=path106,confound=c("RAINFALL"))
TEMP_data_106<-loaddata(path=path106,confound=c("AMB_TEMP"))
RH_data_106<-loaddata(path=path106,confound=c("RH"))
WS_HR_data_106<-loaddata(path=path106,confound=c("WS_HR"))
proc.time() - ptm

path107<-("E:/碩二上/空汙/107年 高屏空品區")
ptm <- proc.time()

SO2_data_107<-loaddata(path=path107,confound=c("SO2"))
CO_data_107<-loaddata(path=path107,confound=c("CO"))
O3_data_107<-loaddata(path=path107,confound=c("O3"))
PM2.5_data_107<-loaddata(path=path107,confound=c("PM2.5"))
NO2_data_107<-loaddata(path=path107,confound=c("NO2"))
PM10_data_107<-loaddata(path=path107,confound=c("PM10"))
NO_data_107<-loaddata(path=path107,confound=c("NO"))

RAIN_data_107<-loaddata(path=path107,confound=c("RAINFALL"))
TEMP_data_107<-loaddata(path=path107,confound=c("AMB_TEMP"))
RH_data_107<-loaddata(path=path107,confound=c("RH"))
WS_HR_data_107<-loaddata(path=path107,confound=c("WS_HR"))
proc.time() - ptm
# about 2967 second

ptm <- proc.time()
#103
SO2_cleaned_103<-dataclean(SO2_data_103)
CO_cleaned_103<-dataclean(CO_data_103)
O3_cleaned_103<-dataclean(O3_data_103)
PM2.5_cleaned_103<-dataclean(PM2.5_data_103)
PM10_cleaned_103<-dataclean(PM10_data_103)
NO_cleaned_103<-dataclean(NO_data_103)
NO2_cleaned_103<-dataclean(NO2_data_103)

RAIN_cleaned_103<-dataclean(RAIN_data_103)
TEMP_cleaned_103<-dataclean(TEMP_data_103)
RH_cleaned_103<-dataclean(RH_data_103)
WS_HR_cleaned_103<-dataclean(WS_HR_data_103)
#104
SO2_cleaned_104<-dataclean(SO2_data_104)
CO_cleaned_104<-dataclean(CO_data_104)
O3_cleaned_104<-dataclean(O3_data_104)
PM2.5_cleaned_104<-dataclean(PM2.5_data_104)
PM10_cleaned_104<-dataclean(PM10_data_104)
NO_cleaned_104<-dataclean(NO_data_104)
NO2_cleaned_104<-dataclean(NO2_data_104)

RAIN_cleaned_104<-dataclean(RAIN_data_104)
TEMP_cleaned_104<-dataclean(TEMP_data_104)
RH_cleaned_104<-dataclean(RH_data_104)
WS_HR_cleaned_104<-dataclean(WS_HR_data_104)
#105
SO2_cleaned_105<-dataclean(SO2_data_105)
CO_cleaned_105<-dataclean(CO_data_105)
O3_cleaned_105<-dataclean(O3_data_105)
PM2.5_cleaned_105<-dataclean(PM2.5_data_105)
PM10_cleaned_105<-dataclean(PM10_data_105)
NO_cleaned_105<-dataclean(NO_data_105)
NO2_cleaned_105<-dataclean(NO2_data_105)

RAIN_cleaned_105<-dataclean(RAIN_data_105)
TEMP_cleaned_105<-dataclean(TEMP_data_105)
RH_cleaned_105<-dataclean(RH_data_105)
WS_HR_cleaned_105<-dataclean(WS_HR_data_105)
#106
SO2_cleaned_106<-dataclean(SO2_data_106)
CO_cleaned_106<-dataclean(CO_data_106)
O3_cleaned_106<-dataclean(O3_data_106)
PM2.5_cleaned_106<-dataclean(PM2.5_data_106)
PM10_cleaned_106<-dataclean(PM10_data_106)
NO_cleaned_106<-dataclean(NO_data_106)
NO2_cleaned_106<-dataclean(NO2_data_106)

RAIN_cleaned_106<-dataclean(RAIN_data_106)
TEMP_cleaned_106<-dataclean(TEMP_data_106)
RH_cleaned_106<-dataclean(RH_data_106)
WS_HR_cleaned_106<-dataclean(WS_HR_data_106)
#107
SO2_cleaned_107<-dataclean(SO2_data_107)
CO_cleaned_107<-dataclean(CO_data_107)
O3_cleaned_107<-dataclean(O3_data_107)
PM2.5_cleaned_107<-dataclean(PM2.5_data_107)
PM10_cleaned_107<-dataclean(PM10_data_107)
NO_cleaned_107<-dataclean(NO_data_107)
NO2_cleaned_107<-dataclean(NO2_data_107)

RAIN_cleaned_107<-dataclean(RAIN_data_107)
TEMP_cleaned_107<-dataclean(TEMP_data_107)
RH_cleaned_107<-dataclean(RH_data_107)
WS_HR_cleaned_107<-dataclean(WS_HR_data_107)
proc.time() - ptm
#about 1254 seconds

#change NA value to 0
ptm <- proc.time()
#103
SO2_clean_103<-clean_data(SO2_cleaned_103,0)
CO_clean_103<-clean_data(CO_cleaned_103,0)
O3_clean_103<-clean_data(O3_cleaned_103,0)
PM2.5_clean_103<-clean_data(PM2.5_cleaned_103,0)
PM10_clean_103<-clean_data(PM10_cleaned_103,0)
NO_clean_103<-clean_data(NO_cleaned_103,0)
NO2_clean_103<-clean_data(NO2_cleaned_103,0)

RAIN_clean_103<-clean_data(RAIN_cleaned_103,0)
TEMP_clean_103<-clean_data(TEMP_cleaned_103,0)
RH_clean_103<-clean_data(RH_cleaned_103,0)
WS_HR_clean_103<-clean_data(WS_HR_cleaned_103,0)
#104
SO2_clean_104<-clean_data(SO2_cleaned_104,0)
CO_clean_104<-clean_data(CO_cleaned_104,0)
O3_clean_104<-clean_data(O3_cleaned_104,0)
PM2.5_clean_104<-clean_data(PM2.5_cleaned_104,0)
PM10_clean_104<-clean_data(PM10_cleaned_104,0)
NO_clean_104<-clean_data(NO_cleaned_104,0)
NO2_clean_104<-clean_data(NO2_cleaned_104,0)

RAIN_clean_104<-clean_data(RAIN_cleaned_104,0)
TEMP_clean_104<-clean_data(TEMP_cleaned_104,0)
RH_clean_104<-clean_data(RH_cleaned_104,0)
WS_HR_clean_104<-clean_data(WS_HR_cleaned_104,0)
#105
SO2_clean_105<-clean_data(SO2_cleaned_105,0)
CO_clean_105<-clean_data(CO_cleaned_105,0)
O3_clean_105<-clean_data(O3_cleaned_105,0)
PM2.5_clean_105<-clean_data(PM2.5_cleaned_105,0)
PM10_clean_105<-clean_data(PM10_cleaned_105,0)
NO_clean_105<-clean_data(NO_cleaned_105,0)
NO2_clean_105<-clean_data(NO2_cleaned_105,0)

RAIN_clean_105<-clean_data(RAIN_cleaned_105,0)
TEMP_clean_105<-clean_data(TEMP_cleaned_105,0)
RH_clean_105<-clean_data(RH_cleaned_105,0)
WS_HR_clean_105<-clean_data(WS_HR_cleaned_105,0)
#106
SO2_clean_106<-clean_data(SO2_cleaned_106,0)
CO_clean_106<-clean_data(CO_cleaned_106,0)
O3_clean_106<-clean_data(O3_cleaned_106,0)
PM2.5_clean_106<-clean_data(PM2.5_cleaned_106,0)
PM10_clean_106<-clean_data(PM10_cleaned_106,0)
NO_clean_106<-clean_data(NO_cleaned_106,0)
NO2_clean_106<-clean_data(NO2_cleaned_106,0)

RAIN_clean_106<-clean_data(RAIN_cleaned_106,0)
TEMP_clean_106<-clean_data(TEMP_cleaned_106,0)
RH_clean_106<-clean_data(RH_cleaned_106,0)
WS_HR_clean_106<-clean_data(WS_HR_cleaned_106,0)
#107
SO2_clean_107<-clean_data(SO2_cleaned_107,0)
CO_clean_107<-clean_data(CO_cleaned_107,0)
O3_clean_107<-clean_data(O3_cleaned_107,0)
PM2.5_clean_107<-clean_data(PM2.5_cleaned_107,0)
PM10_clean_107<-clean_data(PM10_cleaned_107,0)
NO_clean_107<-clean_data(NO_cleaned_107,0)
NO2_clean_107<-clean_data(NO2_cleaned_107,0)

RAIN_clean_107<-clean_data(RAIN_cleaned_107,0)
TEMP_clean_107<-clean_data(TEMP_cleaned_107,0)
RH_clean_107<-clean_data(RH_cleaned_107,0)
WS_HR_clean_107<-clean_data(WS_HR_cleaned_107,0)
proc.time() - ptm
#about 1000 seconds


###########################################
# original  patient data
patient<-data.frame(read.csv("E:/碩二上/空汙/就診資料/20170101-20171231每日人數.csv")[,c(1:4)])
patient_103<-data.frame(read_xlsx(path ="E:/碩二上/空汙/就診資料/2014_1-12月高雄市20歲以上.xlsx",sheet=1,col_names = TRUE,col_types=c("date","numeric","numeric","numeric")))
patient_104<-data.frame(read_xlsx(path ="E:/碩二上/空汙/就診資料/2015_1-12月_高雄市且20歲以上.xlsx",sheet=1,col_names = TRUE,col_types=c("date","numeric","numeric","numeric")))
patient_105<-data.frame(read_xlsx(path ="E:/碩二上/空汙/就診資料/2016_1-12月_高雄市且20歲以上.xlsx",sheet=1,col_names = TRUE,col_types=c("date","numeric","numeric","numeric")))
patient_106<-data.frame(read_xlsx(path ="E:/碩二上/空汙/就診資料/2017-4_20yo_只高雄縣市.xlsx",sheet=1,col_names = TRUE,col_types=c("date","numeric","numeric","numeric")))
patient_107_7<-data.frame(read_xlsx(path ="E:/碩二上/空汙/就診資料/2018_1-7月_高雄市20歲以上.xlsx",sheet=1,col_names = TRUE,col_types=c("date","numeric","numeric","numeric")))
names(patient_103)<-c("date","urticaria","allergy","both")
names(patient_104)<-c("date","urticaria","allergy","both")
names(patient_105)<-c("date","urticaria","allergy","both")
names(patient_106)<-c("date","urticaria","allergy","both")
names(patient_107_7)<-c("date","urticaria","allergy","both")
#tras date data to R format
patient_103[,1]<-as.Date(patient_103[,1])
patient_104[,1]<-as.Date(patient_104[,1])
patient_105[,1]<-as.Date(patient_105[,1])
patient_106[,1]<-as.Date(patient_106[,1])
patient_107_7[,1]<-as.Date(patient_107_7[,1])
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

#cbind date and patient data
patient_2014_wday<-merge(date_2014_wday, patient_103[,c(1:3)], by.x="date",all.x = TRUE)
patient_2015_wday<-merge(date_2015_wday, patient_104[,c(1:3)], by.x="date",all.x = TRUE)
patient_2016_wday<-merge(date_2016_wday, patient_105[,c(1:3)], by.x="date",all.x = TRUE)
patient_2017_wday<-merge(date_2017_wday, patient_106[,c(1:3)], by.x="date",all.x = TRUE)
patient_2018_wday<-merge(date_2018_wday, patient_107_7[,c(1:3)], by.x="date",all.x = TRUE)

##########################################################
#calculate each air mean of day
ptm <- proc.time()
#103
CO_103_mean<-cal_mean(CO_clean_103)
SO2_103_mean<-cal_mean(SO2_clean_103)
O3_103_mean<-cal_mean(O3_clean_103)
PM2.5_103_mean<-cal_mean(PM2.5_clean_103)
PM10_103_mean<-cal_mean(PM10_clean_103)
NO_103_mean<-cal_mean(NO_clean_103)
NO2_103_mean<-cal_mean(NO2_clean_103)

RAIN_103_mean<-cal_mean(RAIN_clean_103)
TEMP_103_mean<-cal_mean(TEMP_clean_103)
RH_103_mean<-cal_mean(RH_clean_103)
WS_HR_103_mean<-cal_mean(WS_HR_clean_103)

#104
CO_104_mean<-cal_mean(CO_clean_104)
SO2_104_mean<-cal_mean(SO2_clean_104)
O3_104_mean<-cal_mean(O3_clean_104)
PM2.5_104_mean<-cal_mean(PM2.5_clean_104)
PM10_104_mean<-cal_mean(PM10_clean_104)
NO_104_mean<-cal_mean(NO_clean_104)
NO2_104_mean<-cal_mean(NO2_clean_104)

RAIN_104_mean<-cal_mean(RAIN_clean_104)
TEMP_104_mean<-cal_mean(TEMP_clean_104)
RH_104_mean<-cal_mean(RH_clean_104)
WS_HR_104_mean<-cal_mean(WS_HR_clean_104)

#105
CO_105_mean<-cal_mean(CO_clean_105)
SO2_105_mean<-cal_mean(SO2_clean_105)
O3_105_mean<-cal_mean(O3_clean_105)
PM2.5_105_mean<-cal_mean(PM2.5_clean_105)
PM10_105_mean<-cal_mean(PM10_clean_105)
NO_105_mean<-cal_mean(NO_clean_105)
NO2_105_mean<-cal_mean(NO2_clean_105)

RAIN_105_mean<-cal_mean(RAIN_clean_105)
TEMP_105_mean<-cal_mean(TEMP_clean_105)
RH_105_mean<-cal_mean(RH_clean_105)
WS_HR_105_mean<-cal_mean(WS_HR_clean_105)

#106
CO_106_mean<-cal_mean(CO_clean_106)
SO2_106_mean<-cal_mean(SO2_clean_106)
O3_106_mean<-cal_mean(O3_clean_106)
PM2.5_106_mean<-cal_mean(PM2.5_clean_106)
PM10_106_mean<-cal_mean(PM10_clean_106)
NO_106_mean<-cal_mean(NO_clean_106)
NO2_106_mean<-cal_mean(NO2_clean_106)

RAIN_106_mean<-cal_mean(RAIN_clean_106)
TEMP_106_mean<-cal_mean(TEMP_clean_106)
RH_106_mean<-cal_mean(RH_clean_106)
WS_HR_106_mean<-cal_mean(WS_HR_clean_106)

#107
CO_107_mean<-cal_mean(CO_clean_107)
SO2_107_mean<-cal_mean(SO2_clean_107)
O3_107_mean<-cal_mean(O3_clean_107)
PM2.5_107_mean<-cal_mean(PM2.5_clean_107)
PM10_107_mean<-cal_mean(PM10_clean_107)
NO_107_mean<-cal_mean(NO_clean_107)
NO2_107_mean<-cal_mean(NO2_clean_107)

RAIN_107_mean<-cal_mean(RAIN_clean_107)
TEMP_107_mean<-cal_mean(TEMP_clean_107)
RH_107_mean<-cal_mean(RH_clean_107)
WS_HR_107_mean<-cal_mean(WS_HR_clean_107)
proc.time() - ptm
# 141 seconds

#cbind all state air mean
ptm <- proc.time()
#103
CO_103<-databindALL(CO_103_mean[,c(-8,-9,-14)],patient_2014_wday)
SO2_103<-databindALL(SO2_103_mean[,c(-8,-9,-14)],patient_2014_wday)
O3_103<-databindALL(O3_103_mean[,c(-8,-9,-14)],patient_2014_wday)
PM2.5_103<-databindALL(PM2.5_103_mean[,c(-8,-9,-14)],patient_2014_wday)
PM10_103<-databindALL(PM10_103_mean[,c(-8,-9,-14)],patient_2014_wday)
NO_103<-databindALL(NO_103_mean[,c(-8,-9,-14)],patient_2014_wday)
NO2_103<-databindALL(NO2_103_mean[,c(-8,-9,-14)],patient_2014_wday)

RAIN_103<-databindALL(RAIN_103_mean[,c(-8,-9,-14)],patient_2014_wday)
TEMP_103<-databindALL(TEMP_103_mean[,c(-8,-9,-14)],patient_2014_wday)
RH_103<-databindALL(RH_103_mean[,c(-8,-9,-14)],patient_2014_wday)
WS_HR_103<-databindALL(WS_HR_103_mean[,c(-8,-9,-14)],patient_2014_wday)

#104
CO_104<-databindALL(CO_104_mean[,c(-8,-9,-14)],patient_2015_wday)
SO2_104<-databindALL(SO2_104_mean[,c(-8,-9,-14)],patient_2015_wday)
O3_104<-databindALL(O3_104_mean[,c(-8,-9,-14)],patient_2015_wday)
PM2.5_104<-databindALL(PM2.5_104_mean[,c(-8,-9,-14)],patient_2015_wday)
PM10_104<-databindALL(PM10_104_mean[,c(-8,-9,-14)],patient_2015_wday)
NO_104<-databindALL(NO_104_mean[,c(-8,-9,-14)],patient_2015_wday)
NO2_104<-databindALL(NO2_104_mean[,c(-8,-9,-14)],patient_2015_wday)

RAIN_104<-databindALL(RAIN_104_mean[,c(-8,-9,-14)],patient_2015_wday)
TEMP_104<-databindALL(TEMP_104_mean[,c(-8,-9,-14)],patient_2015_wday)
RH_104<-databindALL(RH_104_mean[,c(-8,-9,-14)],patient_2015_wday)
WS_HR_104<-databindALL(WS_HR_104_mean[,c(-8,-9,-14)],patient_2015_wday)

#105
CO_105<-databindALL(CO_105_mean[,c(-8,-9,-14)],patient_2016_wday)
SO2_105<-databindALL(SO2_105_mean[,c(-8,-9,-14)],patient_2016_wday)
O3_105<-databindALL(O3_105_mean[,c(-8,-9,-14)],patient_2016_wday)
PM2.5_105<-databindALL(PM2.5_105_mean[,c(-8,-9,-14)],patient_2016_wday)
PM10_105<-databindALL(PM10_105_mean[,c(-8,-9,-14)],patient_2016_wday)
NO_105<-databindALL(NO_105_mean[,c(-8,-9,-14)],patient_2016_wday)
NO2_105<-databindALL(NO2_105_mean[,c(-8,-9,-14)],patient_2016_wday)

RAIN_105<-databindALL(RAIN_105_mean[,c(-8,-9,-14)],patient_2016_wday)
TEMP_105<-databindALL(TEMP_105_mean[,c(-8,-9,-14)],patient_2016_wday)
RH_105<-databindALL(RH_105_mean[,c(-8,-9,-14)],patient_2016_wday)
WS_HR_105<-databindALL(WS_HR_105_mean[,c(-8,-9,-14)],patient_2016_wday)

#106
CO_106<-databindALL(CO_106_mean[,c(-8,-9,-14)],patient_2017_wday)
SO2_106<-databindALL(SO2_106_mean[,c(-8,-9,-14)],patient_2017_wday)
O3_106<-databindALL(O3_106_mean[,c(-8,-9,-14)],patient_2017_wday)
PM2.5_106<-databindALL(PM2.5_106_mean[,c(-8,-9,-14)],patient_2017_wday)
PM10_106<-databindALL(PM10_106_mean[,c(-8,-9,-14)],patient_2017_wday)
NO_106<-databindALL(NO_106_mean[,c(-8,-9,-14)],patient_2017_wday)
NO2_106<-databindALL(NO2_106_mean[,c(-8,-9,-14)],patient_2017_wday)

RAIN_106<-databindALL(RAIN_106_mean[,c(-8,-9,-14)],patient_2017_wday)
TEMP_106<-databindALL(TEMP_106_mean[,c(-8,-9,-14)],patient_2017_wday)
RH_106<-databindALL(RH_106_mean[,c(-8,-9,-14)],patient_2017_wday)
WS_HR_106<-databindALL(WS_HR_106_mean[,c(-8,-9,-14)],patient_2017_wday)

#107
CO_107<-databindALL(CO_107_mean[c(1:212),c(-8,-9,-14)],patient_2018_wday)
SO2_107<-databindALL(SO2_107_mean[c(1:212),c(-8,-9,-14)],patient_2018_wday)
O3_107<-databindALL(O3_107_mean[c(1:212),c(-8,-9,-14)],patient_2018_wday)
PM2.5_107<-databindALL(PM2.5_107_mean[c(1:212),c(-8,-9,-14)],patient_2018_wday)
PM10_107<-databindALL(PM10_107_mean[c(1:212),c(-8,-9,-14)],patient_2018_wday)
NO_107<-databindALL(NO_107_mean[c(1:212),c(-8,-9,-14)],patient_2018_wday)
NO2_107<-databindALL(NO2_107_mean[c(1:212),c(-8,-9,-14)],patient_2018_wday)

RAIN_107<-databindALL(RAIN_107_mean[c(1:212),c(-8,-9,-14)],patient_2018_wday)
TEMP_107<-databindALL(TEMP_107_mean[c(1:212),c(-8,-9,-14)],patient_2018_wday)
RH_107<-databindALL(RH_107_mean[c(1:212),c(-8,-9,-14)],patient_2018_wday)
WS_HR_107<-databindALL(WS_HR_107_mean[c(1:212),c(-8,-9,-14)],patient_2018_wday)

proc.time() - ptm

#cbind the same year data
#2014
data_2014<-data.frame(patient_2014_wday,
                      yday=c(1:dim(patient_2014_wday)[1]),
                      SO2=SO2_103$Mean,
                      CO=CO_103$Mean,
                      O3=O3_103$Mean,
                      PM2.5=PM2.5_103$Mean,
                      PM10=PM10_103$Mean,
                      NO=NO_103$Mean,
                      NO2=NO2_103$Mean,
                      TEMP=TEMP_103$Mean,
                      RH=RH_103$Mean,
                      RAIN=RAIN_103$Mean,
                      WS_HR=WS_HR_103$Mean)

#2015
data_2015<-data.frame(patient_2015_wday,
                      yday=c(1:dim(patient_2015_wday)[1]),
                      SO2=SO2_104$Mean,
                      CO=CO_104$Mean,
                      O3=O3_104$Mean,
                      PM2.5=PM2.5_104$Mean,
                      PM10=PM10_104$Mean,
                      NO=NO_104$Mean,
                      NO2=NO2_104$Mean,
                      TEMP=TEMP_104$Mean,
                      RH=RH_104$Mean,
                      RAIN=RAIN_104$Mean,
                      WS_HR=WS_HR_104$Mean)

#2016
data_2016<-data.frame(patient_2016_wday,
                      yday=c(1:dim(patient_2016_wday)[1]),
                      SO2=SO2_105$Mean,
                      CO=CO_105$Mean,
                      O3=O3_105$Mean,
                      PM2.5=PM2.5_105$Mean,
                      PM10=PM10_105$Mean,
                      NO=NO_105$Mean,
                      NO2=NO2_105$Mean,
                      TEMP=TEMP_105$Mean,
                      RH=RH_105$Mean,
                      RAIN=RAIN_105$Mean,
                      WS_HR=WS_HR_105$Mean)

#2017
data_2017<-data.frame(patient_2017_wday,
                      yday=c(1:dim(patient_2017_wday)[1]),
                      SO2=SO2_106$Mean,
                      CO=CO_106$Mean,
                      O3=O3_106$Mean,
                      PM2.5=PM2.5_106$Mean,
                      PM10=PM10_106$Mean,
                      NO=NO_106$Mean,
                      NO2=NO2_106$Mean,
                      TEMP=TEMP_106$Mean,
                      RH=RH_106$Mean,
                      RAIN=RAIN_106$Mean,
                      WS_HR=WS_HR_106$Mean)

#2018
data_2018<-data.frame(patient_2018_wday,
                      yday=c(1:dim(patient_2018_wday)[1]),
                      SO2=SO2_107$Mean,
                      CO=CO_107$Mean,
                      O3=O3_107$Mean,
                      PM2.5=PM2.5_107$Mean,
                      PM10=PM10_107$Mean,
                      NO=NO_107$Mean,
                      NO2=NO2_107$Mean,
                      TEMP=TEMP_107$Mean,
                      RH=RH_107$Mean,
                      RAIN=RAIN_107$Mean,
                      WS_HR=WS_HR_107$Mean)
data_2014$RAIN[which(data_2014$RAIN=="NaN")]<-0
data_2015$RAIN[which(data_2015$RAIN=="NaN")]<-0
data_2016$RAIN[which(data_2016$RAIN=="NaN")]<-0
data_2017$RAIN[which(data_2017$RAIN=="NaN")]<-0
data_2018$RAIN[which(data_2018$RAIN=="NaN")]<-0

data_4years<-rbind(data_2014,data_2015,data_2016,data_2017,data_2018)

write.csv(data_4years,"data_4years.csv",fileEncoding = "utf-8")

lag_day<-data.frame(lag1=data_4years$date+1,
                    lag2=data_4years$date+2,
                    lag3=data_4years$date+3,
                    lag4=data_4years$date+4,
                    lag5=data_4years$date+5,
                    lag6=data_4years$date+6,
                    lag7=data_4years$date+7)
lag1_data<-merge(data.frame(date=lag_day$lag1),data_4years[,1:4])
lag2_data<-merge(data.frame(date=lag_day$lag2),data_4years[,1:4])
lag3_data<-merge(data.frame(date=lag_day$lag3),data_4years[,1:4])
lag4_data<-merge(data.frame(date=lag_day$lag4),data_4years[,1:4])
lag5_data<-merge(data.frame(date=lag_day$lag5),data_4years[,1:4])
lag6_data<-merge(data.frame(date=lag_day$lag6),data_4years[,1:4])
lag7_data<-merge(data.frame(date=lag_day$lag7),data_4years[,1:4])

lag1_data_4years<-data_4years
lag2_data_4years<-data_4years
lag3_data_4years<-data_4years
lag4_data_4years<-data_4years
lag5_data_4years<-data_4years
lag6_data_4years<-data_4years
lag7_data_4years<-data_4years

lag0_data_4years<-data_4years
lag1_data_4years[,1:4]<-lag1_data[1:1673,]
lag2_data_4years[,1:4]<-lag2_data[1:1673,]
lag3_data_4years[,1:4]<-lag3_data[1:1673,]
lag4_data_4years[,1:4]<-lag4_data[1:1673,]
lag5_data_4years[,1:4]<-lag5_data[1:1673,]
lag6_data_4years[,1:4]<-lag6_data[1:1673,]
lag7_data_4years[,1:4]<-lag7_data[1:1673,]

lag0_daily<-lag0_data_4years[which(lag0_data_4years$wday!=6 & lag0_data_4years$wday!=7),]
lag1_daily<-lag1_data_4years[which(lag1_data_4years$wday!=6 & lag1_data_4years$wday!=7),]
lag2_daily<-lag2_data_4years[which(lag2_data_4years$wday!=6 & lag2_data_4years$wday!=7),]
lag3_daily<-lag3_data_4years[which(lag3_data_4years$wday!=6 & lag3_data_4years$wday!=7),]
lag4_daily<-lag4_data_4years[which(lag4_data_4years$wday!=6 & lag4_data_4years$wday!=7),]
lag5_daily<-lag5_data_4years[which(lag5_data_4years$wday!=6 & lag5_data_4years$wday!=7),]
lag6_daily<-lag6_data_4years[which(lag6_data_4years$wday!=6 & lag6_data_4years$wday!=7),]
lag7_daily<-lag7_data_4years[which(lag7_data_4years$wday!=6 & lag7_data_4years$wday!=7),]


lag0_daily<-lag0_daily[which(lag0_daily$date<as.Date("2018-01-01")),]
lag1_daily<-lag1_daily[which(lag1_daily$date<as.Date("2018-01-01")),]
lag2_daily<-lag2_daily[which(lag2_daily$date<as.Date("2018-01-01")),]
lag3_daily<-lag3_daily[which(lag3_daily$date<as.Date("2018-01-01")),]
lag4_daily<-lag4_daily[which(lag4_daily$date<as.Date("2018-01-01")),]
lag5_daily<-lag5_daily[which(lag5_daily$date<as.Date("2018-01-01")),]
lag6_daily<-lag6_daily[which(lag6_daily$date<as.Date("2018-01-01")),]
lag7_daily<-lag7_daily[which(lag7_daily$date<as.Date("2018-01-01")),]
