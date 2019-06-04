library(mgcv)
library(xtable)
data.air$allergy<-outpatient_csv$X995.3
data.air$hives<-outpatient_csv$X708

write.csv(data.air,"daily_withpatient.csv",fileEncoding = "utf-8",
          row.names=FALSE)

CO_allergy<-gamair1(14,COm,data.air,TEMPm,RHm)
SO2_allergy<-gamair1(14,SO2m,data.air,TEMPm,RHm)
O3_allergy<-gamair1(14,O3m,data.air,TEMPm,RHm)
PM2.5_allergy<-gamair1(14,PM2.5m,data.air,TEMPm,RHm)
PM10_allergy<-gamair1(14,PM10m,data.air,TEMPm,RHm)
NO_allergy<-gamair1(14,NOm,data.air,TEMPm,RHm)
NO2_allergy<-gamair1(14,NO2m,data.air,TEMPm,RHm)

xtable(CO_allergy[[1]],digits =3)
xtable(SO2_allergy[[1]],digits =3)
xtable(O3_allergy[[1]],digits =3)
xtable(PM2.5_allergy[[1]],digits =3)
xtable(PM10_allergy[[1]],digits =3)
xtable(NO_allergy[[1]],digits =3)
xtable(NO2_allergy[[1]],digits =3)

xtable(CO_allergy[[2]],digits =3)
xtable(SO2_allergy[[2]],digits =3)
xtable(O3_allergy[[2]],digits =3)
xtable(PM2.5_allergy[[2]],digits =3)
xtable(PM10_allergy[[2]],digits =3)
xtable(NO_allergy[[2]],digits =3)
xtable(NO2_allergy[[2]],digits =3)

CO_hives<-gamair1(15,COm,data.air,TEMPm,RHm)
SO2_hives<-gamair1(15,SO2m,data.air,TEMPm,RHm)
O3_hives<-gamair1(15,O3m,data.air,TEMPm,RHm)
PM2.5_hives<-gamair1(15,PM2.5m,data.air,TEMPm,RHm)
PM10_hives<-gamair1(15,PM10m,data.air,TEMPm,RHm)
NO_hives<-gamair1(15,NOm,data.air,TEMPm,RHm)
NO2_hives<-gamair1(15,NO2m,data.air,TEMPm,RHm)



xtable(CO_hives[[1]],digits =3)
xtable(SO2_hives[[1]],digits =3)
xtable(O3_hives[[1]],digits =3)
xtable(PM2.5_hives[[1]],digits =3)
xtable(PM10_hives[[1]],digits =3)
xtable(NO_hives[[1]],digits =3)
xtable(NO2_hives[[1]],digits =3)

xtable(CO_hives[[2]],digits =3)
xtable(SO2_hives[[2]],digits =3)
xtable(O3_hives[[2]],digits =3)
xtable(PM2.5_hives[[2]],digits =3)
xtable(PM10_hives[[2]],digits =3)
xtable(NO_hives[[2]],digits =3)
xtable(NO2_hives[[2]],digits =3)


