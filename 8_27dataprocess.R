#E:/碩二上/空汙/106年 高屏空品區
library(xlsx)
setwd("E:/碩二上/空汙/106年 高屏空品區")
path<-("E:/碩二上/空汙/106年 高屏空品區")

#c("SO2","CO","O3","PM2.5","NO2")
##############################load.data#####################################
loaddata<-function(path = "./",confound=NULL,state=NULL){
  require(xlsx)
  fns <- list.files(path,pattern = "*.xls")
  res <- NULL
  for(i in fns) {
  data_original<-read.xlsx(file=i,sheetIndex=1,header = T,encoding = "UTF-8")
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

ptm <- proc.time()

ld<-loaddata(path="E:/碩二上/空汙/106年 高屏空品區",confound=c("SO2","CO","O3","PM2.5","NO2"))

proc.time() - ptm
#   user  system elapsed 
#1917.29    5.25 2113.14 

ptm <- proc.time()

allld<-loaddata(path="E:/碩二上/空汙/106年 高屏空品區")

proc.time() - ptm
#   user  system elapsed 
#1224.58    0.41 1225.44 

ptm <- proc.time()

O_3_data<-loaddata(path="E:/碩二上/空汙/106年 高屏空品區",confound=c("O3"))

proc.time() - ptm
#   user  system elapsed 
#1440.83    6.38 1454.80 
##########################################

O3_stat1<-O_3_data[O_3_data$測站 %in% "左營",]
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

ld_cleaned<-dataclean(ld)

proc.time() - ptm
# user  system elapsed 
#48.14    0.18   50.58  
tmp<-dataclean(O3_stat1)

##creat.date#####
date_2017 = data.frame(seq(from = as.Date("2017-01-01"),to = as.Date("2017-12-31"),by = "day"))
#################
outpatient<-read.xlsx(file="E:/碩二上/空汙/就診資料/20170101-20171231每日人數.xls",sheetIndex=1,header = T,encoding = "UTF-8")
outpatient_csv<-read.csv("E:/碩二上/空汙/就診資料/20170101-20171231每日人數.csv")
outpatient_csv<-outpatient_csv[1:249,1:4]
names(outpatient_csv)<-c("date","708","995.3","708&995.3")

outpatient_csv[,1]<-as.Date(outpatient_csv[,1])
colnames(date_2017)<-colnames(outpatient_csv)[1]

outpatient_alldate<-merge(date_2017, outpatient_csv, by.x="date",all.x = TRUE)
##########Interested information##########
max_O3<-data.frame(apply(tmp[4:27],1,max))
min_O3<-data.frame(apply(tmp[4:27],1,min))
mean_O3<-data.frame(apply(tmp[4:27],1,mean))
med_O3<-data.frame(apply(tmp[4:27],1,median))
O3_obs<-cbind(outpatient_alldate$date,
              max_O3,mean_O3,med_O3,min_O3,outpatient_alldate$`708&995.3`)
colnames(O3_obs)<-c("date","MaxO3","MeanO3","MedO3","MinO3","#people")
O3_obs$panel<-"a"
outpatient_alldate$panel<-"b"
O3_all<-rbind(O3_obs,outpatient_alldate)

par(mfrow = c(1, 2))
p2<-plot(x=tmp$as.Date.res_frame...1..,y=max_O3$apply.tmp.4.27...1..max.,type = "l",col="#ff6666",
         main = "全年臭氧趨勢", xlab ="date", ylab = "O3",ylim=c(0,150))
legend("topright", cex=0.5,                             
       pch = "l",                                
       col = c("#ff6666"), 
       legend = c("max_O3")
)
abline(125,0,                          
       lwd=1,col="#ff8c1a")
p1<-plot(x=outpatient_alldate$date,y=outpatient_alldate$`708&995.3`,type = "l",col="#333333",
         main = "就診人數", xlab ="date", ylab = "人數",ylim=c(0,40))

##########################################
par(mfrow = c(1, 2))
p2<-plot(x=tmp$as.Date.res_frame...1..,y=max_O3$apply.tmp.4.27...1..max.,type = "l",col="#ff6666",
     main = "全年臭氧趨勢", xlab ="date", ylab = "O3",ylim=c(0,150))
legend("topright", cex=0.5,                             
       pch = "l",                                
       col = c("#ff6666","#99ff66","#6699ff","#ffd633"), 
       legend = c("max_O3", "min_O3", "mean_O3","med_O3")
)
abline(125,0,                          
       lwd=1,col="#ff8c1a")
par(new=T)
p3<-plot(x=tmp$as.Date.res_frame...1..,y=min_O3$apply.tmp.4.27...1..min.,type = "l",col="#99ff66",
     main = "全年臭氧趨勢", xlab ="date", ylab = "O3",ylim=c(0,150))
par(new=T)
p4<-plot(x=tmp$as.Date.res_frame...1..,y=mean_O3$apply.tmp.4.27...1..mean.,type = "l",col="#6699ff",
     main = "全年臭氧趨勢", xlab ="date", ylab = "O3",ylim=c(0,150))
par(new=T)
p5<-plot(x=tmp$as.Date.res_frame...1..,y=med_O3$apply.tmp.4.27...1..median.,type = "l",col="#ffd633",
     main = "全年臭氧趨勢", xlab ="date", ylab = "O3",ylim=c(0,150))

p1<-plot(x=outpatient_alldate$date,y=outpatient_alldate$`708&995.3`,type = "l",col="#333333",
         main = "就診人數", xlab ="date", ylab = "人數",ylim=c(0,40))
##########################################
par(mfrow = c(2, 2))
plot(x=O3_obs$MaxO3,y=O3_obs$`#people`,type = "p",col="#1a53ff",pch=20,
     main = "MaxO3 V.S. 就診人數", ylab ="就診人數", xlab = "O3")
abline(lm(`#people`~MaxO3, O3_obs),                          
       lwd=1,col="red")
plot(x=O3_obs$MinO3,y=O3_obs$`#people`,type = "p",col="#1a53ff",pch=20,
     main = "MinO3 V.S. 就診人數", ylab ="就診人數", xlab = "O3")
abline(lm(`#people`~MinO3, O3_obs),                          
       lwd=1,col="red")
plot(x=O3_obs$MedO3,y=O3_obs$`#people`,type = "p",col="#1a53ff",pch=20,
     main = "MedO3 V.S. 就診人數", ylab ="就診人數", xlab = "O3")
abline(lm(`#people`~MedO3, O3_obs),                          
       lwd=1,col="red")
plot(x=O3_obs$MeanO3,y=O3_obs$`#people`,type = "p",col="#1a53ff",pch=20,
     main = "MeanO3 V.S. 就診人數", ylab ="就診人數", xlab = "O3")
abline(lm(`#people`~MeanO3, O3_obs),                          
       lwd=1,col="red")
library(ggplot2)
par(mfrow = c(1, 1))
plot(x=O3_obs$MaxO3,y=O3_obs$`#people`,type = "p",col="#1a53ff",pch=20,
     main = "MaxO3 V.S. 就診人數", ylab ="就診人數", xlab = "O3")
abline(lm(`#people`~MaxO3, O3_obs),                          
       lwd=1,col="red")
