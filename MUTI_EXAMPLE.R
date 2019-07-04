
rrcul<-function(summ){#計算RR與CI用
  beta<-coef(summ)
  Vb <- vcov(summ, unconditional = TRUE)
  se <- sqrt(diag(Vb))
  muti_rr<-data.frame()
  for(j in 1:16){
    k<-c("SO2","CO","O3","PM2.5","PM10","NO","NO2",
         "as.factor(wday)2","as.factor(wday)3","as.factor(wday)4","as.factor(wday)5",
         "as.numeric(TEMP)","as.numeric(RH)","as.numeric(yday)","as.numeric(WIND_SPEED)","as.numeric(RAIN)")[j]
    i <- which(names(beta) == k)
    muti_rr[j,1]<-exp(summ[[1]][k])
    muti_rr[j,2]<-exp(beta[i] + (c(-1,1) * (2 * se[i])))[1]
    muti_rr[j,3]<-exp(beta[i] + (c(-1,1) * (2 * se[i])))[2]
    
  }
  rownames(muti_rr)<-c("SO2","CO","O3","PM2.5","PM10","NO","NO2",
                       "as.factor(wday)2","as.factor(wday)3","as.factor(wday)4","as.factor(wday)5",
                       "as.numeric(TEMP)","as.numeric(RH)","as.numeric(yday)","as.numeric(WIND_SPEED)","as.numeric(RAIN)")
  return(round(muti_rr,4))
}

#urticaria EXAMPLE
#res_urticaria 是 單污染物的結果
min_urticaria_pv<-data.frame()
for(i in 1:7){#找出最小的p-value與滯後天數
  min_urticaria_pv[i,1]<-names(which.min(res_urticaria[[i]][,1]))
  min_urticaria_pv[i,2]<-res_urticaria[[i]][which.min(res_urticaria[[i]][,1]),1]
  min_urticaria_pv[i,3]<-res_urticaria[[i]][which.min(res_urticaria[[i]][,1]),2]
}
rownames(min_urticaria_pv)<-rownames(bb)
colnames(min_urticaria_pv)<-c("lag","p.pv","coeff")
min_urticaria_pv#從這表格篩選達顯著水準的空污與滯後天數
#建立新資料，包含不同空氣汙染物不同滯後天數與當天就診人次與共變數
mutidata1<-data.frame(SO2=lag1_daily$SO2[-1],CO=lag0_daily$CO[-1:-2],O3=lag1_daily$O3[-1],
                      PM2.5=lag2_daily$PM2.5,PM10=lag1_daily$PM10[-1],
                      NO=lag2_daily$NO,NO2=lag0_daily$NO2[-1:-2],lag2_daily[,-6:-12])

mutiair<-NULL
for(i in c(1,2,4,6,7)){#建立線性項
  mutiair<-paste(rownames(min_urticaria_pv)[i],"+",mutiair,sep="")
}

p<-names(lag0_daily)[3]#選取疾病名

#多變數公式生成
formula_urt<-paste(p,"~",mutiair,"+as.factor(wday)",'+s(as.numeric(TEMP),bs="cs",sp=3,k=4)+s(as.numeric(RH),bs="cs",sp=4,k=4)+s(as.numeric(yday),bs="cs",sp=4,k=4)+s(as.numeric(WIND_SPEED),bs="cs",sp=4,k=4)+s(as.numeric(RAIN),bs="cs",sp=4,k=4)',sep="")
mutires_urticaria<-gam(formula(formula_urt),data =mutidata1,family  = "poisson" )
summary(mutires_urticaria)#RESULT
rrcul(mutires_urticaria)#CI
