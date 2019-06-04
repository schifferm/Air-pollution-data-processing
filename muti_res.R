library(mgcv)
library(xtable)
library(car)
library(dplyr)
#multivariable gam
#choose each air pollution lag day
#allergy
min_allergy_pv<-data.frame()
for(i in 1:7){
  min_allergy_pv[i,1]<-names(which.min(res_allergy[[i]][,1]))
  min_allergy_pv[i,2]<-res_allergy[[i]][which.min(res_allergy[[i]][,1]),1]
  min_allergy_pv[i,3]<-res_allergy[[i]][which.min(res_allergy[[i]][,1]),2]
}
rownames(min_allergy_pv)<-rownames(aa)
colnames(min_allergy_pv)<-c("lag","p.pv","coeff")
min_allergy_pv

mutidata<-data.frame(SO2=lag1_daily$SO2[-1],CO=lag2_daily$CO,O3=lag0_daily$O3[-1:-2],
                     PM2.5=lag2_daily$PM2.5,PM10=lag2_daily$PM10,
                     NO=lag0_daily$NO[-1:-2],NO2=lag0_daily$NO2[-1:-2],lag2_daily[,-6:-12])
mutiair<-NULL
for(i in 1:7){
  mutiair<-paste(rownames(min_allergy_pv)[i],"+",mutiair,sep="")
}

p<-names(lag1_daily)[4]

formula_all<-paste(p,"~",mutiair,"+as.factor(wday)",'+s(as.numeric(TEMP),bs="cs",sp=3,k=4)+s(as.numeric(RH),bs="cs",sp=4,k=4)+s(as.numeric(yday),bs="cs",sp=4,k=4)+s(as.numeric(WS_HR),bs="cs",sp=4,k=4)+s(as.numeric(RAIN),bs="cs",sp=4,k=4)',sep="")
mutires_allergy<-gam(formula(formula_all),data = mutidata ,family  = "poisson")
summary(mutires_allergy)

#urticaria
min_urticaria_pv<-data.frame()
for(i in 1:7){
  min_urticaria_pv[i,1]<-names(which.min(res_urticaria[[i]][,1]))
  min_urticaria_pv[i,2]<-res_urticaria[[i]][which.min(res_urticaria[[i]][,1]),1]
  min_urticaria_pv[i,3]<-res_urticaria[[i]][which.min(res_urticaria[[i]][,1]),2]
  }
rownames(min_urticaria_pv)<-rownames(bb)
colnames(min_urticaria_pv)<-c("lag","p.pv","coeff")
min_urticaria_pv
mutidata1<-data.frame(SO2=lag1_daily$SO2[-1],CO=lag0_daily$CO[-1:-2],O3=lag1_daily$O3[-1],
                     PM2.5=lag2_daily$PM2.5,PM10=lag1_daily$PM10[-1],
                     NO=lag2_daily$NO,NO2=lag0_daily$NO2[-1:-2],lag2_daily[,-6:-12])

mutiair<-NULL
for(i in c(1,2,4,6,7)){
  mutiair<-paste(rownames(min_urticaria_pv)[i],"+",mutiair,sep="")
}
p<-names(lag1_daily)[3]

formula_urt<-paste(p,"~",mutiair,"+as.factor(wday)",'+s(as.numeric(TEMP),bs="cs",sp=3,k=4)+s(as.numeric(RH),bs="cs",sp=4,k=4)+s(as.numeric(yday),bs="cs",sp=4,k=4)+s(as.numeric(WS_HR),bs="cs",sp=4,k=4)+s(as.numeric(RAIN),bs="cs",sp=4,k=4)',sep="")
mutires_urticaria<-gam(formula(formula_urt),data =mutidata1,family  = "poisson" )
summary(mutires_urticaria)
#allergy_M
min_allergy_pv_M<-data.frame()
for(i in 1:7){
  min_allergy_pv_M[i,1]<-names(which.min(res_allergy_M[[i]][,1]))
  min_allergy_pv_M[i,2]<-res_allergy_M[[i]][which.min(res_allergy_M[[i]][,1]),1]
  min_allergy_pv_M[i,3]<-res_allergy_M[[i]][which.min(res_allergy_M[[i]][,1]),2]
}
rownames(min_allergy_pv_M)<-rownames(aa)
colnames(min_allergy_pv_M)<-c("lag","p.pv","coeff")
min_allergy_pv_M

mutidata3<-data.frame(PM10=lag2_daily$PM10[-1],
                     NO2=lag5_daily$NO2,lag5_daily[,-6:-12])
mutiair<-NULL
for(i in c(5,7)){
  mutiair<-paste(rownames(min_allergy_pv_M)[i],"+",mutiair,sep="")
}

p<-names(lag1_daily)[19]

formula_all_M<-paste(p,"~",mutiair,"+as.factor(wday)",'+s(as.numeric(TEMP),bs="cs",sp=3,k=4)+s(as.numeric(RH),bs="cs",sp=4,k=4)+s(as.numeric(yday),bs="cs",sp=4,k=4)+s(as.numeric(WS_HR),bs="cs",sp=4,k=4)+s(as.numeric(RAIN),bs="cs",sp=4,k=4)',sep="")
mutires_allergy_M<-gam(formula(formula_all_M),data = mutidata3 ,family  = "poisson")
summary(mutires_allergy_M)

#allergy_F
min_allergy_pv_F<-data.frame()
for(i in 1:7){
  min_allergy_pv_F[i,1]<-names(which.min(res_allergy_F[[i]][,1]))
  min_allergy_pv_F[i,2]<-res_allergy_F[[i]][which.min(res_allergy_F[[i]][,1]),1]
  min_allergy_pv_F[i,3]<-res_allergy_F[[i]][which.min(res_allergy_F[[i]][,1]),2]
}
rownames(min_allergy_pv_F)<-rownames(aa)
colnames(min_allergy_pv_F)<-c("lag","p.pv","coeff")
min_allergy_pv_F
mutidata4<-data.frame(SO2=lag1_daily$SO2[-1],O3=lag0_daily$O3[-1:-2],
                      PM2.5=lag2_daily$PM2.5,PM10=lag2_daily$PM10,
                      NO=lag0_daily$NO[-1:-2],NO2=lag0_daily$NO2[-1:-2],
                      lag2_daily[,-6:-12])
mutiair<-NULL
for(i in c(1,3:7)){
  mutiair<-paste(rownames(min_allergy_pv_F)[i],"+",mutiair,sep="")
}

p<-names(lag1_daily)[20]

formula_all_F<-paste(p,"~",mutiair,"+as.factor(wday)",'+s(as.numeric(TEMP),bs="cs",sp=3,k=4)+s(as.numeric(RH),bs="cs",sp=4,k=4)+s(as.numeric(yday),bs="cs",sp=4,k=4)+s(as.numeric(WS_HR),bs="cs",sp=4,k=4)+s(as.numeric(RAIN),bs="cs",sp=4,k=4)',sep="")
mutires_allergy_F<-gam(formula(formula_all_F),data = mutidata4 ,family  = "poisson")
summary(mutires_allergy_F)

#urticaria_M
min_urticaria_pv_M<-data.frame()
for(i in 1:7){
  min_urticaria_pv_M[i,1]<-names(which.min(res_urticaria_M[[i]][,1]))
  min_urticaria_pv_M[i,2]<-res_urticaria_M[[i]][which.min(res_urticaria_M[[i]][,1]),1]
  min_urticaria_pv_M[i,3]<-res_urticaria_M[[i]][which.min(res_urticaria_M[[i]][,1]),2]
}
rownames(min_urticaria_pv_M)<-rownames(bb)
colnames(min_urticaria_pv_M)<-c("lag","p.pv","coeff")
min_urticaria_pv_M
mutidata5<-data.frame(CO=lag4_daily$CO,
                      PM10=lag1_daily$PM10[-1:-2],
                      NO=lag1_daily$NO[-1:-2],
                      NO2=lag1_daily$NO2[-1:-2],
                      lag4_daily[,-6:-12])

mutiair<-NULL
for(i in c(2,5:7)){
  mutiair<-paste(rownames(min_urticaria_pv_M)[i],"+",mutiair,sep="")
}
p<-names(lag1_daily)[17]

formula_urt_M<-paste(p,"~",mutiair,"+as.factor(wday)",'+s(as.numeric(TEMP),bs="cs",sp=3,k=4)+s(as.numeric(RH),bs="cs",sp=4,k=4)+s(as.numeric(yday),bs="cs",sp=4,k=4)+s(as.numeric(WS_HR),bs="cs",sp=4,k=4)+s(as.numeric(RAIN),bs="cs",sp=4,k=4)',sep="")
mutires_urticaria_M<-gam(formula(formula_urt_M),data =mutidata5,family  = "poisson" )
summary(mutires_urticaria_M)

#urticaria_F
min_urticaria_pv_F<-data.frame()
for(i in 1:7){
  min_urticaria_pv_F[i,1]<-names(which.min(res_urticaria_F[[i]][,1]))
  min_urticaria_pv_F[i,2]<-res_urticaria_F[[i]][which.min(res_urticaria_F[[i]][,1]),1]
  min_urticaria_pv_F[i,3]<-res_urticaria_F[[i]][which.min(res_urticaria_F[[i]][,1]),2]
}
rownames(min_urticaria_pv_F)<-rownames(bb)
colnames(min_urticaria_pv_F)<-c("lag","p.pv","coeff")
min_urticaria_pv_F
mutidata6<-data.frame(SO2=lag1_daily$SO2[-1:-2],
                      CO=lag3_daily$CO,
                      NO2=lag0_daily$NO2[-1:-3],
                      lag3_daily[,-6:-12])

mutiair<-NULL
for(i in c(1,2,7)){
  mutiair<-paste(rownames(min_urticaria_pv_F)[i],"+",mutiair,sep="")
}
p<-names(lag1_daily)[18]

formula_urt_F<-paste(p,"~",mutiair,"+as.factor(wday)",'+s(as.numeric(TEMP),bs="cs",sp=3,k=4)+s(as.numeric(RH),bs="cs",sp=4,k=4)+s(as.numeric(yday),bs="cs",sp=4,k=4)+s(as.numeric(WS_HR),bs="cs",sp=4,k=4)+s(as.numeric(RAIN),bs="cs",sp=4,k=4)',sep="")
mutires_urticaria_F<-gam(formula(formula_urt_F),data =mutidata6,family  = "poisson" )
summary(mutires_urticaria_F)

#urticaria_20
min_urticaria_pv_20<-data.frame()
for(i in 1:7){
  min_urticaria_pv_20[i,1]<-names(which.min(res_urticaria_20[[i]][,1]))
  min_urticaria_pv_20[i,2]<-res_urticaria_20[[i]][which.min(res_urticaria_20[[i]][,1]),1]
  min_urticaria_pv_20[i,3]<-res_urticaria_20[[i]][which.min(res_urticaria_20[[i]][,1]),2]
}
rownames(min_urticaria_pv_20)<-rownames(gg)
colnames(min_urticaria_pv_20)<-c("lag","p.pv","coeff")
min_urticaria_pv_20
mutidata7<-data.frame(PM2.5=lag2_daily$PM2.5,
                      NO2=lag0_daily$NO2[-1:-2],
                      lag2_daily[,-6:-12])

mutiair<-NULL
for(i in c(4,7)){
  mutiair<-paste(rownames(min_urticaria_pv_20)[i],"+",mutiair,sep="")
}
p<-names(lag1_daily)[21]

formula_urt_20<-paste(p,"~",mutiair,"+as.factor(wday)",'+s(as.numeric(TEMP),bs="cs",sp=3,k=4)+s(as.numeric(RH),bs="cs",sp=4,k=4)+s(as.numeric(yday),bs="cs",sp=4,k=4)+s(as.numeric(WS_HR),bs="cs",sp=4,k=4)+s(as.numeric(RAIN),bs="cs",sp=4,k=4)',sep="")
mutires_urticaria_20<-gam(formula(formula_urt_20),data =mutidata7,family  = ziP() )
summary(mutires_urticaria_20)

#urticaria_65
min_urticaria_pv_65<-data.frame()
for(i in 1:7){
  min_urticaria_pv_65[i,1]<-names(which.min(res_urticaria_65[[i]][,1]))
  min_urticaria_pv_65[i,2]<-res_urticaria_65[[i]][which.min(res_urticaria_65[[i]][,1]),1]
  min_urticaria_pv_65[i,3]<-res_urticaria_65[[i]][which.min(res_urticaria_65[[i]][,1]),2]
}
rownames(min_urticaria_pv_65)<-rownames(gg)
colnames(min_urticaria_pv_65)<-c("lag","p.pv","coeff")
min_urticaria_pv_65
mutidata8<-data.frame(SO2=lag1_daily$SO2[-1],
                      CO=lag0_daily$CO[-1:-2],
                      NO=lag2_daily$NO,
                      NO2=lag0_daily$NO2[-1:-2],
                      lag2_daily[,-6:-12])

mutiair<-NULL
for(i in c(1,2,6,7)){
  mutiair<-paste(rownames(min_urticaria_pv_65)[i],"+",mutiair,sep="")
}
p<-names(lag1_daily)[22]

formula_urt_65<-paste(p,"~",mutiair,"+as.factor(wday)",'+s(as.numeric(TEMP),bs="cs",sp=3,k=4)+s(as.numeric(RH),bs="cs",sp=4,k=4)+s(as.numeric(yday),bs="cs",sp=4,k=4)+s(as.numeric(WS_HR),bs="cs",sp=4,k=4)+s(as.numeric(RAIN),bs="cs",sp=4,k=4)',sep="")
mutires_urticaria_65<-gam(formula(formula_urt_65),data =mutidata8,family  = ziP() )
summary(mutires_urticaria_65)
mutires_urticaria_65$outer.info
thu65<-mutires_urticaria_65$family$getTheta()
mutires_urticaria_65_fix<-gam(formula(formula_urt_65),data =mutidata8,family  = ziP(theta = thu65) )
summary(mutires_urticaria_65_fix)


#allergy_20
min_allergy_pv_20<-data.frame()
for(i in 1:7){
  min_allergy_pv_20[i,1]<-names(which.min(res_allergy_20[[i]][,1]))
  min_allergy_pv_20[i,2]<-res_allergy_20[[i]][which.min(res_allergy_20[[i]][,1]),1]
  min_allergy_pv_20[i,3]<-res_allergy_20[[i]][which.min(res_allergy_20[[i]][,1]),2]
}
rownames(min_allergy_pv_20)<-rownames(gg)
colnames(min_allergy_pv_20)<-c("lag","p.pv","coeff")
min_allergy_pv_20
mutidata9<-data.frame(CO=lag2_daily$CO,
                      O3=lag0_daily$O3[-1:-2],
                      PM2.5=lag2_daily$PM2.5,
                      PM10=lag2_daily$PM10,
                      NO=lag0_daily$NO[-1:-2],
                      NO2=lag0_daily$NO2[-1:-2],
                      lag2_daily[,-6:-12])

mutiair<-NULL
for(i in c(2:7)){
  mutiair<-paste(rownames(min_allergy_pv_20)[i],"+",mutiair,sep="")
}
p<-names(lag1_daily)[23]

formula_urt_20<-paste(p,"~",mutiair,"+as.factor(wday)",'+s(as.numeric(TEMP),bs="cs",sp=3,k=4)+s(as.numeric(RH),bs="cs",sp=4,k=4)+s(as.numeric(yday),bs="cs",sp=4,k=4)+s(as.numeric(WS_HR),bs="cs",sp=4,k=4)+s(as.numeric(RAIN),bs="cs",sp=4,k=4)',sep="")
mutires_allergy_20<-gam(formula(formula_urt_20),data =mutidata9,family  = ziP() )
summary(mutires_allergy_20)

#allergy_65
min_allergy_pv_65<-data.frame()
for(i in 1:7){
  min_allergy_pv_65[i,1]<-names(which.min(res_allergy_65[[i]][,1]))
  min_allergy_pv_65[i,2]<-res_allergy_65[[i]][which.min(res_allergy_65[[i]][,1]),1]
  min_allergy_pv_65[i,3]<-res_allergy_65[[i]][which.min(res_allergy_65[[i]][,1]),2]
}
rownames(min_allergy_pv_65)<-rownames(gg)
colnames(min_allergy_pv_65)<-c("lag","p.pv","coeff")
min_allergy_pv_65
mutidata8<-data.frame(SO2=lag1_daily$SO2[-1:-3],
                      PM2.5=lag6_daily$PM2.5,
                      PM10=lag0_daily$PM10[-1:-4],
                      NO2=lag3_daily$NO2[-1],
                      lag6_daily[,-6:-12])
mutiair<-NULL
for(i in c(1,4,5,7)){
  mutiair<-paste(rownames(min_allergy_pv_65)[i],"+",mutiair,sep="")
}
p<-names(lag1_daily)[24]

formula_urt_65<-paste(p,"~",mutiair,"+as.factor(wday)",'+s(as.numeric(TEMP),bs="cs",sp=3,k=4)+s(as.numeric(RH),bs="cs",sp=4,k=4)+s(as.numeric(yday),bs="cs",sp=4,k=4)+s(as.numeric(WS_HR),bs="cs",sp=4,k=4)+s(as.numeric(RAIN),bs="cs",sp=4,k=4)',sep="")
mutires_allergy_65<-gam(formula(formula_urt_65),data =mutidata8,family  = ziP() )
summary(mutires_allergy_65)
pp<-summary(mutires_allergy_65)
pp
######################################
samvif <- function(mod){
  # mod is an mgcv object
  # this function calculates the variance inflation factors for GAM as no one else has written code to do it properly
  
  # this is used to summarise how well the GAM performed
  
  mod.sum <- summary(mod)
  s2 <- mod$sig2 # estimate of standard deviation of residuals
  X <- mod$model # data used to fit the model
  n <- nrow(X) # how many observations were used in fitting?
  v <- -1 # omit the intercept term, it can't inflate variance
  varbeta <- mod.sum$p.table[v,2]^2 # variance in estimates
  varXj <- apply(X=X[,v],MARGIN=2, var) # variance of all the explanatory variables
  VIF <- varbeta/(s2/(n-1)*1/varXj) # the variance inflation factor, obtained by rearranging
  # var(beta_j) = s^2/(n-1) * 1/var(X_j) * VIF_j
  
  VIF.df <- data.frame(variable=names(VIF),
                       vif=VIF, 
                       row.names=NULL)
  
  return(VIF.df)
}
######################################
samvif(mutires_allergy)
samvif(mutires_urticaria)

