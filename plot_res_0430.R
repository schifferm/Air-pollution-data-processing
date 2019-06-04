Sys.setlocale("LC_TIME", "English")
library(plotly)


p <- plot_ly(d, x = ~wt, y = ~hp, z = ~qsec,
             marker = list(color = ~mpg, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Weight'),
                      yaxis = list(title = 'Gross horsepower'),
                      zaxis = list(title = '1/4 mile time')),
         annotations = list(
           x = 1.13,
           y = 1.05,
           text = 'Miles/(US) gallon',
           xref = 'paper',
           yref = 'paper',
           showarrow = FALSE
         ))

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = api_create(p, filename="scatter3d-colorscale")
chart_link


data0<-lag0_daily
data0<-lag1_daily
data0<-lag2_daily
data0<-lag3_daily
data0<-lag4_daily
data0<-lag5_daily
data0<-lag6_daily
data0<-lag7_daily
data0<-data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),]
#[1] "date"      "wday"      "urticaria" "allergy"   "yday"     
#[6] "SO2"       "CO"        "O3"        "PM2.5"     "PM10"     
#[11] "NO"        "NO2"       "TEMP"      "RH"        "RAIN"     
#[16] "WS_HR" 
#no daily on the visit day: 
#PM2.5: relativerisk(RR)=1.01, p-value= 0.004;
j=3
i=9
pdf(paste(names(data0)[i]," and ",names(data0)[j],"on the visit day",".pdf",sep=""),family="GB1",width=12,height = 7) 
par(mar = c(5,5,3,5))
plot(y=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),i], type = "l", ylab = paste("daily mean of ",names(data0)[i],"(μg/m3)",sep = ""),
     x=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),1],
     main = "", xlab = "month",
     col = "blue")
par(new = TRUE)
plot(y=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),j], type = "l", xaxt = 'n', yaxt='n', ylab = '', xlab = '',
     x=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),1],
     lty = 2, col = "red")
axis(side = 4)
mtext(paste(names(data0)[j]), side = 4, line = 3) 
legend("topright", c(paste(names(data0)[i]), paste(names(data0)[j])),
       col = c("blue", "red"), lty = c(1, 2),lwd = 1)
mtext("PM2.5:relativerisk(RR)=1.01, p-value= 0.004", side = 3)
dev.off()

#PM10: RR=1.004, p-value=0.02;
j=3
i=10
pdf(paste(names(data0)[i]," and ",names(data0)[j],"on the visit day",".pdf",sep=""),family="GB1",width=12,height = 7) 
par(mar = c(5,5,3,5))
plot(y=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),i], type = "l", ylab = paste("daily mean of ",names(data0)[i],"(μg/m3)",sep = ""),
     x=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),1],
     main = "", xlab = "month",
     col = "blue")
par(new = TRUE)
plot(y=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),j], type = "l", xaxt = 'n', yaxt='n', ylab = '', xlab = '',
     x=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),1],
     lty = 2, col = "red")
axis(side = 4)
mtext(paste(names(data0)[j]), side = 4, line = 3) 
legend("topright", c(paste(names(data0)[i]), paste(names(data0)[j])),
       col = c("blue", "red"), lty = c(1, 2),lwd = 1)
mtext("PM10:relativerisk(RR)=1.004, p-value=0.02;", side = 3)
dev.off()

#the 2nd Lag days: 
data0<-lag1_daily
data0<-data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),]

#CO:RR=2.65, p-value=0.002;
j=3
i=7
pdf(paste(names(data0)[i]," and ",names(data0)[j],"2nd Lag days",".pdf",sep=""),family="GB1",width=12,height = 7) 
par(mar = c(5,5,3,5))
plot(y=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),i], type = "l", ylab = paste("daily mean of ",names(data0)[i],"(ppm)",sep = ""),
     x=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),1],
     main = "", xlab = "month",
     col = "blue")
par(new = TRUE)
plot(y=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),j], type = "l", xaxt = 'n', yaxt='n', ylab = '', xlab = '',
     x=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),1],
     lty = 2, col = "red")
axis(side = 4)
mtext(paste(names(data0)[j]), side = 4, line = 3) 
legend("topright", c(paste(names(data0)[i]), paste(names(data0)[j])),
       col = c("blue", "red"), lty = c(1, 2),lwd = 1)
mtext("CO:relativerisk(RR)=2.65, p-value=0.002;", side = 3)
dev.off()
#SO2: RR: 1.08, p-value&lt;0.001; 
i=6
pdf(paste(names(data0)[i]," and ",names(data0)[j],"2nd Lag days",".pdf",sep=""),family="GB1",width=12,height = 7) 
par(mar = c(5,5,3,5))
plot(y=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),i], type = "l", ylab = paste("daily mean of ",names(data0)[i],"(ppb)",sep = ""),
     x=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),1],
     main = "", xlab = "month",
     col = "blue")
par(new = TRUE)
plot(y=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),j], type = "l", xaxt = 'n', yaxt='n', ylab = '', xlab = '',
     x=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),1],
     lty = 2, col = "red")
axis(side = 4)
mtext(paste(names(data0)[j]), side = 4, line = 3) 
legend("topright", c(paste(names(data0)[i]), paste(names(data0)[j])),
       col = c("blue", "red"), lty = c(1, 2),lwd = 1)
mtext("SO2:relativerisk(RR)=1.08, p-value=0.001;", side = 3)
dev.off()
#PM2.5: RR=1.02, p-value&lt;0.001; 
i=9
pdf(paste(names(data0)[i]," and ",names(data0)[j],"2nd Lag days",".pdf",sep=""),family="GB1",width=12,height = 7) 
par(mar = c(5,5,3,5))
plot(y=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),i], type = "l", ylab = paste("daily mean of ",names(data0)[i],"(μg/m3)",sep = ""),
     x=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),1],
     main = "", xlab = "month",
     col = "blue")
par(new = TRUE)
plot(y=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),j], type = "l", xaxt = 'n', yaxt='n', ylab = '', xlab = '',
     x=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),1],
     lty = 2, col = "red")
axis(side = 4)
mtext(paste(names(data0)[j]), side = 4, line = 3) 
legend("topright", c(paste(names(data0)[i]), paste(names(data0)[j])),
       col = c("blue", "red"), lty = c(1, 2),lwd = 1)
mtext("PM2.5:relativerisk(RR)=1.02, p-value=0.01;", side = 3)
dev.off()
#PM10: RR=1.01, p-value&lt;0.001; 
i=10
pdf(paste(names(data0)[i]," and ",names(data0)[j],"2nd Lag days",".pdf",sep=""),family="GB1",width=12,height = 7) 
par(mar = c(5,5,3,5))
plot(y=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),i], type = "l", ylab = paste("daily mean of ",names(data0)[i],"(μg/m3)",sep = ""),
     x=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),1],
     main = "", xlab = "month",
     col = "blue")
par(new = TRUE)
plot(y=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),j], type = "l", xaxt = 'n', yaxt='n', ylab = '', xlab = '',
     x=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),1],
     lty = 2, col = "red")
axis(side = 4)
mtext(paste(names(data0)[j]), side = 4, line = 3) 
legend("topright", c(paste(names(data0)[i]), paste(names(data0)[j])),
       col = c("blue", "red"), lty = c(1, 2),lwd = 1)
mtext("PM10:relativerisk(RR)=1.01, p-value=0.001;", side = 3)
dev.off()
#NO2: RR=1.02, p-value=0.026;
i=12
pdf(paste(names(data0)[i]," and ",names(data0)[j],"2nd Lag days",".pdf",sep=""),family="GB1",width=12,height = 7) 
par(mar = c(5,5,3,5))
plot(y=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),i], type = "l", ylab = paste("daily mean of ",names(data0)[i],"(ppb)",sep = ""),
     x=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),1],
     main = "", xlab = "month",
     col = "blue")
par(new = TRUE)
plot(y=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),j], type = "l", xaxt = 'n', yaxt='n', ylab = '', xlab = '',
     x=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),1],
     lty = 2, col = "red")
axis(side = 4)
mtext(paste(names(data0)[j]), side = 4, line = 3) 
legend("topright", c(paste(names(data0)[i]), paste(names(data0)[j])),
       col = c("blue", "red"), lty = c(1, 2),lwd = 1)
mtext("NO2:relativerisk(RR)=1.02, p-value=0.026;", side = 3)
dev.off()
#3ed lag
data0<-lag2_daily
data0<-data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),]
#the 2nd Lag days: 
#the 3rd lag days: O3: RR=1.01, p-value=0.004).
i=8
pdf(paste(names(data0)[i]," and ",names(data0)[j],"3nd Lag days",".pdf",sep=""),family="GB1",width=12,height = 7) 
par(mar = c(5,5,3,5))
plot(y=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),i], type = "l", ylab = paste("daily mean of ",names(data0)[i],"(ppb)",sep = ""),
     x=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),1],
     main = "", xlab = "month",
     col = "blue")
par(new = TRUE)
plot(y=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),j], type = "l", xaxt = 'n', yaxt='n', ylab = '', xlab = '',
     x=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),1],
     lty = 2, col = "red")
axis(side = 4)
mtext(paste(names(data0)[j]), side = 4, line = 3) 
legend("topright", c(paste(names(data0)[i]), paste(names(data0)[j])),
       col = c("blue", "red"), lty = c(1, 2),lwd = 1)
mtext("O3:relativerisk(RR)=1.01, p-value=0.004;", side = 3)
dev.off()
#cov
i=13
i=14
pdf(paste(names(data0)[i]," and ",names(data0)[j],"on the visit day",".pdf",sep=""),family="GB1",width=12,height = 7) 
par(mar = c(5,5,3,5))
plot(y=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),i], type = "l", ylab = paste("daily mean of ",names(data0)[i],"(ppb)",sep = ""),
     x=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),1],
     main = "", xlab = "month",
     col = "blue")
par(new = TRUE)
plot(y=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),j], type = "l", xaxt = 'n', yaxt='n', ylab = '', xlab = '',
     x=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),1],
     lty = 2, col = "red")
plot(y=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),14], type = "l", xaxt = 'n', yaxt='n', ylab = '', xlab = '',
     x=data0[which(data0$date>as.Date("2016-12-31")&data0$date<as.Date("2018-01-01")),1],
     lty = 2, col = "red")
axis(side = 4)
mtext(paste(names(data0)[j]), side = 4, line = 3) 
legend("topright", c(paste(names(data0)[i]), paste(names(data0)[j])),
       col = c("blue", "red"), lty = c(1, 2),lwd = 1)
mtext("", side = 3)
dev.off()
