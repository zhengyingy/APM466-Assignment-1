library(readxl)
APM466_Assignmet_1_Data <- read_excel("APM466 Assignmet 1 Data.xlsx")
View(APM466_Assignmet_1_Data)   
library(jrvFinance)
Issuedates<-APM466_Assignmet_1_Data[,c(4)]
Issuedates<-as.Date(APM466_Assignmet_1_Data$ISSUEDATE,format='%m/%d/%Y') 
APM466_Assignmet_1_Data$ISSUEDATE<-Issuedates
Mature<-as.Date(APM466_Assignmet_1_Data$MATURITYDATE,format='%m/%d/%Y')  
APM466_Assignmet_1_Data$MATURITYDATE<-Mature

#4a)
a<-1
bond1ytm<-vector()
Bond1<-APM466_Assignmet_1_Data[1,c(6:15)]
for(i in Bond1){
  bond1ytm[a]<-bond.yield(settle="2015-04-13", mature="2020-09-01", coupon=0.75e-2, price=i)*100
  a<-a+1
}
a<-1
bond2ytm<-vector()
for(i in APM466_Assignmet_1_Data[2,c(6:15)]){
  bond2ytm[a]<-bond.yield(settle="2018-11-09", mature="2021-02-01", coupon=2.25e-2, price=i)*100
  a<-a+1
}
a<-1
bond3ytm<-vector()
for(i in APM466_Assignmet_1_Data[3,c(6:15)]){
  bond3ytm[a]<-bond.yield(settle="2019-05-06", mature="2021-08-01", coupon=1.5e-2, price=i)*100
  a<-a+1
}
a<-1
bond4ytm<-vector()
for(i in APM466_Assignmet_1_Data[4,c(6:15)]){
  bond4ytm[a]<-bond.yield(settle="2019-11-04", mature="2022-02-01", coupon=1.5e-2, price=i)*100
  a<-a+1
}
a<-1
bond5ytm<-vector()
for(i in APM466_Assignmet_1_Data[5,c(6:15)]){
  bond5ytm[a]<-bond.yield(settle="2011-08-02", mature="2022-06-01", coupon=2.75e-2, price=i)*100
  a<-a+1
}
a<-1
bond6ytm<-vector()
for(i in APM466_Assignmet_1_Data[6,c(6:15)]){
  bond6ytm[a]<-bond.yield(settle="2017-10-06", mature="2023-03-01", coupon=1.75e-2, price=i)*100
  a<-a+1
}
a<-1
bond7ytm<-vector()
for(i in APM466_Assignmet_1_Data[7,c(6:15)]){
  bond7ytm[a]<-bond.yield(settle="2012-07-30", mature="2023-06-01", coupon=1.5e-2, price=i)*100
  a<-a+1
}
a<-1
bond8ytm<-vector()
for(i in APM466_Assignmet_1_Data[8,c(6:15)]){
  bond8ytm[a]<-bond.yield(settle="2018-10-05", mature="2024-03-01", coupon=2.25e-2, price=i)*100
  a<-a+1
}
a<-1
bond9ytm<-vector()
for(i in APM466_Assignmet_1_Data[9,c(6:15)]){
  bond9ytm[a]<-bond.yield(settle="2019-04-05", mature="2024-09-01", coupon=1.5e-2, price=i)*100
  a<-a+1
}
a<-1
bond10ytm<-vector()
for(i in APM466_Assignmet_1_Data[10,c(6:15)]){
  bond10ytm[a]<-bond.yield(settle="2019-10-11", mature="2025-03-01", coupon=1.25e-2, price=i)*100
  a<-a+1
}

a<-round(bond1ytm,digits = 3)
b<-round(bond2ytm,digits = 3)
c<-round(bond3ytm,digits = 3)
d<-round(bond4ytm,digits = 3)
e<-round(bond5ytm,digits = 3)
f<-round(bond6ytm,digits = 3)
g<-round(bond7ytm,digits = 3)
h<-round(bond8ytm,digits = 3)
i<-round(bond9ytm,digits = 3)
j<-round(bond10ytm,digits = 3)

YTM<- matrix(c(a,b,c,d,e,f,g,h,i,j),nrow=10,byrow=FALSE)
colnames(YTM) <- c("r1/2","r1","r3/2","r2","r5/2","r3","r7/2","r4","r9/2","r5")
rownames(YTM) <- c("2020-01-02","2020-01-03","2020-01-06","2020-01-07","2020-01-08","2020-01-09","2020-01-10","2020-01-13","2020-01-14","2020-01-15")
YTM <- as.table(YTM)
YTM

plot_colours <-c("green", "yellow", "red", "blue", rgb(0.4,0.4,0.4),rgb(0.5,0.2,0.1), rgb(0.6,0.2,0), rgb(0.2,0.5,0.3), rgb(0.4,0.6,0.6), rgb(0.6,0.4,0.6))
plot(YTM[1,],type="l",xlim=c(0.5,10),ylim=c(0.7,2.5),xlab="Time(in 6 months)",ylab="yield(in%)",col="green")
for (i in c(1:10)){
  lines(YTM[i,],type="l",col=plot_colours[i])
}
title(main="Yield Curves",col.main="orange",font.main=3)
legend("topleft", legend=rownames(YTM),col=plot_colours, lty=1, cex=0.35)

#4b)
a<-1
r0.5<-vector()
for(i in APM466_Assignmet_1_Data[1,c(6:15)]){
    r0.5<-((-log(i/(0.5*0.75+100)))/0.5)
    print(r0.5)
}
print(r0.5)
r1 <-vector()
for(i in 6:15){
  cal_r1<-function(x){0.5*2.25*exp(-0.5*r0.5[i])+(0.5*2.25+100)*exp(-x)-data.frame(APM466_Assignmet_1_Data)[2,i]}
  r1<-uniroot(cal_r1,interval=c(-10,10))$root
  print(r1)
}

#Write the output of ri's in vector form

for(i in 6:15){
  cal_r1<-function(x){0.5*2.25*exp(-0.5*r0.5[i])+(0.5*2.25+100)*exp(-x)-APM466_Assignmet_1_Data[2,i]}
  r1<-uniroot(cal_r1,interval=c(-10,10))$root
  print(r1)
}
r1<-c(0,0,0,0,0,0.01683982,0.01653982,0.01633838,0.01643796,0.01664056,0.01674131,0.01714445,0.01704251,0.01714445,0.01694511)
for(i in 6:15){
  cal_r1.5<-function(x){(0.5*1.5*(exp(-0.5*r0.5[i])+exp(-1*r1[i]))+(0.5*1.5+100)*exp(-1.5*x)-data.frame(APM466_Assignmet_1_Data)[3,i])}
  r1.5<-uniroot(cal_r1.5,interval=c(-10,10))$root
  print(r1.5)
}
r1.5<-c(0,0,0,0,0,0.01672295,0.01652055,0.01631674,0.01652056,0.01658835,0.0167928,0.01692746,0.0170642,0.01692746,0.01679279)
for(i in 6:15){
  cal_r2<-function(x){0.5*1.5*(exp(-0.5*r0.5[i])+exp(-1*r1[i])+exp(-1.5*r1.5[i]))+(0.5*1.5+100)*exp(-2*x)-APM466_Assignmet_1_Data[4,i]}
  r2<-uniroot(cal_r2,interval=c(-10,10))$root
  print(r2)
}
r2<-c(0,0,0,0,0,0.01650037,0.01624702,0.0160439,0.0161445,0.01614373,0.01639855,0.01655013,0.01660067,0.01660143,0.01644983)
for(i in 6:15){
  cal_r2.5<-function(x){0.5*2.75*(exp(-0.5*r0.5[i])+exp(-1*r1[i])+exp(-1.5*r1.5[i])+exp(-2*r2[i]))+(0.5*2.75+100)*exp(-2.5*x)-APM466_Assignmet_1_Data[5,i]}
  r2.5<-uniroot(cal_r2.5,interval=c(-10,10))$root
  print(r2.5)
}
r2.5<-c(0,0,0,0,0,0.01704069,0.01680097,0.01668258,0.0168021,0.01684213,0.01708408,0.01707906,0.01732452,0.01728444,0.01712469)
for(i in 6:15){
  cal_r3<-function(x){0.5*1.75*(exp(-0.5*r0.5[i])+exp(-1*r1[i])+exp(-1.5*r1.5[i])+exp(-2*r2[i])+exp(-2.5*r2.5[i]))+(0.5*1.75+100)*exp(-3*x)-APM466_Assignmet_1_Data[6,i]}
  r3<-uniroot(cal_r3,interval=c(-10,10))$root
  print(r3)
}
r3<-c(0,0,0,0,0,0.01635173,0.01597584,0.01577148,0.01587246,0.01590623,0.01621382,0.01634994,0.01648609,0.01634815,0.01610917)
for(i in 6:15){
  cal_r3.5<-function(x){0.5*1.5*(exp(-0.5*r0.5[i])+exp(-1*r1[i])+exp(-1.5*r1.5[i])+exp(-2*r2[i])+exp(-2.5*r2.5[i])+exp(-3*r3[i]))+(0.5*1.5+100)*exp(-3.5*x)-APM466_Assignmet_1_Data[7,i]}
  r3.5<-uniroot(cal_r3.5,interval=c(-10,10))$root
  print(r3.5)
}
r3.5<-c(0,0,0,0,0,0.01647315,0.01614818,0.01597148,0.01608905,0.01605806,0.01629377,0.01632106,0.01658965,0.01644035,0.01623372)
for(i in 6:15){
  cal_r4<-function(x){0.5*2.25*(exp(-0.5*r0.5[i])+exp(-1*r1[i])+exp(-1.5*r1.5[i])+exp(-2*r2[i])+exp(-2.5*r2.5[i])+exp(-3*r3[i])+exp(-3.5*r3.5[i]))+(0.5*2.25+100)*exp(-4*x)-APM466_Assignmet_1_Data[8,i]}
  r4<-uniroot(cal_r4,interval=c(-10,10))$root
  print(r4)
}
r4<-c(0,0,0,0,0,0.01589742,0.01556489,0.0153073,0.01575235,0.01548667,0.0158747,0.01602983,0.01604992,0.01584122,0.01558413)
for(i in 6:15){
  cal_r4.5<-function(x){0.5*1.5*(exp(-0.5*r0.5[i])+exp(-1*r1[i])+exp(-1.5*r1.5[i])+exp(-2*r2[i])+exp(-2.5*r2.5[i])+exp(-3*r3[i])+exp(-3.5*r3.5[i])+exp(-4*r4[i]))+(0.5*1.5+100)*exp(-4.5*x)-APM466_Assignmet_1_Data[9,i]}
  r4.5<-uniroot(cal_r4.5,interval=c(-10,10))$root
  print(r4.5)
}
r4.5<-c(0,0,0,0,0,0.0179326,0.01739274,0.01658059,0.01700866,0.01667221,0.01729039,0.01719147,0.01711489,0.01728644,0.01702832)
for(i in 6:15){
  cal_r5<-function(x){0.5*1.25*(exp(-0.5*r0.5[i])+exp(-1*r1[i])+exp(-1.5*r1.5[i])+exp(-2*r2[i])+exp(-2.5*r2.5[i])+exp(-3*r3[i])+exp(-3.5*r3.5[i])+exp(-4*r4[i])+exp(-4.5*r4.5[i]))+(0.5*1.25+100)*exp(-5*x)-APM466_Assignmet_1_Data[9,i]}
  r5<-uniroot(cal_r5,interval=c(-10,10))$root
  print(r5)
}
r5<-c(0,0,0,0,0,0.01505526,0.01456584,0.01383988,0.01422158,0.01392209,0.01447446,0.01438577,0.01431825,0.01447149,0.01423973)

r0.5<-r0.5[r0.5!=0]
r1<-r1[r1!=0]
r1.5<-r1.5[r1.5!=0]
r2<-r2[r2!=0]
r2.5<-r2.5[r2.5!=0]
r3<-r3[r3!=0]
r3.5<-r3.5[r3.5!=0]
r4<-r4[r4!=0]
r4.5<-r4.5[r4.5!=0]
r5<-r5[r5!=0]
r0.50<-r0.5*100
r0.50<-round(r0.50,digits = 3)
r0.50
r1.0<-r1*100
r1.0<-round(r1.0,digits = 3)
r1.0
r1.50<-r1.5*100
r1.50<-round(r1.50,digits = 3)
r1.50
r2.0<-r2*100
r2.0<-round(r2.0,digits = 3)
r2.0
r2.50<-r2.5*100
r2.50<-round(r2.50,digits = 3)
r2.50
r3.0<-r3*100
r3.0<-round(r3.0,digits = 3)
r3.50<-r3.5*100
r3.50<-round(r3.50,digits = 3)
r4.0<-r4*100
r4.0<-round(r4.0,digits = 3)
r4.50<-r4.5*100
r4.50<-round(r4.50,digits = 3)
r5.0<-r5*100
r5.0<-round(r5.0,digits = 3)
Spot_Rate<- matrix(c(r0.50,r1.0,r1.50,r2.0,r2.50,r3.0,r3.50,r4.0,r4.50,r5.0),nrow=10,byrow=FALSE)
colnames(Spot_Rate) <- c("r1/2","r1","r3/2","r2","r5/2","r3","r7/2","r4","r9/2","r5")
rownames(Spot_Rate) <- c("2020-01-02","2020-01-03","2020-01-06","2020-01-07","2020-01-08","2020-01-09","2020-01-10","2020-01-13","2020-01-14","2020-01-15")
Spot_Rate <- as.table(Spot_Rate)
Spot_Rate

plot_colours <- c("green", "yellow", "red", "blue", rgb(0.4,0.4,0.4),rgb(0.5,0.2,0.1), rgb(0.6,0.2,0), rgb(0.2,0.5,0.3), rgb(0.4,0.6,0.6), rgb(0.6,0.4,0.6))
plot(Spot_Rate[1,],type="l",xlim=c(0.5,10),ylim=c(1.2,2.3),xlab="Time(in 6 months)",ylab="spot rate(in%)",col="green")
for (i in c(1:10)){
  lines(Spot_Rate[i,],type="l",col=plot_colours[i])
}
title(main="Spot Curves",col.main="Blue",font.main=3)
legend("topright", legend=rownames(Spot_Rate),col=plot_colours, lty=1, cex=0.35)

#4c)

#f12,f13,f14,f15
v = numeric(40)
mat1 = matrix(v,nrow=10,ncol=4)
for(i in 1:10){
  forward_year1<-c(0,0,0,0)
  for(j in 1:4){
    forward_year1[j]<-((j+1)*Spot_Rate[i,2+2*j]-Spot_Rate[i,2])/j
    mat1[i,j] = forward_year1[j]
  }
 print(forward_year1)
}
print(mat1)
#f23,f24,f25
v = numeric(30)
mat2=matrix(v,nrow=10,ncol=3)
for(i in 1:10){
  forward_year2<-c(0,0,0)
  for(j in 1:3){
    forward_year2[j]<-((j+2)*Spot_Rate[i,4+2*j]-Spot_Rate[i,4])/j
    mat2[i,j] = forward_year2[j]
  }
}
print(mat2)
#f34,f35
v = numeric(20)
mat3=matrix(v,nrow=10,ncol=2)
for(i in 1:10){
  forward_year3<-c(0,0)
  for(j in 1:2){
    forward_year3[j]<-((j+3)*Spot_Rate[i,6+2*j]-Spot_Rate[i,6])/j
    print(forward_year3)
    mat3[i,j] = forward_year3[j]
  }
}
print(mat3)
#f45
v = numeric(10)
mat4=matrix(v,nrow=10,ncol=1)
for(i in 1:10){
  forward_year4<-c(0)
  forward_year4[1]<-(5*Spot_Rate[i,10]-Spot_Rate[i,8])/j
  mat4[i,1] = forward_year4[1]
}
print(mat4)

Forward_rate<-cbind(mat1,mat2,mat3,mat4)
colnames(Forward_rate) <- c("f12","f13","f14","f15","f23","f24","f25","f34","f35","f45")
rownames(Forward_rate) <- c("2020-01-02","2020-01-03","2020-01-06","2020-01-07","2020-01-08","2020-01-09","2020-01-10","2020-01-13","2020-01-14","2020-01-15")
Forward_rate <- as.table(Forward_rate)
Forward_rate
plot_colours <- c("green", "yellow", "red", "blue", rgb(0.4,0.4,0.4),rgb(0.5,0.2,0.1), rgb(0.6,0.2,0), rgb(0.2,0.5,0.3), rgb(0.4,0.6,0.6), rgb(0.6,0.4,0.6))
plot(Forward_rate[1,],type="l",xlim=c(0.5,10),ylim=c(1.3,4.8),xlab="Time(in 6 months)",ylab="forward rate(in%)",col="green")
for (i in c(1:10)){
  lines(Forward_rate[i,],type="l",col=plot_colours[i])
}
title(main="Forward Curves",col.main="Purple",font.main=3)
legend("topleft", legend=rownames(Forward_rate),col=plot_colours, lty=1, cex=0.35)
