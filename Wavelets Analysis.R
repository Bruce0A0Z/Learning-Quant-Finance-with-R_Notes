#install.packages("wavelets")
#help(package = "wavelets") #for exploring the components within the package.

library(wavelets)

#set the trajectory for local data.frame to help read-in later.
#to be changed for use elsewhere.
getwd()->"C:/Users/huawei/Desktop/Summer Learning/R/Wavelet Analysis"
setwd("C:/Users/huawei/Desktop/Summer Learning/R/Wavelet Analysis")
DJI<-read.csv(file = "dji.csv")
#DJI downloaded from https://www.wsj.com/market-data/quotes/index/DJIA/historical-prices.

head(DJI)
class(DJI)
nrow(DJI) #there are 1514 rows in DJI: Dow Jones data from 9/9/2015 to 9/9/2021

DJI<-DJI[,5] #only take the closing price each day.
plot(DJI)
Diff<-diff(DJI)
plot(Diff)

DJI<- as.ts (DJI)  
model<-dwt(DJI,filter = "la8",n.levels = 3)
model

model@W #to extract wavelets coefficients
model@W$W1 #to extract first level of wavelets coefficients

model@V #to extract scaling coefficients
model@V$V1 #to extract first level of scaling coefficients

plot(model)


#Discrete Wavelet Transformation function for the haar filter
model<-dwt(DJI,filter = "haar",n.levels = 3)
plot(model)

#inverse discrete Wavelet Transformation
imodel<-idwt(model,fast = TRUE)
plot(imodel)

#Multi-Resolution Analysis (MRA)
model<-mra(DJI, filter = "la8",n.levels = 3)
#model

#For analysis of market data, Maximal Overlap Discrete Wavelet Transformation is preferred.
model<-modwt(DJI,filter = "la8",n.levels = 5)
plot.modwt(model)
