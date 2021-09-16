##################################
#                                #
#      Decision Tree Setup       #
#                                #
##################################

library("quantmod")
getSymbols("^DJI",src = "yahoo")
dji<-DJI[,"DJI.Close"]
head(dji)

m_avg10<-rollapply(dji,10,mean)
m_avg20<-rollapply(dji,20,mean)

std10<-rollapply(dji,10,sd)
std20<-rollapply(dji,20,sd)

rsi5<-RSI(dji,5,"SMA")
rsi14<-RSI(dji,14,"SMA")

macd12269<-MACD(dji,12,26,9,"SMA")
macd7205<-MACD(dji,7,20,5,"SMA")

bbands<-BBands(dji,20,"SMA",2)

directions<-NULL
lagret<-(dji-Lag(dji,20))/Lag(dji,20)
directions[lagret>0.02]<-1
directions[lagret< (-0.02)]<-(-1)
directions[lagret<0.02 & lagret>-0.02]<-0
tail(directions)

dji<-cbind(dji,m_avg10,m_avg20,std10,std20,rsi5,rsi14,macd12269,macd7205,bbands,directions)

issd<-"2010-01-01"
ised<-"2014-12-31"
ossd<-"2015-01-01"
osed<-"2015-12-31"

isrow<-which(index(dji)>=issd & index(dji)<=ised)
osrow<-which(index(dji)>=ossd & index(dji)<=osed)

isdji<-dji[isrow,] #in-sample data
osdji<-dji[osrow,] #out-sample data

#Find mean and s.d. of each column of in-sample data using the following commands:
isme<-apply(isdji,2,mean)
isstd<-apply(isdji,2,sd)

#Set up the identity matrix of dimension equal to the in-sample data for normalization:
isidn<-matrix(1,dim(isdji)[1],dim(isdji)[2])
norm_isdji<-(isdji - t(isme*t(isidn)))/t(isstd*t(isidn)) #Normalization standardized data = (X - mean(X))/std(X)
norm_isdji[,dim(norm_isdji)[2]]<-directions[isrow]
tail(norm_isdji)

osidn<-matrix(1,dim(osdji)[1],dim(osdji)[2])
norm_osdji<-(osdji-t(isme*t(osidn)))/t(isstd*t(osidn))
norm_osdji[,dim(norm_osdji)[2]]<-directions[osrow]


##################################
#                                #
#      Decision Tree Modules     #
#                                #
##################################
#install.packages("party")
library(party)

model<-ctree(directions~.,norm_isdji)
print(model)
plot(model)
summary(model)

dm<-dim(norm_osdji)
pred<-predict(model,norm_osdji[,1:(dm[2]-1)])
head(pred)
plot(pred)
#The plot of the prediction shows clearly 3 classes corresponding to 3 
