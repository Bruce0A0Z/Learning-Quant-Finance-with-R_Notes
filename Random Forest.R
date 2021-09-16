##################################
#                                #
#      Random Forest Setup       #
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
directions[lagret>0.02]<-"Up"
directions[lagret< (-0.02)]<-"Down"
directions[lagret<0.02 & lagret>-0.02]<-"NoWhere"
tail(directions)

dji<-cbind(dji,m_avg10,m_avg20,std10,std20,rsi5,rsi14,macd12269,macd7205,bbands)
length(names(dji))

issd<-"2010-01-01"
ised<-"2014-12-31"
ossd<-"2015-01-01"
osed<-"2015-12-31"

isrow<-which(index(dji)>=issd & index(dji)<=ised)
osrow<-which(index(dji)>=ossd & index(dji)<=osed)

isdji<-dji[isrow,] #in-sample data
osdji<-dji[osrow,] #out-sample data
isdir<-directions[isrow]
osdir<-directions[osrow]

#Find mean and s.d. of each column of in-sample data using the following commands:
isme<-apply(isdji,2,mean)
isstd<-apply(isdji,2,sd)

#Set up the identity matrix of dimension equal to the in-sample data for normalization:
isidn<-matrix(1,dim(isdji)[1],dim(isdji)[2])
norm_isdji<-(isdji - t(isme*t(isidn)))/t(isstd*t(isidn)) #Normalization standardized data = (X - mean(X))/std(X)
tail(norm_isdji)

osidn<-matrix(1,dim(osdji)[1],dim(osdji)[2])
norm_osdji<-(osdji-t(isme*t(osidn)))/t(isstd*t(osidn))

class(isdir)
isdir<-as.factor(isdir)
osdir<-as.factor(osdir)


##################################
#                                #
#      Random Forest Module      #
#                                #
##################################

#install.packages("randomForest")
library(randomForest)
#library(help=randomForest)

model<-randomForest(norm_isdji,y=isdir,xtest = norm_osdji,ytest = osdir,ntree = 500)
print(model)
head(model$err.rate)

plot(model$err.rate[,1],type = "l",ylim = c(0.05,0.3),ylab="Error")
lines(model$err.rate[,2],col="red")
lines(model$err.rate[,3],col="green")
lines(model$err.rate[,4],col="blue")
legend(1,0.3,c("in-sample","Down","NoWhere","Up"),pch = 1,col = c("black","red","green","blue"))

#If you want to extract variables which help to control error,
#you can choose those variables depending upon MeanDecreaseGinni, which can be accessed using the following code:
value<-importance(model,type = 2)
head(value)
