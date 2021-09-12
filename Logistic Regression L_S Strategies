#install.packages("quantmod")
#install the package quantmod
library("quantmod")

#load the package into the workspace, importing data into R from Yahoo, and only extract the closing price.
getSymbols("^DJI",src = "yahoo")
dji<-DJI[, "DJI.Close"]

#The input data to the logistic regression is constructed using different indicators,
#such as moving average, standard deviation, RSI, MACD, Bollinger Bands, and so on,
#which has some predictive power in market direction - up or down.
#The following commands construct the indicators:
m_avg10<-rollapply(dji,10,mean)
m_avg20<-rollapply(dji,20,mean)

std10<-rollapply(dji,10,sd)
std20<-rollapply(dji,20,sd)

rsi5<-RSI(dji,5,"SMA")
rsi14<-RSI(dji,14,"SMA")

macd12269<-MACD(dji,12,26,9,"SMA")
macd7205<-MACD(dji,7,20,5,"SMA")

bbands<-BBands(dji,20,"SMA",2)

#Create directional signal variables: UP(1) when the current price is greater than 20 days ago
#and DOWN(0) when the current price is less than 20 days previous price:
directions<-NULL
directions[dji>Lag(dji,20)]<-1
directions[dji<Lag(dji,20)]<-0

#Bind all columns consisting of price and indicators
dji<-cbind(dji,m_avg10,m_avg20,std10,std20,rsi5,rsi14,macd12269,macd7205,bbands,directions)
tail(dji)
dim(dji)

#Next: divide the data into 2 parts, in-sample and out-sample data.
#In-sample data: used for model building process
#Out-sample data: used for model evaluation purposes
#The following gives in-sample start & end, and out-sample start & end
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

dm<-dim(isdji)
norm_isdji[,dm[2]]<-directions[isrow] #don't want to standardize the direction variables!

formula<-paste("directions ~ .",sep = "")
model<-glm(formula,family = "binomial",norm_isdji)
summary(model)

pred<-predict(model,norm_isdji)
prob<-1/(1+exp(-(pred)))

par(mfrow=c(2,1))
plot(pred, type = "l")
plot(prob, type = "l")
head(prob)#to notice that these are predicted probability of stock price going up

#turn the prob into prediction signals, UP when prob>0.5, etc.
pred_directions<-NULL
pred_directions[prob>0.5]<-1
pred_directions[prob<=0.5]<-0

#Next, we need to check our model to see how much of our model has predicted UP as UP and DOWN as DOWN
#to do this, we use the "caret" package and the confusion matrix tool in it.
#The confusion matrix gives a matrix as an output whose diagonal elements are correctly predicted,
#and off-diagonals are errors or wrongly predicted. Thus we should aim to reduce off-diagonal elements.
#install.packages("caret")
#install.packages("e1071") #Log: the confusionMatrix function requires data and reference to be factors with the same levels but when we applied as.factor to both inputs, the warning message says "package e1071 is required".
library("caret")
library("e1071")
matrix<-confusionMatrix(as.factor(pred_directions),as.factor(directions[isrow]))
matrix
#the output matrix shows that we have got 94% accuracy and 362(correct UP)+819(correct DOWN)=1181 correct predictions out of 1258.

#Finally, we should test the model in the standardized out-sample data
#Standardization, but here we should use the mean and sd of in-sample data:
osidn<-matrix(1,dim(osdji)[1],dim(osdji)[2])
norm_osdji<-(osdji-t(isme*t(osidn)))/t(isstd*t(osidn))
norm_osdji[,dim(osdji)[2]]<-directions[osrow]

ospred<-predict(model,norm_osdji)
osprob<-1/(1+exp(-(ospred)))
plot(osprob,type="l")
ospred_directions<-NULL
ospred_directions[osprob>0.5]<-1
ospred_directions[osprob<=0.5]<-0
osmatrix<-confusionMatrix(as.factor(ospred_directions),as.factor(directions[osrow]))
osmatrix
#85% accuracy on the out-sample data

#To be studied:
# - meaning of RSI,MACD,SMA,Bollinger Band,etc.

