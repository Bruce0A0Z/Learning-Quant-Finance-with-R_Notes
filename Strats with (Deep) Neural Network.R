##################################
#                                #
#         Neural Network         #
#                                #
##################################

library("quantmod")
getSymbols("^DJI",src="yahoo") #need VPN to work

#Firstly, set the indicators to be considered in the NN:
dji<-DJI[,"DJI.Close"]
ret<-Delt(dji) #Delt calculates the k-period % difference of series
avg10<-rollapply(dji,10,mean)
avg20<-rollapply(dji,20,mean)
std10<-rollapply(dji,10,sd)
std20<-rollapply(dji,20,sd)
rsi5<-RSI(dji,5,"SMA")
rsi14<-RSI(dji,14,"SMA")
macd12269<-MACD(dji,12,26,9,"SMA")
macd7205<-MACD(dji,7,20,5,"SMA")
bbands<-BBands(dji,20,"SMA",2)

#Next, define UP(DOWN) if the return over the last 20 days is greater(less)
#than 2%(-2%); and NOWHERE if it is in between +/-2%.
directions<-data.frame(matrix(NA,dim(dji)[1],1))
lagret<-(dji-Lag(dji,20))/Lag(dji,20)
directions[lagret>0.02]<-"Up"
directions[lagret< (-0.02)]<-"Down"
directions[lagret<0.02 & lagret>-0.02]<-"NoWhere"
#dim(directions)

dji<-cbind(dji,avg10,avg20,std10,std20,rsi5,rsi14,macd12269,macd7205,bbands)
#dji

#Splitting the data set into train, validation and test sets:
train_sdate<-"2010-01-01"
train_edate<-"2013-12-31"
vali_sdate<-"2014-01-01"
vali_edate<-"2014-12-31"
test_sdate<-"2015-01-01"
test_edate<-"2015-12-31"

trainrow<-which(index(dji)>=train_sdate & index(dji)<=train_edate)
valirow<-which(index(dji)>=vali_sdate & index(dji)<=vali_edate)
testrow<-which(index(dji)>=test_sdate & index(dji)<=test_edate)

traindji<-dji[trainrow,]
validji<-dji[valirow,]
testdji<-dji[testrow,]

trainme<-apply(traindji,2,mean)
trainstd<-apply(traindji,2,sd)

trainidn<-matrix(1,dim(traindji)[1],dim(traindji)[2])
valiidn<-matrix(1,dim(validji)[1],dim(validji)[2])
testidn<-matrix(1,dim(testdji)[1],dim(testdji)[2])

norm_traindji<-(traindji-t(trainme*t(trainidn)))/t(trainstd*t(trainidn))
norm_validji<-(validji-t(trainme*t(valiidn)))/t(trainstd*t(valiidn))
norm_testdji<-(testdji-t(trainme*t(testidn)))/t(trainstd*t(testidn))

train_direction<-directions[trainrow,1]
vali_direction<-directions[valirow,1]
test_direction<-directions[testrow,1]

#Next, use the Neural Network package to construct our model
library(nnet)
set.seed(1)
model<-nnet(norm_traindji,class.ind(train_direction),size = 4,trace = F)
model
#Output of the above command says 15-4-3 which means that norm-traindji has
#15 columns, the size set is 4, and the class.indicator of train_dir has 3 type of outputs
#being UP, DOWN and NoWhere.

#Do the prediction on the validation set:
vali_pred<-predict(model,norm_validji)
head(vali_pred)
vali_pred_class<-data.frame(matrix(NA,dim(vali_pred)[1],1))
vali_pred_class[vali_pred[,"Down"]>0.5,1]<-"Down"
vali_pred_class[vali_pred[,"NoWhere"]>0.5,1]<-"NoWhere"
vali_pred_class[vali_pred[,"Up"]>0.5,1]<-"Up"
#vali_pred_class

library(caret)
matrix<-confusionMatrix(as.factor(vali_pred_class[,1]),as.factor(vali_direction))
matrix #matrix of validation result shows 88% accuracy

#Test the validation result on the test set:
test_pred<-predict(model,norm_testdji)
head(test_pred)
test_pred_class<-data.frame(matrix(NA,dim(test_pred)[1],1))
test_pred_class[test_pred[,"Down"]>0.5,1]<-"Down"
test_pred_class[test_pred[,"NoWhere"]>0.5,1]<-"NoWhere"
test_pred_class[test_pred[,"Up"]>0.5,1]<-"Up"

test_matrix<-confusionMatrix(as.factor(test_pred_class[,1]),as.factor(test_direction))
test_matrix #82% accuracy
#Comment: consistency on accuracy across validation and test set has shown its good generalization power


#Advancement: use the classes for signal generation. People buy when they anticipate UP direction and sell when they anticipate DOWN direction
signal<-ifelse(test_pred_class=="Up",1,ifelse(test_pred_class=="Down",-1,0))
ret<-ret[testrow]
#changing (test -> vali or train) can show the strategy of the other sets.
#changing the ifelse parameter to decide actions regarding particular signal will change the strategy.
#e.g. the above signal output says "if UP then buy 1, DOWN then sell 1, NoWhere buy/sell 0".
cost<-0
trade_ret<-ret*Lag(signal)-cost
#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
cumm_ret<-Return.cumulative(trade_ret)
annual_ret<-Return.annualized(trade_ret)

charts.PerformanceSummary(trade_ret)
#terrible strategy, but the book says generating profitable strategy is beyond the scope of this book...




##################################
#                                #
#      Deep Neural Network       #
#                                #
##################################

#install.packages("deepnet")
library(deepnet)
set.seed(1)
dn_model<-dbn.dnn.train(norm_traindji,class.ind(train_direction),hidden = c(3,4,6))
#in above we had 3 hidden layer structure with 3,4,and 6 in the hidden layer 1,2,and 3
#class.ind is used to convert 3 directions into column vector where each column represents 1 direction.

#to see accuracy over the validation dataset:
nn.predict(dn_model,norm_validji)
nn.test(dn_model,norm_validji,class.ind(vali_direction),t=0.4)

#Comment:we should choose t as per our requirement, to understand the impact of t on accuracy:
t_reaction<-NULL
for (i in 1:10) {
  t_reaction[i]<-nn.test(dn_model,norm_validji,class.ind(vali_direction),t=i/10)
  
}
plot(t_reaction,type = "l")
#We can tell that indeed t = 0.4 is the local maximum.
#For some weird reason, the nn.predict function returned same number for each day...and all of them are below 0.5.





##################################
#                                #
#      H2o for Deep Learning     #
#                                #
##################################

#H2o is another package which can be used for deep neural network learning.
#It is implemented in Java and can use multithreads and multinodes of the CPU;
#however, deepnet is implemented in R itself and uses only a single thread
#and doesn't have the flexibility to use multithreads and multinodes of the CPU.
#The following commands install and load it into the workspace:
install.packages("h2o")
library("h2o")
h2o.init() #H2o needs 64-bit Java

#try the following codes when your computer has 64-bit JAVA:

#data<-cbind(as.data.frame(norm_traindji),train_direction)
#class(norm_traindji)
#class(train_direction)

#datah2o<-as.h2o(data,"h2o")
#class(datah2o)
#dim(datah2o)
#model<-h2o.deeplearning(1:15,16,training_frame = datah2o,hidden = c(4,5,2,7))
#vali_pred<-predict(model,as.h2o(norm_validji,"h2o"))
#vali_pred<-as.data.frame(vali_pred)
#vali_pred_class<-data.frame(matrix(NA,dim(vali_pred)[1],1))
#the following follows from before - figure it out yourself.
