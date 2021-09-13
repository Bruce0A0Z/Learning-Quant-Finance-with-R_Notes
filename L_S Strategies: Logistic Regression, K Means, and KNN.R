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





##################################
#                                #
#       K Means Clustering       #
#                                #
##################################

clusters<-3
set.seed(1)
norm_isdji<-norm_isdji[,-dim(norm_isdji)[2]]
norm_osdji<-norm_osdji[,-dim(norm_osdji)[2]]

model<-kmeans(norm_isdji,clusters)
model$cluster #it shows the relevant cluster number each piece of data belongs to.
model$center #shows the centers of each final clusters 1,2,and 3.
model$size #gives the size of each cluster

#K Means Clustering is unsupervised learning, its performance is calculated by:
#Ratio of the sum of squares within clusters to the total sum of squares.
model$tot.withinss
model$totss
(ratio<-model$tot.withinss/model$totss)

#for cluster = 3, the accuracy is ~49%. Let's discover how the accuracy changes with K
ratio_string<-NULL
for (k in 1:100) {
  model.test<-kmeans(norm_isdji,k)
  ratio_string[k]<-model.test$tot.withinss/model.test$totss
}
plot(ratio_string,type = "l")
#Because K Means is a minimization problem, the smaller the ratio the better
#And the above for loop showed: the more clusters, the lower ratio.
#However, there is the potential over-fitting problem.
#Q:How to find the optimal number of clusters?
#A:


#Now go to out-sample data:
#There is no predict formula for k-means since it's unsupervised:
dist_matrix<-matrix(NA,nrow(norm_osdji),3)
for (i in 1:nrow(norm_osdji)) {
  for (j in 1:nrow(model$center)) {
    dist_matrix[i,j]<-dist(rbind(as.array(as.numeric(norm_osdji[i,])),as.array(as.numeric(model$center[j,]))))
  }
}
#dist_matrix

label<-array(NA,nrow(dist_matrix)) #lable the out-sample data with the cluster they are allocated to.
for (i in 1:nrow(dist_matrix)) {
  label[i]<-which(dist_matrix[i,] == min(dist_matrix[i,]))
}
label #We can see that out-sample data were only classified into cluster 1 and 3, majority in 1...

#Total sum of square = sum square distance between centers + total within sum of squares
#ratio = withinss/totss = withinss/(withinss + centerss)
#1/ratio = 1 + centerss/withinss
#ratio = 1/(1 + centerss/withinss)
(centerss<-sum(dist(model$center)^2))
withinss<-0
for (i in 1:nrow(norm_osdji)) {
  withinss=withinss+dist_matrix[i,label[i]]^2
}
#withinss
(ratio_os<-1/(1 + centerss/withinss)) #ratio quite high, showing bad clustering results

#rerun the k mean algo for out-sample data
model.os<-kmeans(norm_osdji,3)
model.os$cluster
model.os$size
(ratio_alt<-model.os$tot.withinss/model.os$totss)

model$center
model.os$center

#Summary:
# we continued using the normalized in-sample and out-sample data from the Logistic Regression code above.

#Methodology:
#1. With the in-sample data, we use kmean() to cluster them into 3 groups, performance generally around 50%, which could be lowered by adding more clusters.
#2. With the out-sample data, we clustered them with regards to the existing clustering scheme of in-sample data, i.e. each out-sample data will be allocated to the group whose center is closest to them.

#Result: out-sample data were only allocated to group 1 and 3, and the ratio were quite high at ~97%.

#Analysis:
#1. Apply kmean() to out-sample data with 3 clusters, the ratio is only ~45%, showing a much better fitting.
#2. A glance into the centers of in-sample clusters and out-sample clusters shows that the centers for out-sample data are in general 0.7 higher than those of in-sample data, in the index of DJI.Close, other indexes similarly different.

#Proposal:
#1. The brute-force calculation of ratio when fitting in-sample model to out-sample data might be mistaken...resulting in too high a ratio?
#2. Assuming no problem with 1, when fitting in-sample model to out-sample data, we could consider increasing the centers' indexes by a certain amount.
#Trial of 2:
#a) estimate the increment with the average of the differences of indexes:
increment<-array(NA,ncol(model$center))
for (i in 1:ncol(model$center)) {
  a<-model.os$center[1,i] - model$center[3,i]
  b<-model.os$center[2,i] - model$center[1,i]
  c<-model.os$center[3,i] - model$center[2,i] #the comparison between 2 sets of center is on a ordered basis
  increment[i] = (a+b+c)/3
}
increment

model_is_proxy<-matrix(NA,dim(model$center)[1],dim(model$center)[2])
for (i in 1:dim(model$center)[1]) {
  model_is_proxy[i,]<-model$center[i,]+increment
}
#model_is_proxy

#b) repeat the above validation codes:
dist_matrix<-matrix(NA,nrow(norm_osdji),3)
for (i in 1:nrow(norm_osdji)) {
  for (j in 1:nrow(model$center)) {
    dist_matrix[i,j]<-dist(rbind(as.array(as.numeric(norm_osdji[i,])),as.array(as.numeric(model_is_proxy[j,]))))
  }
}
dist_matrix

label<-array(NA,nrow(dist_matrix)) #lable the out-sample data with the cluster they are allocated to.
for (i in 1:nrow(dist_matrix)) {
  label[i]<-which(dist_matrix[i,] == min(dist_matrix[i,]))
}
#label #now the clustering is more even with some data points in each group
table(label)

#Total sum of square = sum square distance between centers + total within sum of squares
#ratio = withinss/totss = withinss/(withinss + centerss)
#1/ratio = 1 + centerss/withinss
#ratio = 1/(1 + centerss/withinss)
(centerss<-sum(dist(model$center)^2))
withinss<-0
for (i in 1:nrow(norm_osdji)) {
  withinss=withinss+dist_matrix[i,label[i]]^2
}
#withinss
(ratio_os<-1/(1 + centerss/withinss)) #ratio quite high, maybe the brute ratio calculation is mistaken!

