library(e1071)
library(caret)
library(lattice)
setwd("C:/Users/Vijaya Shree/Documents/R/datasets")
getwd()
#reading the data file
loans = read.csv('kiva_loans.csv', header=T)
dt<-loans[loans$activity,c(3,7,15,4)]
x<- 60/100
ds <- sample(nrow(dt),nrow(dt)*x)
train<-dt[ds,]
test<-dt[-ds,]

#model
Classifier<-naiveBayes(activity~.,data=train)

#Prediction
predBayes<-predict(Classifier, newdata = test)
test$prediction<-predBayes
#output
head(test,10)
#evaluation
cfm<-confusionMatrix(predBayes,test$activity)
cfm
bacc<-cfm$overall[1:6]
bacc
