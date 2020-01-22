library(nnet)
library(neuralnet)
library(NeuralNetTools)
library(caret)
library(lattice)
library(ggplot2)
setwd("C:/Users/Vijaya Shree/Documents/R/datasets")
getwd()
#reading the data file
loans = read.csv('kiva_loans.csv', header=T)
head(loans)
# check for missing data in the data frame
sum (is.na(loans))
colSums(sapply(loans,is.na))
#removing partner_id
colnames(loans)
loans[,c("partner_id")] <- list(NULL)
colnames(loans)
# check for missing data in the data frame after removing partner_id
sum (is.na(loans))
colSums(sapply(loans,is.na))
#################################################################
dt<-loans[loans$activity,c(3,7,15,4)]
x<- 60/100
rid<-sample(1:nrow(dt),x*nrow(dt))
train<-dt[rid,]
test<-dt[-rid,]
#################################################################
modelnn <- nnet(activity~.,data = dt,subset = rid, size=2, rang=0.1, decay=5e-4, maxit=100)
plotnet(modelnn)

#pediction
pred <- as.data.frame(predict(modelnn, newdata = test))
column_pred<-apply(pred, 1, function(x) colnames(pred)[which.max(x)])
test$prednn<-column_pred
#Evaluation
cfm<-confusionMatrix(as.factor(test$prednn),test$activity)
cfm
annacc<-cfm$overall[1:6]
annacc
