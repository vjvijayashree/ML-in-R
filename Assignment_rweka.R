library(RWeka)
library(caret)
library(ggplot2)
library(lattice)
library(nnet)
library(neuralnet)
library(NeuralNetTools)
library(ROCR)
library(partykit)
setwd("C:/Users/Vijaya Shree/Documents/R/datasets")
getwd()
#reading the data file
loans = read.csv('kiva_loans.csv', header=T)
dim(loans)
str(loans)
head(loans)

# check for missing data in the data frame df
sum (is.na(loans))
colSums(sapply(loans,is.na))

#removing partner_id
colnames(loans)
loans[,c("partner_id")] <- list(NULL)
colnames(loans)

# check for missing data in the data frame after removing partner_id
sum (is.na(loans))
colSums(sapply(loans,is.na))

#########################################
dt<-loans[loans$loan_amount,c(3,7,15,4)]
x<- 60/100
rid<-sample(1:nrow(dt),x*nrow(dt))
train<-dt[rid,]
test<-dt[-rid,]
########################################
N <- make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")
modelwk<-N(activity~., data=dt,subset = rid, control=Weka_control(H='2', N=10, G=FALSE), options=NULL)
test$predwk<-predict(modelwk, newdata = test)
########################################
cfm<-confusionMatrix(test$predwk,test$activity)
wkacc<-cfm$overall[1:5]
wkacc
