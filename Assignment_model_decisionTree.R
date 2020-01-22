library(rpart)
library(rpart.plot)
library(caret)
library(lattice)
library(e1071)
#seting working directory
setwd("C:/Users/Vijaya Shree/Documents/R/datasets")
#reading the data file
loans = read.csv('kiva_loans.csv', header=T)

#removing partner_id
loans[,c("partner_id")] <- list(NULL)


#decisionTree_dataPartition
dt<-loans[loans$activity,c(3,7,14,4)]
x<- 60/100
rid<-sample(1:nrow(dt),x*nrow(dt))
train<-dt[rid,]
test<-dt[-rid,]

#model
dt_model<-rpart(activity~.,train,method = "class")
prp(dt_model)
#prediction
pred_dt <- predict(dt_model, newdata = test[1:3])
hpre<-apply(pred_dt, 1, function(x) colnames(pred_dt)[which.max(x)])
test$prediction<-hpre
head(test,10)
#confusionMatrix
cfm<-confusionMatrix(as.factor(test$prediction),test$activity)
cfm
dacc<-cfm$overall[1:5]
dacc