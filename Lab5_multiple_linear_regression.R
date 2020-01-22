# Multiple Linear Regression
getwd()
# Importing the dataset
setwd("C:/Users/Vijaya Shree/Documents/R/datasets")
dataset = read.csv('kiva_loans.csv')
loans=dataset
summary(dataset)
# install.packages('caTools')
library(caTools)
dt<-loans[loans$loan_amount,c(3,7,15,4)]
x<- 60/100
rid<-sample(1:nrow(dt),x*nrow(dt))
train<-dt[rid,]
test<-dt[-rid,]

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = loan_amount~ .,
               data = train)
summary(regressor)
regressor1 = lm(formula = loan_amount~ .,
               data = test)
summary(regressor1)


# Predicting the Test set results
y1 = predict(regressor, data = train)
y_pred = predict(regressor1, data = test)


# RMSE on training set
sqrt(mean((train$loan_amount-y1)^2))

# RMSE on test set
sqrt(mean((test$loan_amount-y_pred)^2))

# Question - Why is the RMSE on test set more than RMSE on training set?

