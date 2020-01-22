library(caTools)
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

##################################
dt<-loans[loans$loan_amount,c(3,7,15,4)]
x<- 60/100
rid<-sample(1:nrow(dt),x*nrow(dt))
train<-dt[rid,]
test<-dt[-rid,]
##################################

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = loan_amount~ .,
               data = train)
summary(regressor)
regressor1 = lm(formula = loan_amount~ .,
               data = test)
summary(regressor)



# Predicting the Test set results
y1 = predict(regressor, data = train)
y_pred = predict(regressor1, data = test)

# RMSE on training set
sqrt(mean((train$loan_amount-y1)^2))

# RMSE on test set
sqrt(mean((test$loan_amount-y_pred)^2))

