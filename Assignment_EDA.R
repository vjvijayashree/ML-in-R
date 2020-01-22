#install.packages('DataExplorer')
#install.packages("rpart.plot")
#install.packages("caret")
#install.packages("e1071")
library(DataExplorer)
library(dplyr)
library(data.table)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(caret)
library(lattice)
library(e1071)
library(EnvStats)
setwd("C:/Users/Vijaya Shree/Documents/R/datasets")
getwd()
#reading the data file
loans = read.csv('kiva_loans.csv', header=T)
dim(loans)
str(loans)
head(loans)
#rosnerTest(loans$loan_amount,k = 3,alpha = 0.05, warn = T)

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

#attribute loan_amount is the target variable
USD<-c(round(mean(loans$loan_amount),digits=2), round(mean(loans$funded_amount),digits = 2), round(max(loans$loan_amount)))
USD<-data.frame(USD, row.names = c("Loan Mean", "Funded Mean", "Max Loan"))
USD
#plot loan_amount
x<-loans$loan_amount
x<-x[x < max(x)]
plot(table(x), type = "h", xlim = c(25, 50000), ylab="Loans", xlab="USD")
plot(head(table(x),100), type = "h", ylab="Loans", xlab="USD", col="blue")
#analyse loan_amount
abt_loan<-data.frame(table(x))
abt_loan1<-abt_loan[c(40,80,120,160,200,240,280,320,360,399),]
names(abt_loan1)[1]<-"USD"
names(abt_loan1)[2]<-"Loans"
abt_loan1

#Activity vs Loan
act_loan<-table(loans$activity)
act_loan<-data.frame(head(sort(act_loan, decreasing = TRUE),10))
names(act_loan)[1]<-"Activity"
names(act_loan)[2]<-"Loans"
act_loan

#Sector vs Loan
sect_loan<-table(loans$sector)
sect_loan<-data.frame(head(sort(sect_loan, decreasing = TRUE),10))
names(sect_loan)[1]<-"Sector"
names(sect_loan)[2]<-"Loans"
sect_loan

#countries Vs Loan

#plot1
countries_loan<-table(loans$country)
countries_loan<-data.frame(head(sort(countries_loan, decreasing = TRUE),10))
countries_loan<-data.frame(countries_loan)
names(countries_loan)[1]<-"Countries"
names(countries_loan)[2]<-"Loans"
countries_loan
# Simple Bar Plot 
counts <- table(countries_loan)
barplot(counts, main="Data", 
        xlab="Number of loans",
        ylab ="data")
#ggplot(countries_loan,aes(x=Loans,y=Countries))+
#geom_bar(stat='identity', aes(fill=type), width=.5)
#plot2
h<-hist(loans$loan_amount,col="red",xlab="Loan amount", 
     main="Histogram of Loan amount")
xfit<-seq(min(x),max(x),length=5000) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)
#plot3
attach(loans)
plot(loans$loan_amount, loans$activity, main="Scatterplot Example", 
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)
# Add fit lines
abline(lm(mpg~wt), col="red") # regression line (y~x) 
lines(lowess(wt,mpg), col="blue") # lowess line (x,y)
loans_dt  <- as.data.table(loans)
nloans <- loans_dt[, .N, by=country]
nloans$N_z <- round((nloans$N - mean(nloans$N))/sd(nloans$N), 2)
nloans$type <- ifelse(nloans$N_z < 0, "below", "above")
nloans <- nloans[order(N_z),]
nloans$country <- factor(nloans$country, levels = nloans$country)
nloans <- tail(nloans, 50)
ggplot(nloans, aes(x=country, y=N_z, label=N_z)) +
  geom_bar(stat='identity', aes(fill=type), width=.5)  +
  scale_fill_manual(name="Number of loans",
                    labels = c("Above Average", "Below Average"),
                    values = c("above"="#6dff75", "below"="#feff7a")) +
  labs(subtitle="Normalised number of loans",   title= "Loans per country (Top 50)") + 
  coord_flip()
plot(x=loans$activity,y=loans$loan_amount,xlab="Activity",ylab="Loan Amount", main = "Activity vs. Loan Amount")
abline(lm(loans$activity ~ loans$loan_amount), col="Blue")

#decisionTree
dt<-loans[loans$activity,c(3,7,15,4)]
x<- 60/100
rid<-sample(1:nrow(dt),x*nrow(dt))
train<-dt[rid,]
test<-dt[-rid,]
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
