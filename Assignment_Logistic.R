setwd("C:/Users/Vijaya Shree/Documents/R/datasets")
getwd()
#reading the data file
loans = read.csv('kiva_loans.csv', header=T)
dt<-loans[,c(3,7,15,4)]
dt$actnum<- as.numeric(dt$activity)
dt$ctnum<- as.numeric(dt$country_code)
data<-dt[,c(1,3,5,6)]
names(data)[3]<-"activity"
names(data)[4]<-"country_code"

#remove_partnerID
loans[,c("partner_id")] <- list(NULL)
dt<-loans[,c(3,7,15,4)]

dt$actnum<- as.numeric(dt$activity)
dt$ctnum<- as.numeric(dt$country_code)
data<-dt[,c(1,3,5,6)]
names(data)[3]<-"activity"
names(data)[4]<-"country_code"

x<- 60/100
rid<-sample(1:nrow(data),x*nrow(data))
train<-data[rid,]
test<-data[-rid,]

fit<-glm(loan_amount ~ ., data = train)
summary(fit)
