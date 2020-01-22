# Multiple Linear Regression

# Importing the dataset
dataset = read.csv('50_Startups.csv', header = TRUE)
dataset
# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


# Fitting Multiple Linear Regression to the Training set
regressor1 = lm(formula = Profit ~ .,
               data = training_set)

# Predicting the Test set results
y_pred = predict(regressor1, newdata = test_set)
summary(regressor1)
# Building the optimal model using Backward Elimination
regressor2 = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
               data = dataset)
summary(regressor2)

regressor3 = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
               data = dataset)
summary(regressor3)
regressor4 = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
               data = dataset)
summary(regressor4)
regressor5 = lm(formula = Profit ~ R.D.Spend,
               data = dataset)
summary(regressor5)

