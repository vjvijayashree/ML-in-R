# This lab includes application of Regularization, Cross Validation and Grid Search to a linear regression model
# It uses Boston housing dataset to build a model to predict the price of houses

# Libraries Needed
install.packages()
library(caret)
library(glmnet)
library(mlbench) #  Boston Housing Datset
library(psych)   # To view correlation

# Data - load data from mlbench
data("BostonHousing")
data <- BostonHousing
str(data)



# Data Partition
set.seed(123)
ind <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
train_data <- data[ind==1,]
test_data <- data[ind==2,]


# Firt a basic Linear Regression Model
lm1 <- lm(medv ~., train_data) 
summary(lm1)

# Select only some of the attributes and build Linear Regression Model
lm2 <- lm(medv ~ crim+zn+nox+rm+age+dis+rad+
           tax+ptratio+b+lstat, train_data)
summary(lm2)



# Custom Control Parameters for cross validation
custom <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 5,
                       verboseIter = T)


# Fitl a Linear Model with Cross Validation
set.seed(1234)
lm_cv <- train(medv ~ .,
            train_data,
            method = 'lm',
            trControl = custom)
summary(lm_cv)

# Results
lm_cv$results
plot(lm_cv$finalModel)


# Ridge Regression
set.seed(1234)

ridge <- train(medv ~.,
               train_data,
               method = 'glmnet',
               tuneGrid = expand.grid(alpha = 0,
                                      lambda = seq(0.1, 1, length = 10)),
               trControl = custom)

# Plot Results
plot(ridge)
print(ridge)
plot(ridge$finalModel, xvar = "lambda", label = T) 
# In the above after cross validation, finalModel is set with best parameters. 
# This finalModel is then used to plot variation is attributes as a function of lambda
plot(ridge$finalModel, xvar = 'dev', label=T)
lridge=ridge$finalModel
lridge$tuneValue

# Lasso Regression
set.seed(1234)
lasso <- train(medv ~.,
               train_data,
               method = 'glmnet',
               tuneGrid = expand.grid(alpha = 1,
                                      lambda = seq(0.001, 1, length = 5)),
               trControl = custom)

# Plot Results
plot(lasso)
plot(lasso$finalModel, xvar = 'lambda', label=T)

# Elastic Net Regression
set.seed(1234)
en <- train(medv ~.,
            train_data,
            method = 'glmnet',
            tuneGrid = expand.grid(alpha = seq (0, 1, length = 10) ,
                                   lambda = seq(0.001, 1, length = 5)),
            trControl = custom)

# Plot Results
plot(en)
plot(en$finalModel, xvar = 'lambda', label=T)
plot(varImp(en))

# Compare Models
model_list <- list(LinearModel= lm_cv, Ridge = ridge, Lasso = lasso, Elasticnet = en)
res <- resamples(model_list)
summary(res)

# Best Model
en$bestTune
best <- en$finalModel
coef(best, s = en$bestTune$lambda)
print(en)


# Prediction
p1 <- predict(en, train_data)
sqrt(mean((train_data$medv-p1)^2))

p2 <- predict(en, test_data)
sqrt(mean((test_data$medv-p2)^2))

