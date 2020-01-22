
# Aim of this lab is:  To build ridge, lasso regression models using cross validation
# To observe how lambda- the regularization cofficent, effects the error
# To observe the relationship between lambda and attribute coefficents in each of the models
# To compare the performance of the two models

# It uses Boston housing dataset to build a model to predict the price of houses

# Libraries Needed
#installed.packages()
#install.packages('mlbench')
#install.packages('glmnet')
#install.packages('tidyverse')
#install.packages('broom')
library(mlbench)    # Required for caret, Boston dataset
library(ggplot2)    # Plotting
library(caret)      # Sampling, cross validation
library(glmnet)     # regression models
library(tidyverse)
library(broom)     # for plotting coeffs


# Data - load data from mlbench
data("BostonHousing")
data <- BostonHousing
str(data)

# df <- data("BostonHousing")

# Data Partition
set.seed(222)
ind <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
train_data <- data[ind==1,]
test_data <- data[ind==2,]

# Go to help on glmnet. Note that glmnet uses matrices for x, y.
# converting dataframe into matrix

# Method 1
x <- data.matrix(train_data[,1:13])
y <- train_data$medv

#"""
# Method 2
x= model.matrix(medv~., train_data)[,-1]
# In the ablove expression where teh formula is used "medv~., train_data", 
# in matrix conversion medv goes into first column, this is then removed by removing the 1st column "-1"
y <- train_data$medv

# Ridge regression - glmnet parameter alpha=0 for ridge regression
# For numerical prediction choose family - gaussian, for classification family = binomial
# glmnet by defaut chooses 100 lambda values that are data dependent
l_ridge <- glmnet(x, y, family="gaussian", alpha=0)
plot(l_ridge, xvar = 'lambda', label=T)

# From the plot note that the coefficients reduce when labda increases. However all 13 attributes remain, none of the are dropped. 
# Now we need to find the best value for lambda. 
# This may be done using the builtin cross validation of cv.glmnet. Look up this function teh value of k for CV


cv_out_ridge = cv.glmnet(x, y, alpha =0)
plot (cv_out_ridge)
names(cv_out_ridge)
# two lambda values may be noted. 'lambda.min', 'lambda.1se'- lambda for error within 1 standard deviation
lambda_min <- cv_out_ridge$lambda.min
lambda_min
lambda_1se<- cv_out_ridge$lambda.1se
lambda_1se

# Now let us plot the ridge regression ouput once again
plot(l_ridge, xvar = 'lambda', label=T)
abline(v = (cv_out_ridge$lambda.1se), col = "red", lty = "dashed")
abline(v = (cv_out_ridge$lambda.min), col = "blue", lty = "dashed")

# Now set lambda to one of these values and build the model
l_ridge_final <- glmnet(x, y, family="gaussian", lambda = lambda_1se, alpha=0)
coef(l_ridge_final)
plot(coef(l_ridge_final))

# alternate plot of the coeffs
coef(l_ridge_final) %>%
  tidy() %>%
  filter(row != "(Intercept)") %>%
  top_n(25, wt = abs(value)) %>%
  ggplot(aes(value, reorder(row, value))) +
  geom_point() +
  ggtitle("Top 25 influential variables") +
  xlab("Coefficient") +
  ylab(NULL)

# Prediction with training set
p1 <- predict(l_ridge_final, x) #remeber to use x and not train_data
rmse_l_ridge_final <- sqrt(mean((train_data$medv-p1)^2))
rmse_l_ridge_final
# Convert test set attributes to a matrix, and predict target variable on test set and compute rmse


# Repeat the same with LASSO REGRESSION

cv_out_lasso = cv.glmnet(x, y, alpha = 1)
plot (cv_out_lasso)
names(cv_out_lasso)
# two lambda values may be noted. 'lambda.min', 'lambda.1se'- lambda for error within 1 standard deviation
lambda_min <- cv_out_lasso$lambda.min
lambda_min
lambda_1se<- cv_out_lasso$lambda.1se
lambda_1se

# Now let us plot the lasso regression ouput once again
l_lasso <- glmnet(x, y, family="gaussian", alpha =1)
plot(l_lasso, xvar = 'lambda', label=T)
abline(v = (cv_out_lasso$lambda.1se), col = "red", lty = "dashed")
abline(v = (cv_out_lasso$lambda.min), col = "blue", lty = "dashed")

# Now set lambda to one of these values and build the model
l_lasso_final <- glmnet(x, y, family="gaussian", lambda = lambda_1se, alpha=0)
coef(l_lasso_final)
plot(coef(l_lasso_final))
plot(coef(l_ridge_final))

# Note the number variables for varying lambda
# Note the coeffs of lasso regression
# Finally predict using train and test set 
# compare the two models

