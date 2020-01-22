# Lab 7 Logistic Regression
# Objective 1 - Perform Logistic Regression with and without preproccessing and compare teh results.
# Objective 2 - Perform various data normalization operations using "caret " package and compare results
# Objective 3 - Compute sensitivity, specificity and AUC.  draw ROC curve for one of teh chosen result 



fileURL <- "http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"
download.file(fileURL, destfile="breast-cancer-wisconsin.data", method="curl")
# read the data
df <- read.table("breast-cancer-wisconsin.data", na.strings = "?", sep=",")
str(df)
# Name the columns. 
# These names are displayed in the tree to facilitate semantic interpretation

library(DataExplorer)
library(ggplot2)
library(data.table)

df <- df [ ,-1]

df$V11 <- factor(df$V11, levels=c(2,4), labels=c("1", "2"))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(df$V11, SplitRatio = 0.7)
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)

# Checking Class distribution
prop.table(table(df$V11))
prop.table(table(training_set$V11))
prop.table(table(test_set$V11))

# Building classifier
classifier = glm(V11 ~.,
                 training_set,
                 family = binomial)
summary(classifier)
# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', test_set[ ,-10] )
prob_pred
y_pred = ifelse(prob_pred > 0.5, 1, 0)
y_pred
cm = table(test_set$V11, y_pred)
cm

# Varation 1 - Run the above by removing the variable with missing values

# Checking for missing values
sum(is.na(df))
colSums(is.na(df))
# Drop a column with many missing values
df$V7 <- NULL


set.seed(123)
split = sample.split(df$V11, SplitRatio = 0.7)
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)

classifier = glm(V11 ~.,
                 training_set,
                 family = binomial)
summary(classifier)

# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', test_set[ ,-9] )
y_pred = ifelse(prob_pred > 0.5, 1, 0)
cm = table(test_set$V11, y_pred)
cm

# Q1: Closely examine cm and comment


# Q2: Variation 2 - Run the Expt by imputing the missing values. comment on this experiment

# Varaition 3 - Run the experiment after normalising the values

# scaling
# This instruction only scales the attributes while retaining the class variable as it is
training_set[ ,1:8] = scale(training_set[ , 1:8])
test_set[ ,1:8] = scale(test_set[ , 1:8])

classifier = glm(V11 ~.,
                 training_set,
                 family = binomial)
summary(classifier)
# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', test_set[ ,-9] )
y_pred = ifelse(prob_pred > 0.5, 1, 0)
cm = table(test_set$V11, y_pred)
cm


# Q3: Perform scaling with "preProcess_normalized" command in caret.
# Q4: Choose 2 different options of normalization and observe the output
# Q5: Compute sensitivity, specificity and AUC.  draw ROC curve for one of teh chosen result 
# Q6: Compare performance with Naive Bayes



