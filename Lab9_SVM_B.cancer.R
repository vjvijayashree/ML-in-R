# SVM in R

library(ggplot2)
library(e1071)

# Dataset
fileURL <- "http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"
download.file(fileURL, destfile="breast-cancer-wisconsin.data", method="curl")
# read the data
df <- read.table("breast-cancer-wisconsin.data", na.strings = "?", sep=",")
str(df)
# Name the columns. 
# These names are displayed in the tree to facilitate semantic interpretation
df <- df [ ,-1]

#Removing columns with missing Values
sum (is.na(df))
colSums(sapply(df,is.na))
df$V7 <- NULL

df$V11 <- factor(df$V11, levels=c(2,4), labels=c("1", "2"))
# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(df$V11, SplitRatio = 0.7)
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)

# Default SVM Model
svm_model <- svm(V11~., data = training_set)
summary(svm_model)

# Confusion Matrix
pred = predict (svm_model, training_set)
pred
length(pred)
length(training_set$V11)
training_set$V11
cm = table(Predicted = pred, Actual = training_set$V11)
cm
1-sum(diag(cm))/sum(cm)


# SVM Linear model
svm_linear = svm (V11~., data = training_set,
             kernel = "linear")

summary (svm_linear)

# Confusion Matrix
pred = predict (model, iris)
cm = table(Predicted = pred, Actual = training_set$V11)
cm
1-sum(diag(cm))/sum(cm)

# SVM sigmoid model
svm_sigmoid = svm (V11~., data = training_set,
                  kernel = "sigmoid")
summary (svm_sigmoid)

# Confusion Matrix
pred = predict (svm_sigmoid, training_set)
levels(svm_sigmoid)
levels(training_set)
cm = table(Predicted = pred, Actual = training_set$V11)
cm
1-sum(diag(cm))/sum(cm)

# Model Tuning
set.seed(123)
# tune function tunes the hyperparameters of teh model using grid search method
tuned_model = tune(svm, V11~., data=training_set,
     ranges = list(epsilon = seq (0, 1, 0.1), cost = 2^(0:2)))
plot (tuned_model)
summary (tuned_model)

opt_model = tuned_model$best.model
summary(opt_model)

# Building teh best model
svm_best <- svm (V11~., data = training_set, epsilon = 0, cost = 1
                 )
summary(svm_best)


tuned_model = tune(svm, V11~., data=training_set,
                   ranges = list(epsilon = seq (0, 1, 0.1), cost = 2^(0:2)), kernel = c("radial", "linear", "sigmoid"))
plot (tuned_model)
summary (tuned_model)


