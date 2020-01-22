# Example: http://www.learnbymarketing.com/tutorials/neural-networks-in-r-tutorial/

#install.packages("neuralnet")
library (neuralnet)
library(ggplot2)


# Dataset
fileURL <- "http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"
download.file(fileURL, destfile="breast-cancer-wisconsin.data", method="curl")
# read the data
df <- read.table("breast-cancer-wisconsin.data", na.strings = "?", sep=",")
str(df)

# These names are displayed in the tree to facilitate semantic interpretation
df <- df [ ,-1]

#Removing columns with missing Values
sum (is.na(df))
colSums(sapply(df,is.na))
df$V7 <- NULL

df$V11 <- factor(df$V11)

df1 <- df[,-9] #removing target variable since not used for normalization
#Min- Max Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
df1.norm <-as.data.frame(lapply(df1, normalize))
V11 <- df$V11
ds <- cbind(df1.norm, V11)
str(ds)

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
# This is not startified sampling
# You may repeat this experiment with stratified sampling and compare results
# You may also repeat the experiment with various NN structures
library(caTools)
set.seed(123)
split = sample.split(ds$V11, SplitRatio = 0.7)
training_set = subset(ds, split == TRUE)
test_set = subset(ds, split == FALSE)

library(neuralnet)
set.seed(333)
# ce - cross entropy
n <- neuralnet(V11~V2+V3+V4+V5+V6+V8+V9+V10, data = training_set,
               hidden = 5,
               err.fct = "ce",
               linear.output = FALSE)
plot(n)
summary(n)
n$result.matrix

# Prediction on training set
output_training <- compute(n, training_set[,-9])
output_training$net.result
pred_training <- output_training$net.result[,2]
pred_training
pred1 <- ifelse(pred_training > 0.5, 1, 0)
pred1
length(pred1)
cm_training <- table(pred1, training_set$V11)
cm_training
accuracy_training <- 1-sum(diag(cm_training))/sum(cm_training)


# Prediction on test set
output_test <- compute(n, test_set[,-9])
output_test$net.result
pred_test <- output_test$net.result[,2]
pred2 <- ifelse(pred_test > 0.5, 1, 0)
pred2
length(pred2)
cm_test <- table(pred2, test_set$V11)
cm_test
1-sum(diag(cm_test))/sum(cm_test)

# Repeat, two hidden layers-first with 2 neurons, second with 1 neuron
# repeat network traing 5 times - each training starts with a new initializations & computes the weights
# lifesign - to see full output
# Neural Networks

set.seed(333)
# ce - cross entropy
n <- neuralnet(V11~V2+V3+V4+V5+V6+V8+V9+V10, data = training_set, hidden = c(2,1), 
               err.fct = "ce",algorithm = "rprop+",linear.output = FALSE,stepmax = 1000000)

# Prediction on training set
output_training <- compute(n, training_set[,-9])
output_training$net.result
pred_training <- output_training$net.result[,2]
pred_training
pred1 <- ifelse(pred_training > 0.5, 1, 0)
pred1
length(pred1)
cm_training <- table(pred1, training_set$V11)
cm_training
accuracy_training <- 1-sum(diag(cm_training))/sum(cm_training)
accuracy_training

# Prediction on test set
output_test <- compute(n, test_set[,-9])
output_test$net.result
pred_test <- output_test$net.result[,2]
pred2 <- ifelse(pred_test > 0.5, 1, 0)
pred2
length(pred2)
cm_test <- table(pred2, test_set$V11)
cm_test
1-sum(diag(cm_test))/sum(cm_test)


