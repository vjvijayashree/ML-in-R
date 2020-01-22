# Decision Tree Classification on Breast cancer dataset
# Downloading the file
#install.packages("party")
fileURL <- "http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"
download.file(fileURL, destfile="breast-cancer-wisconsin.data", method="curl")
# read the data
data <- read.table("breast-cancer-wisconsin.data", na.strings = "?", sep=",")
str(data)

# Remove ID column, col = 1
data <- data[,-1]

# Name the columns. 
# These names are displayed in the tree to facilitate semantic interpretation

names(data) <- c("ClumpThickness",
                 "UniformityCellSize",
                 "UniformityCellShape",
                 "MarginalAdhesion",
                 "SingleEpithelialCellSize",
                 "BareNuclei",
                 "BlandChromatin",
                 "NormalNucleoli",
                 "Mitoses",
                 "Class")

# Numerical values in the response variable are converted to labels

data$Class <- factor(data$Class, levels=c(2,4), labels=c("benign", "malignant"))

print(summary(data))

#"""
#Note that there are 16 missing values in BareNuclei
#Later you will see that there is no imputation of these missing values. 
#Investigate how decision trees handle missing values
#Read rpart documentation from this.
#This link has some extra information: 
#https://stats.stackexchange.com/questions/96025/how-do-decision-tree-learning-algorithms-deal-with-missing-values-under-the-hoo
#"""

# Dividing the dataset into training and validation sets. There are many ways to do this.
# Alternate method is also listed here.

set.seed(123)
ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.7, 0.3))
trainData <- data[ind==1,]
validationData <- data[ind==2,]
table(trainData$Class)

#"""
# Alternate method 
#set.seed(123)
#split = sample.split(data$Class, SplitRatio = 0.7)
# Create training and testing sets
#dataTrain = subset(quality, split == TRUE)
#dataTest = subset(quality, split == FALSE)
#"""
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("party")

library(rpart)
library(rpart.plot)
library(party)

# Can generate different types of trees with rpart
# Default split is with Gini index
tree = rpart(Class ~ ., data=trainData, method="class")
print(tree)
prp(tree)
prp (tree, type = 3)
rpart.plot(tree, extra = 104, nn = TRUE)
# Split with entropy information
entTree = rpart(Class ~ ., data=trainData, method="class", parms=list(split="information"))
prp(tree)
prp(entTree)
library(rpart.plot)

plotcp(tree)

# Here we use tree with parameter settings.
# This code generates the tree with training data
tree_with_params = rpart(Class ~ ., data=trainData, method="class", minsplit = 1, minbucket = 10, cp = -1)
prp (tree_with_params)
print(tree_with_params)
summary(tree_with_params)
plot(tree_with_params)
text(tree_with_params)
plotcp(tree_with_params)
# Now we predict and evaluate the performance of the trained tree model 
Predict = predict(tree_with_params, validationData)
# Now examine the values of Predict. These are the class probabilities
Predict

#"""
# pred <= predict (mymodel, dataset, type = 'prob')
# To produce classes only, without the probabilities, run the next command.
# By default threshold is set at 0.5 to produce the classes
#"""

Predict = predict(tree_with_params, validationData, type = "class")
Predict


# Producing confusion matrix

Confusion_matrix = table(Predict, validationData$Class)
print(Confusion_matrix)


# ROC curve - Evaluation matrix
#install.packages("ROCR")
library(ROCR)
#install.packages("gplots")

# To draw ROC we need to predict the prob values. So we run predict again
# Note that PredictROC is same as Predict with "type = prob"

PredictROC = predict(tree_with_params, validationData)
PredictROC
PredictROC[,2]

pred = prediction(PredictROC[,2], validationData$Class)
perf = performance(pred, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve

auc = as.numeric(performance(pred, "auc")@y.values)
auc = round(auc, 3)
auc

