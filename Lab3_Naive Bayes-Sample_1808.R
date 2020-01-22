# This is a very basic code. Please add comments, add new content into it


fileURL <- "http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"
download.file(fileURL, destfile="breast-cancer-wisconsin.data", method="curl")
# read the data
df <- read.table("breast-cancer-wisconsin.data", na.strings = "?", sep=",")
str(df)
# Name the columns. 
# These names are displayed in the tree to facilitate semantic interpretation
df <- df[ , -1]
ds <- df
names(ds) <- c("ClumpThickness",
                 "UniformityCellSize",
                 "UniformityCellShape",
                 "MarginalAdhesion",
                 "SingleEpithelialCellSize",
                 "BareNuclei",
                 "BlandChromatin",
                 "NormalNucleoli",
                 "Mitoses",
                 "Class")



prop.table(table(ds$Class))
corrTable <- cor(df[,c("V2","V3","V4","V5","V6","V7","V8","V9","V10")])
corrTable


df$V11 <- factor(df$V11, levels=c(2,4), labels=c("1", "2"))
set.seed(1234)
ind <- sample(2, nrow(df), replace=TRUE, prob=c(0.7, 0.3))
trainDf <- df[ind==1,]
validationDf <- df[ind==2,]

# Running Naive Bayes using e1071 library

library(e1071)

classifier = naiveBayes(x = trainDf[ ,-10],
                        y = trainDf$V11 )
# Using the classifier on training data to test the predictions
y_pred_train = predict(classifier, newdata = trainDf[ ,-10])
cm = table(trainDf$V11, y_pred_train)
cm
summary(classifier)
# Validating the classifier on the validation data
y_pred_validation = predict(classifier, newdata = validationDf[ ,-10])
cm = table(validationDf$V11, y_pred_validation)
cm

# Variation1 - To view raw probabilities
classifier = naiveBayes(x = trainDf[ ,-10],
                        y = trainDf$V11, laplace = 1 )

classifier = naiveBayes(x = trainDf[ ,-10],
                        y = trainDf$V11)
# Using the classifier on training data to test the predictions
y_pred_train_raw = predict (classifier, newdata = trainDf[ ,-10], type = "raw" )
y_pred_train_class = predict (classifier, newdata = trainDf[ ,-10], type = "class" )
y_pred_train_class
s = cbind(y_pred_train_raw, y_pred_train_class)

# Variation 2 - To apply Laplacian Smoothing

classifier = naiveBayes(x = trainDf[ ,-10],
                        y = trainDf$V11, laplace=1 )
y_pred_train_raw = predict (classifier, newdata = trainDf[ ,-10], type = "raw", threshold = 0.001, eps = 0)