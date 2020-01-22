library(curl)
# Dataset
fileURL <- "http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"
download.file(fileURL, destfile="breast-cancer-wisconsin.data", method="curl")
# read the data
df <- read.table("breast-cancer-wisconsin.data", na.strings = "?", sep=",")
str(df)
# Name the columns. 
# These names are displayed in the tree to facilitate semantic interpretation

df <- df [ ,-1]
df$V11 <- factor(df$V11, levels=c(2,4), labels=c("1", "2"))

# Removing columns with missing Values
sum (is.na(df))
colSums(sapply(df,is.na))
df$V7 <- NULL

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(df$V11, SplitRatio = 0.7)
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)
str(training_set)
#install.packages("randomForest")
library(randomForest)
set.seed(345)
rf <- randomForest(V11~.,data = training_set )
print(rf)
attributes(rf)
p1 <- predict(rf, training_set)
p1
cm <- table (p1, training_set$V11)
cm
p2 <- predict(rf, test_set)
cm <- table(p2, test_set$V11)
cm
plot(rf)
# In the plot black solid line for overall OOB error and the colour lines, one for each class' error.
# Tuning mtry
library(caret)
str(training_set)
tuneRF(training_set[ ,-9], training_set$V11,
      stepFactor=0.5,
      plot = TRUE,
      ntreeTry = 400,
      trace = TRUE,
      improve = 0.05)

rf <- randomForest(V11~.,data = training_set,
                   ntreeTry = 400,
                   mtry=2,
                   importance = TRUE,
                   proximity = TRUE)
print(rf)

p1 <- predict(rf, training_set)
cm <- table(p1, training_set$V11)
cm
p2 <- predict(rf, test_set)
cm <- table(p2, test_set$V11)

# Number of nodes for trees
hist(treesize(rf),
     main = "No. of nodes for trees",
     col = "green")

varImpPlot(rf)
importance(rf)
varUsed(rf)


