install.packages("caret", dependencies = TRUE)
install.packages("Hmisc")
install.packages("DataExplorer")
install.packages("e1071")
install.packages ("KlaR")
install.packages("rlang")
install.packages("stringi")
library (caret)
library (Hmisc)
library (DataExplorer)
library (e1071)
library (klaR)
library (ggplot2)
library (rlang)



#defining the URL of where dataset is located online
DatasetURL <- "http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"
# Downloading the dataset
download.file(DatasetURL, destfile="BreastCancer.data")
# reads the comma seperated data and making ? as missing values. 
originalDataSet <- read.table("BreastCancer.data", na.strings = "?", sep=",")


# checkging the structure, dimensions, headings and missing values. 
dim(originalDataSet)
str(originalDataSet)
head (originalDataSet)
colSums(sapply(originalDataSet,is.na))
sum (is.na(originalDataSet))

# creating a new dataset with without the ID column (first coloulmn). 
newDataSet <- originalDataSet[-1]

#labeling the coloumns
names(newDataSet) <- c("clumpThickness",
                      "uniformityCellSize",
                      "uniformityCellShape",
                      "marginalAdhesion",
                      "singleEpithelialCellSize",
                      "bareNuclei",
                      "blandChromatin",
                      "normalNucleoli",
                      "mitoses",
                      "class")

# checkging the structure, dimensions, headings and missing values. 
dim(newDataSet)
str(newDataSet)
head (newDataSet)
colSums(sapply(newDataSet,is.na))
sum (is.na(newDataSet))
summary(newDataSet)
# finding the distribution in the mentioned column/observation
prop.table (table (newDataSet$class)) 
#shows density for each variable in a plot
plot_density(newDataSet)
plot_density(originalDataSet)
#shows rows columns discrete_columns continuous_columns all_missing_columns total_missing_values total_observations memory_usage
plot_intro(newDataSet)
##Check correlations with correlations matrix 
plot_correlation(newDataSet)
#find missing values columns in a plot
plot_missing(newDataSet)



# convert class variable to a factor and replacing 2 with 1 and 4 with 2
newDataSet$class <- factor(newDataSet$class, levels=c(2,4), labels=c("1", "2")) 

#using Hmisc package imputing bareNucliei variable with mean. Originally around 2% of the bareNucliei variable had missing values.
mean(newDataSet$bareNuclei) # checking mean before imputing
newDataSet$bareNuclei <- as.integer(impute(newDataSet$bareNuclei, mean))  # replace missng values with mean of variable





#Partitioning the newDataSet by using 70% for training and 30% for testing.
set.seed(1234)
train <- sample(2, nrow(newDataSet), replace = TRUE, prob = c(0.7, 0.3))
trainSet <- newDataSet[train==1,]
testSet <- newDataSet[train==2,]

# finding the distribution in the mentioned column/observation
prop.table (table (trainSet$class))
prop.table (table (testSet$class))



###USING NAIVE BAYES MODEL###

#training Naive Bayes model using {caret} package
NBclassifierCaret = train(x= trainSet[,-10],y=trainSet$class, 'nb')
# Confusion matrix and a summary (training and test data) / using caret package
CarretNBTrain = predict(NBclassifierCaret,trainSet[,-10])
confusionMatrix(data = CarretNBTrain, trainSet$class)
CarretNBTest = predict(NBclassifierCaret,testSet[,-10])
confusionMatrix(data = CarretNBTest, testSet$class)

#training Naive Bayes model using {caret} package with 10 fold cross validation
NBclassifierCaretCV = train(x= newDataSet[,-10],y=newDataSet$class, 'nb', trControl = trainControl(method ='cv', number = 10))
CVtrainDataset = predict (NBclassifierCaretCV, newdata = newDataSet[,-10])
# Confusion matrix and a summary / using caret package
confusionMatrix(data = CVtrainDataset, newDataSet$class)


# training the Naive bayes model using {E1071} package
NBclassifierE1071 = naiveBayes(x = trainSet[,-10], y =trainSet$class, laplace = 0)
# Confusion matrix and a summary (training and test data) / using caret package
y_pred_train = predict (NBclassifierE1071, newdata = trainSet[,-10])
confusionMatrix(data = y_pred_train, trainSet$class)
y_pred_validation = predict (NBclassifierE1071, newdata = testSet[,-10])
confusionMatrix(data = y_pred_validation, testSet$class)

