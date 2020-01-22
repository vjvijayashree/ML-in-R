fileURL <- "http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"
download.file(fileURL, destfile="breast-cancer-wisconsin.data", method="curl")
# read the data
df <- read.table("breast-cancer-wisconsin.data", na.strings = "?", sep=",")
str(df)
# Name the columns. 
# These names are displayed in the tree to facilitate semantic interpretation

library(DataExplorer)
library(caret)

# Drop the index column
df <- df [ ,-1]
# Change labels
df$V11 <- factor(df$V11, levels=c(2,4), labels=c("1", "2"))

# Checking for missing values
sum(is.na(df))
colSums(is.na(df))
# Drop a column with many missing values
df$V7 <- NULL
#Scaling
#The scale transform calculates the standard deviation for an attribute and
#divides each value by that standard deviation.
#"""
df_scale <- df
df_scale [ ,1:8] = scale(df_scale[ , 1:8])
summary(df_scale)

preprocessParams <- preProcess(ds[,1:8], method=c("scale"))
# summarize transform parameters
print(preprocessParams)
# transform the dataset using the parameters
transformed <- predict(preprocessParams, ds[,1:8])
# summarize the transformed dataset
summary(transformed)

df_standardise <- df
preprocessParams <- preProcess(df_standardise[,1:8], method=c("center", "scale"))
# summarize transform parameters
print(preprocessParams)
# transform the dataset using the parameters
transformed <- predict(preprocessParams, df_standardise[,1:8])
# summarize the transformed dataset
summary(transformed)

#"""
#Observation: the function scale(..) is performing standardization! 
#That centering and scaling. Where all the values are centered to the mean (mean = 0)
#and scaled by standard deviation)

#For other variations of normalization, refer to:
#Ref: https://machinelearningmastery.com/pre-process-your-dataset-in-r/
#"""

