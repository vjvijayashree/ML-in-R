# Read dataset
# data(iris)
# df = iris
df = read.csv(file.choose(), header=TRUE)
# df = read.csv('Iris.csv', header = T)

# dg <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"), header = FALSE) 
# View dataset
dim(df)
str(df)
head(df)
# check for missing data in the data frame df
sum (is.na(df))
colSums(sapply(df,is.na))


# split dataset
# Stratified sampling
library(caTools)
split = sample.split(df$Species, SplitRatio = 0.75)
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)

# Checking if there is any bias in sampling
prop.table(table(training_set$Species))
prop.table(table(test_set$Species))

# random sampling
set.seed(123)
ind <- sample(2, nrow(df), replace = T, prob = c(0.75, 0.25))
train <- df[ind==1,]
test <- df[ind==2,]
prop.table(table(train$Species))
prop.table(table(test$Species))

library(dplyr)
# drop a column
df<- select(df, -Id)
# change col names to lowercase
ds <- df
colnames(ds) <- tolower(colnames(ds))
