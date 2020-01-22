# install.packages("neuralnet")
library (neuralnet)
# Data
fileURL <- "http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"
download.file(fileURL, destfile="breast-cancer-wisconsin.data", method="curl")
# read the data
df <- read.table("breast-cancer-wisconsin.data", na.strings = "?", sep=",")
str(df)
# Name the columns. 
# These names are displayed in the tree to facilitate semantic interpretation
df <- df[,-1]

# Checking for missing values
sum(is.na(df))
colSums(is.na(df))
# Drop a column with many missing values
df$V7 <- NULL

df$V11 <- factor(df$V11, levels=c(2,4), labels=c("1", "2"))
str(df)

# Min-Max Normalization
# For NN normalization is essential
df$V2 <- (df$V2 - min(df$V2))/(max(df$V2) - min(df$V2))
df$V3 <- (df$V3 - min(df$V3))/(max(df$V3) - min(df$V3))
df$V4 <- (df$V4 - min(df$V4))/(max(df$V4) - min(df$V4))
df$V5 <- (df$V5 - min(df$V5))/(max(df$V5) - min(df$V5))
df$V6 <- (df$V6 - min(df$V6))/(max(df$V6) - min(df$V6))
df$V8 <- (df$V8 - min(df$V8))/(max(df$V8) - min(df$V8))
df$V9 <- (df$V9 - min(df$V9))/(max(df$V9) - min(df$V9))
df$V10 <- (df$V10 - min(df$V10))/(max(df$V10) - min(df$V10))

str(df)

# Data Partition
set.seed(222)
ind <- sample(2, nrow(df), replace = TRUE, prob = c(0.7, 0.3))
training <- df[ind==1,]
testing <- df[ind==2,]

# Neural Networks
set.seed(333)
# ce - cross entropy
n <- neuralnet(V11~.,
               data = training,
               hidden = 5,
               err.fct = "ce",
               linear.output = FALSE)
plot(n)

# Repeat, two hidden layers-first with 2 neurons, second with 1 neuron
# repeat network traing 5 times - each training starts with a new initializations & computes the weights
# lifesign - to see full output
# default maxmum steps is 100000, it may converge sooner, with stepmax can specify the max steps.
n <- neuralnet(V11~.,
               data = training,
                hidden = c(2,1),
                err.fct = "ce",
                rep = 6,
                lifesign = "full",
                algorithm = "rprop+",
                linear.output = FALSE,
                stepmax = 100000)
plot(n)

# Prediction, choose the best rep in this calculation ex: 3 
output <- compute(n, training[-1,], rep = 3)
head(output$net.result)
head(training[1,])


# Prediction
# training[,-1] # Removing the response variable
output <- compute(n, training[-1,]) # Computes the response for the training data
head(output$net.result)
head(training[1,])



# Confusion Matrix & Misclassification Error - training data
output <- compute(n, training[-1,])
p1 <- output$net.result
pred1 <- ifelse(p1>0.5, 1, 0)
tab1 <- table(pred1, training$V11)
tab1
1-sum(diag(tab1))/sum(tab1)

# Confusion Matrix & Misclassification Error - testing data
output <- compute(n, testing[,-1])
p2 <- output$net.result
pred2 <- ifelse(p2>0.5, 1, 0)
tab2 <- table(pred2, testing$admit)
tab2
1-sum(diag(tab2))/sum(tab2)
