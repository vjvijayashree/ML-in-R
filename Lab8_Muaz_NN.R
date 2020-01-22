#Abdulla Muaz
#install.packages("neuralnet")
library (neuralnet)
# Data
fileURL <- "http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"
download.file(fileURL, destfile="breast-cancer-wisconsin.data", method="curl")
# read the data
dataset <- read.table("breast-cancer-wisconsin.data", na.strings = "?", sep=",")
str(dataset)

dataset = dataset[2:11]   

drops <- c("V7")
dataset = dataset[ , !(names(dataset) %in% drops)]


# Feature Scaling
dataset[,-9] = scale(dataset[,-9])

# Encoding the target feature as factor
dataset$V11 = factor(dataset$V11, levels = c(2, 4), labels=c("1","2"))


# Data Partition
library(caTools)
set.seed(222)
split = sample.split(dataset$V11, SplitRatio = 0.7)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

str(training_set)

# Neural Networks
library(neuralnet)
set.seed(333)
# ce - cross entropy
n <- neuralnet(V11~V2+V3+V4+V5+V6+V8+V9+V10,
               data = training_set,
               hidden = 5,
               err.fct = "ce",
               linear.output = FALSE)
plot(n)

""" 
# Repeat, two hidden layers-first with 2 neurons, second with 1 neuron
# repeat network traing 5 times - each training starts with a new initializations & computes the weights
# lifesign - to see full output
# default maxmum steps is 100000, it may converge sooner, with stepmax can specify the max steps.
n <- neuralnet(admit~gre+gpa+rank,
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
output <- compute(n, training[,-1], rep = 3)
head(output$net.result)
head(training[1,])
"""


# Prediction
# training[,-1] # Removing the response variable
output <- compute(n, training_set[,-9]) # Computes the response for the training data
head(output$net.result)
head(training_set[,9])



# Confusion Matrix & Misclassification Error - training data
output <- compute(n, training_set[,-9])
p1 <- output$net.result
pred1 <- ifelse(p1>0.5, 1, 0)


tab1 <- table(pred1, training_set[,9])
tab1
1-sum(diag(tab1))/sum(tab1)

# Confusion Matrix & Misclassification Error - testing data
output <- compute(n, testing[,-1])
p2 <- output$net.result
pred2 <- ifelse(p2>0.5, 1, 0)
tab2 <- table(pred2, testing$admit)
tab2
1-sum(diag(tab2))/sum(tab2)
