### Practical Machine Learning Algorithm ####
## R version 3.6.0

#Get the datasets by reading the csv files from the links provided.
training <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"),header=TRUE)
testing <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"),header=TRUE)

dim(training)

### As the data for building the model is ~20K is sufficient to build most of the models.
### This is a classification problem with 5 different classes with Labels. We can use the following 
### 1. Decision trees, 2. Random forest, 3. Gradient descent, 4. mutli-class logistic regression, 5. SVM, 6. Naive Bayes
## Any of the above model with 90% above accuracy and sensitivity and specificity would be a great model for prediction.
## Let us build simple models to see the accuracy
str(training)


##Load libraries
library(caret)

inTrain <- createDataPartition(training$classe,p=0.7,list=FALSE)
train <- training[inTrain,]
test <- training[-inTrain,]

# The dataset contains many NAs and it may contain near zero variences. remove unncessessary features.
# Remove first 2 features since they 

train <- train[,-1:-2]
train <- train[,-nearZeroVar(train)]

# remove variables that are almost always NA
naFeatures <- sapply(train, function(x) mean(is.na(x))) > 0.95
train <- train[,naFeatures==F]


library(randomForest)
# 3 fold cross validation
kfoldVal <- trainControl(method="cv", number=3, verboseIter=F)
fit <- train(classe ~ ., data=train, method="rf", trControl=kfoldVal)
fit$finalModel

### Model evaluation on the test data
test <- test[,-1:-2]
test <- test[,-nearZeroVar(test)]

# remove variables that are almost always NA
naFeatures <- sapply(test, function(x) mean(is.na(x))) > 0.95
test <- test[,naFeatures==F]

predMod <- predict(fit, newdata=test)
confusionMatrix(test$classe, predMod)

#Overall model accuracy is 99.81% which could be a best model keeping sensitivity & specificity >=99%

#Predict the testing document
testing <- testing[,-1:-2]
testing <- testing[,-nearZeroVar(testing)]

# remove variables that are almost always NA
naFeatures <- sapply(testing, function(x) mean(is.na(x))) > 0.95
testing <- testing[,naFeatures==F]

predMod <- predict(fit, newdata=testing)
print(predMod)

