## Download and Load the data
if (!file.exists("Courseradata")) {
    dir.create("Courseradata")
}
trainUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

download.file(trainUrl, destfile = "/Users/adrianromano/Downloads/Courseradata/trainFile.csv", method = "curl")
download.file(testUrl, destfile = "/Users/adrianromano/Downloads/Courseradata/testFile.csv", method = "curl")

trainData <- read.csv("/Users/adrianromano/Downloads/Courseradata/trainFile.csv", na.strings = c("", "NA", "#DIV/0!"))
testData <- read.csv("/Users/adrianromano/Downloads/Courseradata/testFile.csv", na.strings = c("", "NA", "#DIV/0!"))

## See dimensions and number of NA values
dim(trainData)
dim(testData)
sum(is.na(trainData))
sum(is.na(testData))

## Remove variables containing NA values and not important for prediction
trainData <- trainData[, colSums(is.na(trainData)) == 0]
testData <- testData[, colSums(is.na(testData)) == 0]
trainData <- trainData[, -c(1:7)]
testData <- testData[, -c(1:7)]
dim(trainData)
dim(testData)

## See Near Zero Variance
library(caret)
nzv <- nearZeroVar(trainData, saveMetrics = TRUE)

## Split the training data
set.seed(1995)
inTrain <- createDataPartition(trainData$classe, p = 0.7, list = FALSE)
training <- trainData[inTrain, ]
testing <- trainData[-inTrain, ]
dim(training)
dim(testing)

## Decision Tree Model
library(rpart)
set.seed(1995)
cartFit <- train(classe ~ ., method = "rpart", trControl = trainControl(method = "cv", number = 5), data = training)
cartPred <- predict(cartFit, newdata = testing)
cartCM <- confusionMatrix(cartPred, testing$classe)
cartCM$table

## Boosted Decision Tree Model
library(gbm)
set.seed(1995)
gbmFit<- train(classe ~ ., method = "gbm", data = training, trControl = trainControl(method = "cv", number = 5), verbose = FALSE)
gbmPred<- predict(gbmFit, newdata = testing)
gbmCM <- confusionMatrix(gbmPred, testing$classe)
gbmCM$table

## Random Forest Model
library(randomForest)
set.seed(1995)
rfFit <- train(classe ~ ., method = "rf", data = training, trControl = trainControl(method = "cv", number = 5), importance = TRUE)
rfPred <- predict(rfFit, newdata = testing)
rfCM <- confusionMatrix(rfPred, testing$classe)
rfCM$table

## Model Comparison
accuracy <- data.frame(Model = c("CART", "GBM", "RF"),
                       Accuracy = rbind(cartCM$overall[1], gbmCM$overall[1], rfCM$overall[1]))
accuracy

## Prediction to original testing data
prediction <- predict(rfFit, newdata = testData)
prediction
