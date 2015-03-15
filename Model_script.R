## Damien Edwards
## Machine Learning Course project

library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)

##Create traing and test set
trainingset <- read.csv("/Users/dame81/Desktop/ML/pml-training.csv", na.strings=c("NA","#DIV/0!", ""))
testingset<-read.csv("/Users/dame81/Desktop/ML/pml-testing.csv", na.strings=c("NA","#DIV/0!", ""))
## Delete Column missing values
trainingset<-trainingset[,colSums(is.na(trainingset)) == 0]
testingset <-testingset[,colSums(is.na(testingset)) == 0]
trainingset   <-trainingset[,-c(1:7)]
testingset <-testingset[,-c(1:7)]
## Subset of data
subset<-createDataPartition(y=trainingset$classe, p=0.75, list=FALSE)
subTraining <- trainingset[subset, ] 
subTesting <- trainingset[-subset, ]
## Plot of Data
plot(subTraining$classe, col="green", main="Levels of the variable classe within the subTraining data set", xlab="classe levels", ylab="Frequency")

##Model 1
modelA <- rpart(classe ~ ., data=subTraining, method="class")
predictionA<- predict(modelA, subTesting, type = "class")
rpart.plot(modelA, main="Classification Tree", extra=102, under=TRUE, faclen=0)
confusionMatrix(predictionA, subTesting$classe)

##Model 2
modelB <- randomForest(classe ~. , data=subTraining, method="class")
predictionB<- predict(modelB, subTesting, type = "class")
confusionMatrix(predictionB, subTesting$classe)

##PredictionModel
finalmodel<-predict(modelB,testingset,type="class")
finalmodel

##WriteModel
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(finalmodel)




