library(nnet)                                               #include libs
library(hmeasure)

modelName <- "neuralNetwork2"
modelName
setwd("e:/Learning/ML/Numerai/Tournament 93")              #Set Working directory        

trainingset <- read.csv("numerai_training_data.csv")          #Loading Data
testset <- read.csv("numerai_tournament_data.csv")

nrow(trainingset)                                           #Displaying number of rows
nrow(testset)

names(trainingset)                                          #Displaying the names of columns
names(testset)

trainingset  <- trainingset[,grep("feature|target",names(trainingset))]
#

target <- names(trainingset)[51]
target

actual <- testset[1:46362,c(54)]

inputs <- setdiff(names(trainingset),target)
n=30
inputs <-sample(inputs, n)
inputs


testset <- testset[,grep("id|feature",names(testset))]     #Dataset for testing

formula <- as.formula(paste(target, "~", paste(c(inputs), collapse = "+")))             #Formula
formula

model   <- nnet(formula, trainingset, size=13, linout=TRUE, skip=TRUE, MaxNWts=13564, trace=FALSE, maxit=121)    #Model Building
model

Predicted <- as.numeric(round(predict(model, testset)))       #TESTING
head(Predicted)
PredictedProb <- predict(model, testset)
head(PredictedProb)


accuracy <- round(mean(actual==Predicted) *100,2)
accuracy

writedata<-data.frame(testset$id,PredictedProb)
names(writedata)[1]<-paste("id")
names(writedata)[2]<-paste("probability")
write.csv(writedata, file=paste(modelName,".csv",sep=''), row.names=FALSE)
