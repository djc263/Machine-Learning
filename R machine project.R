
set.seed(12345)
pmltesting<-read.csv("pml-testing.csv",na.strings=c("","NA"))
pmltraining<-read.csv("pml-training.csv",na.strings=c("","NA"))
library(caret)
library(ggplot2)
library(randomForest)

inTrain <- createDataPartition(pmltraining$classe, p = .7,list=FALSE)
training <- pmltraining[ inTrain,]
testing <- pmltraining[-inTrain,]


##some values are above 95% NA, these are removed now
removena1<-colSums(is.na(training))<(.95*nrow(training))
training2<-training[,removena1]
testing2<-testing[,removena1]
pmltesting2<-pmltesting[,removena1]


##summary(training2)


##remove row nume user name, window, and timestamp feilds at start of training set
training3<-training2[,-c(1:7)]
testing3<-testing2[,-c(1:7)]
pmltesting3<-pmltesting2[,-c(1:7)]

test<-sapply(training3,is.numeric)
training3<-training3[complete.cases(training3),]

##featurePlot(x=training,y=pmltraining$classe,type=pairs)

#classe is the result variable based on how well they did the exercise

prepml<-preProcess(training3[,-53],method=c("center","scale"))
training4<-predict(prepml,training3[,-53])
classe<-training3$classe
training4<-cbind(training4,classe)

testing4<-predict(prepml,testing3[,-53])
classe<-testing3$classe
testing4<-cbind(testing4,classe)

pmltesting4<-predict(prepml,pmltesting3[,-53])

prepml<-preProcess(training4[,-53],method="pca",thresh=.90)
training5<-predict(prepml,training4[,-53])
classe<-training3$classe
training5<-cbind(training5,classe)


##modFit<-train(classe~ .,data=training5,method="rf",prox=TRUE)
modFit<-randomForest(classe~ .,data=training4,ntree=500)

pred <- predict(modFit,testing4)

modFit
table(pred,testing4$classe)
accuracy<-sum(pred==testing4$classe)/nrow(testing4)
accuracy

x <- predict(modFit,pmltesting4)

setwd("D:/Users/dave/R/machine learning project")

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(x)
