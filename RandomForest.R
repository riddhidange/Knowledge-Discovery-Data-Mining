#Riddhi Mahesh Dange
rm(list=ls())
colcls=c("Class"="factor")
#loading dataset
breastcancer<-read.csv("/Users/riddhidange/Desktop/ CS513 KDDM/hw4/breast-cancer-wisconsin.csv",na.strings = '?', colClasses =  colcls)
breastcancer<- na.omit(breastcancer)
set.seed(111)
#segregating data into testing and training
ID<-sort(sample(nrow(breastcancer),round(.30*nrow(breastcancer))))
training<-breastcancer[-ID,]
test<-breastcancer[ID,]
#6.2 Implementing random forest methodology
library(randomForest)
rf <- randomForest( factor(Class)~., data=training, importance=TRUE, ntree=1000)
importance(rf, type = 1)
varImpPlot(rf)
p = predict(rf,test[-11])

# determing Confusion Matrix
ConfM= table(test[,11], p)
ConfM

# calculating error rate
w<- (test[,11]!=p)
er<-sum(w)/length(w)
er

# measuring Accuracy
accuracy<- sum(diag(ConfM)) / sum(ConfM)
accuracy
