#Riddhi Mahesh Dange
rm(list=ls())
BreastC=read.csv("/Users/riddhidange/Desktop/wisc_bc_ContinuousVar (1).csv",na.strings = '?')
#View(BreastC)
BreastC<- data.frame(lapply(BreastC, factor))
BreastC$diagnosis = factor(BreastC$diagnosis,levels = c('M','B'),labels = c(1,2))
BreastC <- data.frame(lapply(na.omit(BreastC), as.numeric))
# create test and training dataset
index<-sort(sample(nrow(BreastC),round(.70*nrow( BreastC))))
training<- BreastC[index,]
test<- BreastC[-index,]
library(e1071)
#applying svm
svm.model<-svm(diagnosis ~ ., data=training, type="C-classification")
svm.pred <- predict(svm.model,  test )
table(actual=test$diagnosis,svm.pred )
SVM_wrong<- (test$diagnosis!=svm.pred)
error_rate<-sum(SVM_wrong)/length(SVM_wrong)
error_rate
accuracy=(1- error_rate)*100
accuracy
