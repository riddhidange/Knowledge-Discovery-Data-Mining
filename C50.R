#Riddhi Mahesh Dange
rm(list=ls())
colcls=c("Class"="factor")
#loading the dataset
breast_cancer<-read.csv(file="/Users/riddhidange/Desktop/ CS513 KDDM/hw4/breast-cancer-wisconsin.csv")
set.seed(111)
#6.1 implementing C5.0 methodology
library(C50)
#loading the dataset
breastcancer<-read.csv("/Users/riddhidange/Desktop/ CS513 KDDM/hw4/breast-cancer-wisconsin.csv",na.strings = '?')
View(breastcancer)
table(breastcancer$Class)
#Factor the data set
breastcancer$Class <- factor(breastcancer$Class, levels = c(2,4),labels = c("Benign", "Malignant"))
# segregating the data set into test and testing 
idx<-sort(sample(nrow(breastcancer),as.integer(.70*nrow(breastcancer))))
training<-breastcancer[idx,]
test<-breastcancer[-idx,]
#implementing C5.0
model<-C5.0(Class~.,training[,-1])
summary(model)
plot(model)
p<-predict(model,test[,-1],type="class") 
confM<-table(test[,11],p)
confM
str(p)
w<-sum(test[,11]!=p)
er<-w/length(test[,11])
er
