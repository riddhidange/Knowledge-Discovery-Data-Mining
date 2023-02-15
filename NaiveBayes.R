#Riddhi Mahesh Dange
rm(list=ls())
#libraries needed
library(e1071)
library(caret)
#loading the dataset
breast_cancer<- read.csv(file='/Users/riddhidange/Desktop/ CS513 KDDM/hw4/breast-cancer-wisconsin.csv', na.strings=c("?"))
#deleting rows with missing values
breast_cancer1 <-na.omit(breast_cancer)
sum(is.na(breast_cancer1))
#deleting sample ID
breastcancer<-breast_cancer1[-1]
View(breastcancer)
breastcancer$Class <- factor(breastcancer$Class, levels= c('2','4'), labels=c('Benign','Maligant'))
str(breastcancer)
summary(breastcancer)
prop.table(table(Category= breastcancer$F1, Class=breastcancer$Class))
#segregating data into training and testing
idx<-sample(nrow(breastcancer),as.integer(.70*nrow(breastcancer)))
training<-breastcancer[idx,]
test<-breastcancer[-idx,]
#implementing naive bayes algorithm
nb_a<- naiveBayes(training$Class~.,data=training)
nb_a
category_a <-predict(nb_a,test)
category_a
table(nb_a=category_a, class=test$Class)
nb_w <- sum(category_a != test$Class)
nb_w
nb_er <- nb_w/length(category_a) 
nb_er
