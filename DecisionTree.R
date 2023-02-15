#Riddhi Mahesh Dange
rm(list = ls())
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
breastcancer = read.csv("/Users/riddhidange/Desktop/ CS513 KDDM/hw4/breast-cancer-wisconsin.csv")

breastcancer$Class <- factor(breastcancer$Class,levels = c(2,4), labels = c("Benign","Malignant"))
set.seed(111)
#segregating into test and train data
idx<-sort(sample(nrow(breastcancer),as.integer((.30*nrow(breastcancer)))))
training <- breastcancer[-idx,]
testing= breastcancer[idx,]
#Growing the tree
cart<- rpart(Class~., data = training)
summary(cart)
#Plots
rpart.plot(cart)
#scoring
cart_prediction<-predict(cart, testing, type="class")
#Frequency Table
table(Actual=testing[,11], Cart =cart_prediction)
cart_predictionnew<-predict(cart,testing)
str(cart_predictionnew)
cart_prediction_category<-ifelse(cart_predictionnew[,1]<=.5,'Malignant','Benign')
table(Actual=testing[,11],Cart=cart_prediction_category)

#Percentage accuracy
match<-(testing[,11]==cart_prediction)*100
accuracy<-sum(match)/length(match)
accuracy

#Error rate
error<- sum(testing[,11]!=cart_prediction)
er<-error/length(testing[,11])
er

library(rpart.plot)
prp(cart)
fancyRpartPlot(cart)

