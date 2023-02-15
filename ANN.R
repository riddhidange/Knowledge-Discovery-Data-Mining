#Riddhi Mahesh Dange
rm(list=ls())
library("neuralnet")
#Loading the dataset
BreastC=read.csv("/Users/riddhidange/Desktop/wisc_bc_ContinuousVar (1).csv",na.strings = '?')
#View(BreastC)
BreastC<-data.frame(lapply(BreastC,factor))
# View the dataframe loaded
#View(BreastC)
is.factor(BreastC$diagnosis)
# Replacing 'M' and 'B' of 'diagnosis' column with '0' and '1'
BreastC$diagnosis <- factor(BreastC$diagnosis, levels = c('M','B'),labels = c(0,1))
BreastC <- data.frame(lapply(na.omit(BreastC), as.numeric))
#Splitting data into test and train
index <- sort(sample(nrow(BreastC),round(.70*nrow(BreastC))))
training <- BreastC[index,]
test <- BreastC[-index,]
# Applying svm
nn_ann_data <- neuralnet(diagnosis~.,training[2], hidden=5, threshold=0.01)
ann <- compute(nn_ann_data, test)
ann$net.result 
ann_cat <- ifelse(ann$net.result < 1.5,1,2)
table(Actual=test$diagnosis, Predition=ann_cat)
incorrect <- (test[,2] != ann_cat)
#error rate
error_rate <- sum(incorrect)/length(incorrect)
error_rate
# accuracy
accuracy <- (1 - error_rate) * 100
print(paste("Accuracy is: ", accuracy))
