breast_cancer <- read.csv("/Users/riddhidange/Desktop/hw2/breast-cancer-wisconsin .csv", na.string="?")
str(breast_cancer)
#removal of rows with missing values and sample ID
missing <- na.omit(breast_cancer)
breast_cancer_new<- missing[,-1]
#dividing in categories represented by factor 
breast_cancer_new$Class <- as.factor(breast_cancer_new$Class)
levels(breast_cancer_new$Class)<-c('benign','maligant')
summary(breast_cancer_new)

#segregating into training and testing data in bthe ratio 70:30
ID<- sample(nrow(breast_cancer_new), as.integer(.70*nrow(breast_cancer_new)))
training <- breast_cancer_new[ID,]
testing <-breast_cancer_new[ID,]

#classification based on k=3
library(kknn)
library(caret)
k3 <- kknn(formula=Class~., training, testing, k=3, kernel='rectangular')
fitk3 <- fitted(k3)
table(testing$Class,fitk3)
CMatrix <- table(testing$Class, fitk3)
confusionMatrix(CMatrix)

#classification based on k=5
k5 <- kknn(formula=Class~., training, testing, k=5, kernel='rectangular')
fitk5 <- fitted(k5)
table(testing$Class,fitk5)
CMatrix <- table(testing$Class, fitk5)
confusionMatrix(CMatrix)

#classification based on k=10
k10 <- kknn(formula=Class~., training, testing, k=10, kernel='rectangular')
fitk10 <- fitted(k5)
table(testing$Class,fitk10)
CMatrix <- table(testing$Class, fitk10)
confusionMatrix(CMatrix)



           
           

