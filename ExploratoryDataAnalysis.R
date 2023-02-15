#Q1.loading the csv file
breast_cancer<- read.csv(file='/Users/riddhidange/Desktop/hw2/breast-cancer-wisconsin .csv', na.strings = '?') 
#viewing the dataset
View(breast_cancer)
#1. summarzing each column
summary(breast_cancer)
#2.identifying missing values 
sum(is.na(breast_cancer))
colSums(is.na(breast_cancer))
#3. replacing the missing values with mean of the column
for (x in  1:ncol(breast_cancer))
{
  breast_cancer[is.na(breast_cancer[,x]),x] <- mean(breast_cancer[,x],na.rm=TRUE)
}
View(breast_cancer)

#4.displaying the scatter plot frequency table of "class" vs. "F6"
freq_table=table(breast_cancer$F6, breast_cancer$Class)
colnames<- c("Class","F6")
freq_table
plot(breast_cancer[,2:7], col="red", main="Scatter Plot- Frequency table of Class vs F6")
 
#5.Histogram of columns F7-F9
par(mfrow=c(2,3))
boxplot(breast_cancer$F7, main="Boxplot of F7", border="black", col="blue")
boxplot(breast_cancer$F8, main="Boxplot of F8", border="black", col="green")
boxplot(breast_cancer$F9, main="Boxplot of F9", border="black", col="yellow")

#Q2.deleting all objects from R enviornment
rm(list=ls())
#reloading csv file
breast_cancer<- read.csv("/Users/riddhidange/Desktop/hw2/breast-cancer-wisconsin .csv", na.strings = '?')
View(breast_cancer)
#removing rows with missing values
breast_cancer[breast_cancer=="?"]<- NA
breast_cancer= na.omit(breast_cancer)
View(breast_cancer)

