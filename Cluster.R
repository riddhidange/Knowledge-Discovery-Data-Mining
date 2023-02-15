#Riddhi Mahesh Dange
rm(list=ls())
#loading the dataset
Breastcancer<-read.csv("/Users/riddhidange/Desktop/wisc_bc_ContinuousVar (1).csv",na.strings = '?')#Change the path accordingly.
View(Breastcancer)
summary(Breastcancer)
table(Breastcancer$diagnosis)
#To factor the dataset
Breastcancer<-na.omit(Breastcancer)
Breastcancer<-Breastcancer[-1]
cancer_dist<-dist(Breastcancer[,-1])
#applying hclust algorithm
hclust_results<-hclust(cancer_dist)
plot(hclust_results)
hclust_2<-cutree(hclust_results,2)
table(hclust_2,Breastcancer[,1])


rm(list=ls())
#loading the dataset
Breastcancer<-read.csv("/Users/riddhidange/Desktop/wisc_bc_ContinuousVar (1).csv",na.strings = '?')#Change the path accordingly.
View(Breastcancer)
summary(Breastcancer)
table(Breastcancer$diagnosis)
#To factor the data set
Breastcancer<-na.omit(Breastcancer)
Breastcancer<-Breastcancer[-1]
#applying kmean algorithm
Kmeans<- kmeans(Breastcancer[,-1],2,nstart = 10)
Kmeans$cluster
table(Kmeans$cluster,Breastcancer[,1])


