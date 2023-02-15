#Riddhi Mahesh Dange
rm(list=ls())
#Loading the dataset
BreastC<-read.csv("/Users/riddhidange/Desktop/wisc_bc_ContinuousVar (1).csv",na.strings = '?')
BreastC<-na.omit(BreastC)
BreastC<-BreastC[-1]
View(BreastC)
library("kohonen")
training<-BreastC[,-1]
#Applying SOM
SOM<-som(as.matrix(training), grid = somgrid(xdim = 10, ydim=10, topo="hexagonal"), rlen=500, 
                keep.data = TRUE )
plot(SOM, type="changes")
plot(SOM, type="dist.neighbours", main = "SOM neighbour distances")
summary(SOM)
str(SOM)
SOM$unit.classif
table(cluster=SOM$unit.classif,BreastC[,1])
summary(SOM)
map(SOM,as.matrix(BreastC[,-1]))
