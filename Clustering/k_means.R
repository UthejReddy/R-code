#install package plyr
install.packages("plyr")
library(plyr)

x<-runif(50)#generating 50 random numbers from uniform distribution(0,1)
y<-runif(50)#generating 50 random numbers 

#combining both the datasets
data<-cbind(x,y)
plot(data)#ploting the dataset

#get the reference value of k for the experiment in elbow plt-4 clusters:
#use the k-value for the reference and run to find the optimum k value
wss<-c()
for(i in 2:15)#using for loop to get appropriate value of k 
wss[i]<-sum(kmeans(data,centers = i)$withinss)  
plot(1:15,wss,type = "b",xlab = "no of clusters",ylab = "avg distance")

#cluster algorithm building
km<-kmeans(data,8)
km$centers#this will show the distance between centroid and all datapoints
km$cluster#this will show the clusters formed

install.packages("animation")
library(animation)
windows()#this will popup a seperate window to show the clusters
km<-kmeans.ani(data,7)
