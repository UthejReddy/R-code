#PCA:-Principle Component Analysis
#Used to identify relation between columns
#install packages "gdata" and "xlsx"
install.packages("gdata")
install.packages("xlsx")

library(gdata)
wine<-read.csv(file.choose())
#Applying principle component from column 2 to 7
pca<-princomp(wine[,2:7],cor = TRUE,scores = TRUE,covmat = NULL)
summary(pca)

pca$loadings
pca$scores
U<-pca$scores
View(U)

first3comp<-pca$scores[,1:3]

plot(pca$scores[,1:2],col="blue",pch=22,cex=0.3,lwd=3)#plot the graph 
text(pca$scores[,1:2],labels = c(1:25),cex = 1)

pairs(pca$scores)

#hierarchial clustering
mydata<-scale(U[,1:3])#convert all the data in similar dimention between -3 to 3
mydata

#calculate the distance between wines using euclidean distance
d<-dist(mydata,method = "euclidean")
d

#calculate the average distance between the wines
y<-hclust(d,method = "average")
y

plot(y)#plot the dendrogram

#cut the dendogram in to 4 clusters
group<-cutree(y,k=5)
group

#mark the clusters or seperate them in rectangle
rect.hclust(y,k=5,border = "red")

#show all the similar clusters or the similar universities
clusters=data.frame('uni'=first3comp[,1],'cluster'=group)
clusters


#Kmeans cluster
#install package plyr
install.packages("plyr")
library(plyr)

plot(mydata)#ploting the dataset

#get the reference value of k for the experiment in elbow plt-4 clusters:
#use the k-value for the reference and run to find the optimum k value
wss<-c()
for(i in 2:15)#using for loop to get appropriate value of k 
  wss[i]<-sum(kmeans(mydata,centers = i)$withinss)  
plot(1:15,wss,type = "b",xlab = "no of clusters",ylab = "avg distance")

#cluster algorithm building
km<-kmeans(first3comp,5)
km$centers#this will show the distance between centroid and all datapoints
km$cluster#this will show the clusters formed

install.packages("animation")
library(animation)
windows()#this will popup a seperate window to show the clusters
km<-kmeans.ani(first3comp,5)
km

#now applying kmeans cluster on the whole data

wss<-c()
for(i in 2:15)#using for loop to get appropriate value of k 
  wss[i]<-sum(kmeans(mydata,centers = i)$withinss)  
plot(1:15,wss,type = "b",xlab = "no of clusters",ylab = "avg distance")

#cluster algorithm building
km<-kmeans(mydata,4)
km$centers#this will show the distance between centroid and all datapoints
km$cluster#this will show the clusters formed

install.packages("animation")
library(animation)
windows()#this will popup a seperate window to show the clusters
km<-kmeans.ani(mydata,4)
km
