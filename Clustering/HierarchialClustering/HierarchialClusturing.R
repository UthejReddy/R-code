mydata<-scale(Universities[,2:7])#convert all the data in similar dimention between -3 to 3
mydata

#calculate the distance between universities using euclidean distance
d<-dist(mydata,method = "euclidean")
d

#calculate the average distance between the universities
y<-hclust(d,method = "average")
y

plot(y)#plot the dendrogram

#cut the dendogram in to 4 clusters
group<-cutree(y,k=4)
group

#mark the clusters or seperate them in rectangle
  rect.hclust(y,k=4,border = "red")

#show all the similar clusters or the similar universities
clusters=data.frame('uni'=Universities[,1],'cluster'=group)
clusters
