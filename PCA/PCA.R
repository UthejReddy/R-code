#PCA:-Principle Component Analysis
#Used to identify relation between columns
#install packages "gdata" and "xlsx"
install.packages("gdata")
install.packages("xlsx")

library(gdata)
#Applying principle component from column 2 to 7
pca<-princomp(Universities[,2:7],cor = TRUE,scores = TRUE,covmat = NULL)
summary(pca)

pca$scores
pca$loadings

plot(pca$scores[,1:2],col="blue",pch=22,cex=0.3,lwd=3)#plot the graph 
text(pca$scores[,1:2],labels = c(1:25),cex = 1)
