zoo_KNN<-read.csv(file.choose())

View(zoo_KNN)

install.packages("caret")
install.packages("class")
install.packages("gmodels")

library(class)
library(caret)
library(gmodels)

zoo_KNN<-zoo_KNN[-1]

str(zoo_KNN)#here type column is in integer format so we have to convert it into factor.

zoo_KNN$type<-as.factor(zoo_KNN$type)#converted to factors

table(zoo_KNN$type)#this shows number of records class belongs to

#normalize the data to get correct classification
normal<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}


#data has been normalized, now apply that normalized data to all columns
zoo_n<-as.data.frame(lapply(zoo_KNN[1:16],normal))

#combine normalized data and type column
combine <- cbind(zoo_n,zoo_KNN$type)                     

#now create random partitions of the data
datapartition <- createDataPartition(combine$`zoo_KNN$type`,p=.70,list = FALSE)
training <-combine[datapartition,]
testing <- combine[-datapartition,]

#create label to training and testing data
training_label <-combine[datapartition,17]
testing_label <- combine[datapartition,17]

pred<-knn(train = training,test = testing,cl=training_label,k=5)
pred

CrossTable(testing$`zoo_KNN$type`,pred)

a <-table(testing$`zoo_KNN$type`,pred)

accuracy<-sum(diag(a))/sum(a)*100
accuracy
#Here we are getting a accuracy of 96%,which means 96% of data is classified correctly.

#now we try to change the value of k to check the accuracy for multiple techniques.

#prediction 1,k = 8
pred1<-knn(train = training,test = testing,cl=training_label,k=8)
pred1

CrossTable(x=testing$`zoo_KNN$type`,pred1)

a <-table(testing$`zoo_KNN$type`,pred1)

accuracy<-sum(diag(a))/sum(a)*100
accuracy
# here we get accuracy as 89%

#prediction 2, k=10
pred2<-knn(train = training,test = testing,cl=training_label,k=10)
pred2

CrossTable(x=testing_label,pred2)

a <-table(testing_label,pred2)

accuracy<-sum(diag(a))/sum(a)*100
accuracy
#Here we get accuracy as 89%

#prediction 3, k=4
pred3<-knn(train = training,test = testing,cl=training_label,k=10)
pred3

CrossTable(x=testing$`zoo_KNN$type`,pred3)

a <-table(testing$`zoo_KNN$type`,pred3)

accuracy<-sum(diag(a))/sum(a)*100 
accuracy
#Here we get accuracy as 92%
#It is clear that as we increase th k values accuracy decreases.



acc <- c()
for (i in 1:50) #We will take k-Values from 1 to 50
{
  print(i)
  
  #Lets Build the Model
  class1 <-knn(train = training[,-17], test = testing[,-17], cl = training$`zoo_KNN$type`, k=i) 
  CrossTable(testing$`zoo_KNN$type`, class1, prop.r = F, prop.c = F, prop.chisq = F)
  tab1 <- table(testing$`zoo_KNN$type`, class1)
  acc <- c(acc,round((sum(diag(tab1))/sum(tab1))*100, digits = 2))
}

summary(acc)
acctable <- data.frame("k" = 1:50, "Accuracy123" = acc)
attach(acctable)
ggplot(acctable, mapping = aes(k, Accuracy123)) + geom_text(aes(label = k), vjust = 3) +geom_line(linetype = "dashed", arrow = arrow())  + geom_point() +  ggtitle("Model Accuracy for 50 Different models")
