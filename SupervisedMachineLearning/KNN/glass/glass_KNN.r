#K Nearest Neighbour
#here k is number of nearest neighbour
install.packages("gmodels")
install.packages("caret")
install.packages("class")

library(gmodels)
library(class)
library(caret)

glass<- read.csv(file.choose())
summary(glass)
str(glass)

cor(glass[-10])#correlation between all columns accept class column.
var(glass)#variance between all the columns

#stardard deviation of all columns.
sd(glass$RI)
sd(glass$Na)
sd(glass$Mg)
sd(glass$Al)
sd(glass$Si)
sd(glass$K)
sd(glass$Ca)
sd(glass$Ba)
sd(glass$Fe)

#boxplot of each column.
boxplot(glass$RI)
boxplot(glass$Na)
boxplot(glass$Mg)
boxplot(glass$Al)
boxplot(glass$Si)
boxplot(glass$K)
boxplot(glass$Ca)
boxplot(glass$Ba)
boxplot(glass$Fe)

#convert the type column into factors because it is in the interger format.
glass$Type <- as.factor(glass$Type)

table(glass$Type)#number of classes available
prop.table(table(glass$Type))
#normalize the given data
normal<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

#now apply the normalized data to all columns
glass_n<-as.data.frame(lapply(glass[1:9],normal))

#combine normalized data and class variable
combine<-cbind(glass_n,glass$Type)



#now create training and test data randomly
indatapartition <- createDataPartition(combine$`glass$Type`, p=.50, list = FALSE)
training <- combine[indatapartition,]
testing <- combine[-indatapartition,]

#now label the training and test data
train_label<-combine[indatapartition,10]
test_label<-combine[-indatapartition,10]

X<-NROW(test_label)#To know the number of counts of rows
X

#predict the nearest neighbour using test and train data and taking k value 5.
pred<-knn(train = training,test = testing,cl=train_label,k=5)
pred


#cross tsble shows which class are correctly classified
CrossTable(x=test_label,y=pred)

a <- table(test_label, pred)
Accuracy <- ((sum(diag(a))/sum(a))*100)
Accuracy
#here we get accuracy of 57% which shows that 57% of data is correctly classified.


#improve the accuracy of classification
#use scale funtion to standardize the data frame
scale<-as.data.frame(scale(glass[-10]))

summary(scale)
combine2<-cbind(scale,glass$Type)

#create random test and train data
indatapartition2<- createDataPartition(combine2$`glass$Type`, p=.50, list = FALSE)
training2 <- combine2[indatapartition2,]
testing2 <- combine2[-indatapartition2, ]

train2_label<-combine2[indatapartition2,10]
test2_label<-combine2[-indatapartition2,10]

#here we take k value as 5
scale_pred <- knn(train = training2,test = testing2,cl=train2_label,k=5)
scale_pred

CrossTable(x=test2_label,y=scale_pred)

a2<- table(test2_label,scale_pred)
Accuracy2<-sum(diag(a2)/sum(a2))*100
Accuracy2
#here after improving the classification we are getting accuracy value as 70%
#and here we try to table multiple values of to know which values gives accurecy better.
#putting k values as 9
scale_pred <- knn(train = training2,test = testing2,cl=train2_label,k=9)
scale_pred

CrossTable(x=test2_label,y=scale_pred)

a2<- table(test2_label,scale_pred)
Accuracy2<-sum(diag(a2)/sum(a2))*100
Accuracy2

#putting k value as 10
scale_pred <- knn(train = training2,test = testing2,cl=train2_label,k=10)
scale_pred

CrossTable(x=test2_label,y=scale_pred)

a2<- table(test2_label,scale_pred)
Accuracy2<-sum(diag(a2)/sum(a2))*100
Accuracy2

#putting k values as 6
scale_pred <- knn(train = training2,test = testing2,cl=train2_label,k=6)
scale_pred

CrossTable(x=test2_label,y=scale_pred)

a2<- table(test2_label,scale_pred)
Accuracy2<-sum(diag(a2)/sum(a2))*100
Accuracy2

#putting k values as 4
scale_pred <- knn(train = training2,test = testing2,cl=train2_label,k=4)
scale_pred

CrossTable(x=test2_label,y=scale_pred)

a2<- table(test2_label,scale_pred)
Accuracy2<-sum(diag(a2)/sum(a2))*100
Accuracy2

#putting k values as 9
scale_pred <- knn(train = training2,test = testing2,cl=train2_label,k=15)
scale_pred

CrossTable(x=test2_label,y=scale_pred)

a2<- table(test2_label,scale_pred)
Accuracy2<-sum(diag(a2)/sum(a2))*100
Accuracy2

#creating a loop
accuracy <- c()
for (i in 1:25) #This will Take k values from 1 to 25
{
  print(i)
  
  class1 <- knn(train = training[,-10], test = testing[,-10], cl = training[,10], k=i)
  CrossTable(testing$`glass$Type`, class1, prop.r = F, prop.c = F,prop.chisq = F)
  tab1 <- table(testing$`glass$Type`, class1)
  accuracy <- c(accuracy,round(sum(diag(tab1))/sum(tab1)*100, digits = 2))
  
}

summary(accuracy)
boxplot(accuracy)

AccuracyTable <- data.frame("K.value" = 1:25, "Accuracy" = accuracy)

ggplot(AccuracyTable, mapping = aes(K.value, Accuracy)) + geom_line(linetype = "dashed") + geom_point() + ggtitle("Model Accuracy for Different K-Value")
