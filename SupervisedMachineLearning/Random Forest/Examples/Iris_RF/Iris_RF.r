dataset<-datasets::iris
install.packages("caret",dependencies = TRUE)
install.packages("randomForest")
install.packages("gmodels")

library(caret)
library(gmodels)
library(randomForest)

summary(dataset)

dim(dataset)

#standard deviation
sd(dataset$Sepal.Length)
sd(dataset$Sepal.Width)
sd(dataset$Petal.Length)
sd(dataset$Petal.Width)

#variance of dataset
var(dataset$Sepal.Length)
var(dataset$Sepal.Width)
var(dataset$Petal.Length)
var(dataset$Petal.Width)

#correlation 
cor(dataset[,-5])


model<-randomForest(iris$Species~.,data = iris,ntree=1000)

#view the forest results
print(model)

#imporantace of the variable-lower gini
print(importance(model))

#Prediction
pred<-predict(model,iris[,-5])

a<-table(pred,iris$Species)
a

acc<-sum(diag(a))/sum(a)

acc

#model for 500 number of trees
model2<-randomForest(Species~.,data = iris,ntree=500)

#view the forest results
print(model2)

#imporantace of the variable-lower gini
print(importance(model2))

#Prediction
pred2<-predict(model2,iris[,-5])


a2<-table(pred2,iris$Species)

a2

acc2<-sum(diag(a2))/sum(a2)

acc2
