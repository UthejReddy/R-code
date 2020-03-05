dataset<-datasets::iris
install.packages("caret",dependencies = TRUE)
install.packages("randomForest")

library(randomForest)


model<-randomForest(iris$Species~.,data = iris,ntree=1000)

#view the forest results
print(model)

#imporantace of the variable-lower gini
print(importance(model))

#Prediction
pred<-predict(model,iris[,-5])
table(pred,iris$Species)
