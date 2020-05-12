#random forest for fraud dataset

fraud_rf<- read.csv(file.choose())

install.packages("caret",dependencies = TRUE)

install.packages("randomForest")

install.packages("gmodel")

library(randomForest)
library(gmodels)
library(caret)

#dimensions of datset
dim(fraud_rf)

summary(fraud_rf)

#standard deviations of dataset
sd(fraud_rf$Taxable.Income)
sd(fraud_rf$City.Population)
sd(fraud_rf$Work.Experience)

#variance of dataset
var(fraud_rf$Taxable.Income)
var(fraud_rf$City.Population)
var(fraud_rf$Work.Experience)



#histogram of numeric columns
hist(fraud_rf$Taxable.Income)
hist(fraud_rf$City.Population)
hist(fraud_rf$Work.Experience)

#Boxplot of columns
boxplot(fraud_rf$Taxable.Income)
boxplot(fraud_rf$City.Population)
boxplot(fraud_rf$Work.Experience)



#use if else statemnet to diffr=erentiate the data into risky and good data
income_type <- ifelse(fraud_rf$Taxable.Income<= 30000, "Risky", "Good")
FraudCheck <- data.frame(fraud_rf,income_type)

#convert the data into factor
FraudCheck$income_type <- as.factor(FraudCheck$income_type)

#create an train ad test data to show the data if tree
datapartition <- createDataPartition(FraudCheck$income_type, p=.60, list = F)
fraud_train <- FraudCheck[datapartition,]
fraud_test <- FraudCheck[-datapartition,]


#create a rando forest model
model<-randomForest(FraudCheck$income_type~.,data = FraudCheck,ntree=1000)

print(model)#here data is categorized and error is also shown

print(importance(model))
pred1 <- predict(model, fraud_test[,-7])


#crosstable for the model
CrossTable(fraud_test[,6], pred1)

#find the accuracy
tab1 <- table(fraud_test[,6], pred1)
sum(diag(tab1))/sum(tab1)

normalise <- function(x) {
  return((x - max(x))/(max(x) - min(x)))
}

fraud_apply <- as.data.frame(lapply(FraudCheck[,c(3,4)], normalise))
fraud_bind <- cbind(FraudCheck, fraud_rf[,c(1,2,5,6)])

datapartition2 <- createDataPartition(fraud_bind$income_type, p=.60, list = F)
fraud_train2 <- fraud_bind[datapartition2,]
fraud_test2 <- fraud_bind[-datapartition2,]


#create another model for different number of tress
#create a random forest model
model2<-randomForest(fraud_bind$income_type~.,data = fraud_apply,ntree=1500)

print(model2)#here data is categorized and error is also shown

print(importance(model2))
pred2 <- predict(model2, fraud_test[,-7])


#crosstable for the model
CrossTable(fraud_test[,6], pred2)s

#find the accuracy
tab2 <- table(fraud_test[,6], pred2)

sum(diag(tab2))/sum(tab2)

