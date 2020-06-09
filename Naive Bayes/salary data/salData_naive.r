library(e1071)
library(caret)
library(pROC)
library(ggplot2)

sal_train <- read.csv(file.choose()) #This data is used to train the Model
attach(salary_train)

sal_test <- read.csv(file.choose()) #This data is used to Test the Model
attach(salary_test)

summary(sal_train)
str(sal_train)
names(sal_train)

#convert variables into an Cataegorical Vriable.
sal_train$workclass <- as.factor(sal_train$workclass)
sal_train$education <-as.factor(sal_train$education)
sal_train$maritalstatus <- as.factor(sal_train$maritalstatus)
sal_train$occupation <- as.factor(sal_train$occupation)
sal_train$relationship <- as.factor(sal_train$relationship)
sal_train$race <- as.factor(sal_train$race)
sal_train$sex <- as.factor(sal_train$sex)
sal_train$native <- as.factor(sal_train$native)

summary(sal_test)
str(sal_test)
names(sal_test)

sal_test$workclass <- as.factor(sal_test$workclass)
sal_test$education <-as.factor(sal_test$education)
sal_test$maritalstatus <- as.factor(sal_test$maritalstatus)
sal_test$occupation <- as.factor(sal_test$occupation)
sal_test$relationship <- as.factor(sal_test$relationship)
sal_test$race <- as.factor(sal_test$race)
sal_test$sex <- as.factor(sal_test$sex)
sal_test$native <- as.factor(sal_test$native)
sal_train$Salary <- as.factor(ifelse(sal_train$Salary == " >50K", "High", "Low"))
sal_test$Salary <- as.factor(ifelse(sal_test$Salary == " >50K", "High", "Low"))

str(sal_train)
str(sal_test)

#use scale command to get the gat between -3 to 3
sal_train$age <- scale(sal_train$age)
sal_train$educationno <- scale(sal_train$educationno)
sal_train$capitalgain <- scale(sal_train$capitalgain)
sal_train$capitalloss<- scale(sal_train$capitalloss)
sal_train$hoursperweek <- scale(sal_train$hoursperweek)
sal_test$age <- scale(sal_test$age)
sal_test$educationno <- scale(sal_test$educationno)
sal_test$capitalgain <- scale(sal_test$capitalgain)
sal_test$capitalloss<- scale(sal_test$capitalloss)
sal_test$hoursperweek <- scale(sal_test$hoursperweek)

#Plots
ggplot(sal_train) + geom_bar(aes(age))  

ggplot(sal_train) + geom_bar(aes(sex), color = "blue") 

ggplot(sal_train) + geom_histogram(aes(race), stat = "count") 

ggplot(sal_train, aes(age,capitalgain)) + geom_line()

# Build the Classifier
sal_classifier <- naiveBayes(Salary~. , data = sal_train)

summary(sal_classifier)

pred <- predict(sal_classifier, sal_test[,-14])
conf <- confusionMatrix(sal_test$Salary, pred, mode = "everything")
conf

acc <-  conf$overall[1]
precision <-  conf$byClass[5]
recall <-  conf$byClass[6]
f1score <-  conf$byClass[7]
area <- roc(sal_test$Salary,predictor = factor(pred, ordered = TRUE))
plot(area)
auc <- area$auc
auc

#here area under curve is 0.7056


#build the model
sal_classifier2 <- naiveBayes(Salary~age + educationno + occupation + sex, data = sal_train )
sal_classifier2
summary(sal_classifier2)
pred2 <- predict(sal_classifier2, sal_test[,-14])
conf2 <- confusionMatrix(sal_test$Salary, pred2, mode = "everything")
conf2
acc2 <-  conf2$overall[1]
precision2 <-  conf2$byClass[5]
recall2 <-  conf2$byClass[6]
f1score2 <-  conf2$byClass[7]
area2 <- roc(sal_test$Salary,predictor = factor(pred2, ordered = TRUE))
plot(area2)
auc1 <- area2$auc
auc1
#here we got area under curve as 0.698

sal_classifier3 <- naiveBayes(Salary~age + educationno + occupation + sex + capitalgain, data = sal_train )
sal_classifier3
summary(sal_classifier3)
pred3 <- predict(sal_classifier3, sal_test[,-14])
conf3 <- confusionMatrix(sal_test$Salary, pred3, mode = "everything")
conf3
acc3 <-  conf3$overall[1]
precision3 <-  conf3$byClass[5]
recall3 <-  conf3$byClass[6]
f1score3 <-  conf3$byClass[7]
area3 <- roc(sal_test$Salary,predictor = factor(pred3, ordered = TRUE))
plot(area3)
auc3 <- area3$auc
auc3
#here we got area under curve as 0.5818

sal_classifier4 <- naiveBayes(Salary~age + educationno + occupation + sex + capitalgain + capitalloss + hoursperweek, data = sal_train )
sal_classifier4
summary(sal_classifier4)
pred4 <- predict(sal_classifier4, sal_test[,-14])
conf4 <- confusionMatrix(sal_test$Salary, pred4, mode = "everything")
conf4
acc4 <-  conf4$overall[1]
precision4 <-  conf4$byClass[5]
recall4 <-  conf4$byClass[6]
f1score4 <-  conf4$byClass[7]
area4 <- roc(sal_test$Salary,predictor = factor(pred4, ordered = TRUE))
plot(area4)
auc4 <- area4$auc
auc4
#here  we got area under curve as 0.6162
