train<-read.csv(file.choose())
test<-read.csv(file.choose())

#basic statistics for train dataset
sd(train$age)
sd(train$capitalgain)
sd(train$capitalloss)
sd(train$hoursperweek)

#graphical representation of train dataset.
boxplot(train$age)
boxplot(train$capitalgain)
boxplot(train$capitalloss)
boxplot(train$hoursperweek)

#histogram
hist(train$age)
hist(train$capitalgain)
hist(train$capitalloss)
hist(train$hoursperweek)


library(kernlab)


salary_classifier <-ksvm(train$Salary~.,data=train,kernel="vanilladot")
                         
salary_predictions<-predict(salary_classifier,test)
head(salary_predictions)#shows starting 6 letters from the dataset

table(salary_predictions,test$Salary)
agreement<-salary_predictions==test$Salary
prop.table(table(agreement))#shows the accuracy 
#here we get accuracy as 84%.

####Improving model performance
salary_classifier2 <-ksvm(train$Salary~.,data=train,kernel="rbfdot")

salary_predictions2<-predict(salary_classifier2,test)
head(salary_predictions2)

table(salary_predictions2,test$Salary)
agreement_2<-salary_predictions2==test$Salary
prop.table(table(agreement_2))

#here accuracy has been improved.