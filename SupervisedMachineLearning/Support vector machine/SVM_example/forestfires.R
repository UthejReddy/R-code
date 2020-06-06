##Support Vector Machine

forestfires<-read.csv(file.choose())

summary(forestfires)

#standard deviation
sd(forestfires$FFMC)
sd(forestfires$DMC)
sd(forestfires$DC)
sd(forestfires$ISI)
sd(forestfires$temp)
sd(forestfires$RH)
sd(forestfires$wind)   
sd(forestfires$rain)
sd(forestfires$area)

#graphycal representation
boxplot(forestfires$FFMC)
boxplot(forestfires$DMC)
boxplot(forestfires$DC)
boxplot(forestfires$ISI)
boxplot(forestfires$temp)
boxplot(forestfires$RH)
boxplot(forestfires$wind)
boxplot(forestfires$rain)
boxplot(forestfires$area)


#divide the data into Training and test
letters_train<-forestfires[1:300,]
letters_test<-forestfires[301:517,]

##Training a model on the data
#begin by training a simple linear SVM
library(kernlab)

#create model
letter_classifier <-ksvm(letters_train$size_category~.,data=letters_train,kernel="vanilladot")

#Evaluating model performance
#predictions on testing dataset
letter_predictions<-predict(letter_classifier,letters_test)

head(letter_predictions)#shows starting 6 letters from the dataset

table(letter_predictions,letters_test$size_category)
agreement<-letter_predictions==letters_test$size_category
prop.table(table(agreement))#shows the accuracy 
#here the accuracy is 96%. that means size has been classified 96% times correctly.
