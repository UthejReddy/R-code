##Support Vector Machine

#divide the data into Training and test
letters_train<-letterdata[1:16000,]
letters_test<-letterdata[16001:20000,]

##Training a model on the data
#begin by training a simple linear SVM
library(kernlab)
letter_classifier <-ksvm(letter~.,data=letters_train,kernel="vanilladot")

#Evaluating model performance
#predictions on testing dataset
letter_predictions<-predict(letter_classifier,letters_test)
head(letter_predictions)#shows starting 6 letters from the dataset

table(letter_predictions,letters_test$letter)
agreement<-letter_predictions==letters_test$letter
prop.table(table(agreement))#shows the accuracy 

####Improving model performance
letter_classifier_2 <-ksvm(letter~.,data=letters_train,kernel="rbfdot")

letter_predictions_2<-predict(letter_classifier_2,letters_test)
head(letter_predictions_2)

table(letter_predictions_2,letters_test$letter)
agreement_2<-letter_predictions_2==letters_test$letter
prop.table(table(agreement_2))
