install.packages("neuralnet")

concrete_norm<-as.data.frame(lapply(concrete,normalize))


#create training and test data
concrete_train<-concrete_norm[1:773,]
concrete_test<-concrete_norm[774:1030,]


#training a model on the data
#train the neuralnet model

library(neuralnet)

#simple ANN with only a singke hidden neuron
concrete_model<-neuralnet(formula = strength~cement+slag+ash+water+superplastic
                                  +coarseagg+fineagg+age,data=concrete_train)

#visualize the network topology
window()
plot(concrete_model)#plot the network

#Evaluating model performance
#obtain model results
model_results<-compute(concrete_model,concrete_test[1:8])

#obtain predicted strength values
predicted_strength<-model_results$net.result

#examine the correlation between predicted and actual values
cor(predicted_strength,concrete_test$strength)

#improving model performance
#a more complex neural network topology with 5 hidden neurons
concrete_model2<-neuralnet(strength~cement+slag+ash+water+superplastic
                           +coarseagg+fineagg+age,data=concrete_train,hidden = c(5,2))


#plot the network
window()
plot(concrete_model2)