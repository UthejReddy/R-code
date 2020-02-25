sum(is.na(claimants))#gives the number of NA values available in the dataset
claimants<-na.omit(claimants)#omit NA values from the dataset
#na.omit will omit the rows which have atleast 1 NA value

dim(claimants)#gives the total count of available rows and coloumns
colnames(claimants)#will show the names of the all coloumns
claimants<-claimants[,-1]#removing the first value which is an index

# preparing a linear regression 
mod<-lm(ATTORNEY~.,data = claimants)#create a linear model using dataset
mod
pred<-predict(mod,claimants)
pred
plot(claimants$CLMINSUR,pred)#plot the graph for predicted values versus coloumn of the dataset

#we can now use the linear regression technique to classify the data
plot(pred)

#GLM function use sigmoid curve to produce desirable results
#the output of sigmoid function lies between 0-1
model<-glm(ATTORNEY~.,data = claimants,family = "binomial")
summary(model)

#confusion matrix table
prob<-predict(model,claimants,type = "response")
prob

#confusion matrix table and considering the thresshold value as 0.5
confusion<-table(prob>0.5,claimants$ATTORNEY)
confusion

#model accuracy
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
