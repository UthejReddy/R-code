#import dataset NewspaperData

#create model for dataset
model<-lm(sunday~daily,data=NewspaperData)
summary(model)

#predict sundays data for 300 daily circulation
pred<-predict(model,newdata=data.frame(daily=300))
pred

library("lattice")#import a library lattice

#create a dotplot to show the data in graphical format
dotplot(NewspaperData$sunday,main="dot plot of sunday circulation",col="dodgerblue4")
dotplot(NewspaperData$daily,main="dot plot of daily circulation",col="dodgerblue4")

#create a boxplot to show the data in graphical format
boxplot(NewspaperData$sunday,col = "dodgerblue4")
boxplot(NewspaperData$daily,col = "dodgerblue4")

#reggression equation
#syntex model<-lm(y~x,data=dataset name)
model<-lm(sunday~daily,data=NewspaperData)
summary(model)

#create a dataframe to predict sundays circulation on the basis of daily circulation
new<-data.frame(daily=c(200,500,300))
pred<-predict(model,newdata = new)
pred

#predict the number of errors in model
pred<-predict(model)
pred

#show the final data with the errors
finaldata=data.frame(NewspaperData,pred,"Error"=NewspaperData$sunday-pred)
finaldata
