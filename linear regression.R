#import dataset NewspaperData

#create model for dataset
model<-lm(sunday~daily,data=NewspaperData)
summary(model)

#predict sundays data for 300 daily circulation
pred<-predict(model,newdata=data.frame(daily=300))
pred
