#Delivery dataset

delivery <- read.csv(file.choose())#import dataset
summary(delivery)#get discriptive data

#plot graph
plot(delivery)

#calculate standard diviation
sd(delivery$Delivery.Time)
sd(delivery$Sorting.Time)

#calculate variance
var(delivery$Delivery.Time)
var(delivery$Sorting.Time)

#create linear model
model <- lm(delivery$Delivery.Time~., data = delivery)

summary(model)
#Here we can see that the R-Squared value is 0.6823 and the P-value is less than 0.05

pred<-predict(model,newdata=data.frame(Sorting.Time=30))
pred

#Squareroot Transformation
model<-lm(delivery$Delivery.Time~.,sqrt(delivery))
summary(model)

#Log Transformation
model<-lm(log(delivery$Delivery.Time)~.,data=delivery)
summary(model)
#Here using log transformation we get better R-Squared value and Residual error is decreased.  

