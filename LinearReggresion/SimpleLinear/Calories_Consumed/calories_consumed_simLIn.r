#Calories Consumed

calorie <- read.csv(file.choose())#import the dataset
summary(calorie)

#get the standard deviation of datset
sd(calorie$Weight.gained..grams.)
sd(calorie$Calories.Consumed)

#get the variance of the dataset
var(calorie$Weight.gained..grams.)
var(calorie$Calories.Consumed)

#corelation between the data
cor(calorie)

#plot the graph 
plot(calorie)

#create linear model
model <- lm(calorie$Weight.gained..grams.~. , data = calorie)
summary(model)
#The R-Sqaured value for the model is 0.8968 and the p-value is less than 0.05.

pred<-predict(model,newdata=data.frame(Calories.Consumed=3400))
pred
predict(model,interval="predict")


#sqareroot tranformation
model<-lm(calorie$Weight.gained..grams.~.,sqrt(calorie))
summary(model)

#log transformation
model<-lm(log(calorie$Weight.gained..grams.)~.,data=calorie)
summary(model)
predict(model,interval="predict")
#Here using log transformation we get better R-Squared value and Residual error is decreased.  

