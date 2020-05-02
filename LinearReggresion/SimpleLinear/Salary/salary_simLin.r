#Salary Hike

salary <- read.csv(file.choose())#import dataset

summary(salary)#get discriptive data

plot(salary)#plot graph

#calculate standard diviation
sd(salary$YearsExperience)
sd(salary$Salary)

#calculate variance
var(salary$YearsExperience)
var(salary$Salary)

#create linear model
model <- lm(salary$Salary~., data = salary)

summary(model)
#Here  R-Squared value is 0.957 and the P-value is less than 0.05.

pred<-predict(model,newdata=data.frame(YearsExperience=11))
pred


#Squareroot Transformaton
model<-lm(salary$Salary~.,sqrt(salary))
summary(model)

#Log Transformaton
model<-lm(log(salary$Salary)~.,data=salary)
summary(model)
#Here using log transformation we get better R-Squared value and Residual error is decreased.  

