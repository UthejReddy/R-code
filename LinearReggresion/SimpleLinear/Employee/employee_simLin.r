#Employee data

employee <- read.csv(file.choose())#import datset

summary(employee)#get discriptive data

plot(employee)#plot graph

#get standard diviation
sd(employee$Salary_hike)
sd(employee$Churn_out_rate)

#get variance
var(employee$Salary_hike)
var(employee$Churn_out_rate)

#create linear model
model <- lm(employee$Churn_out_rate~., data = employee)
summary(model)
#Here  R-Squared value is 0.8312 and the P-value is less than 0.05. 

pred<-predict(model,newdata=data.frame(Salary_hike=1600))
pred

#Squareroot Transformation
model<-lm(employee$Churn_out_rate~.,sqrt(employee))
summary(model)

#Log Transformation
model<-lm(log(employee$Churn_out_rate)~.,data=employee)
summary(model)
#Here using log transformation we get better R-Squared value and Residual error is decreased.  
