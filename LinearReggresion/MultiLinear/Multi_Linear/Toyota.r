#Multi Linear Regression


corolla <- read.csv(file.choose())

corolla <- data.frame(cbind(corolla$Price,corolla$Age_08_04,corolla$KM,corolla$HP,corolla$cc, corolla$Doors, corolla$Gears,corolla$Quarterly_Tax,corolla$Weight))
colnames(corolla) <- c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")

summary(corolla)

#Lets build a predicting model

model <- lm(corolla$Price~. , data = corolla)
summary(model)

#its R-Square value is 0.8638.

#Predict the Values for the Data of the Dataset

pred <- predict(model)
pred

final<- data.frame(corolla$Price, pred, Error = model$residuals)
View(final)

#here we use scale funtion to convert all the values between -3 and 3.

scalling<- data.frame(scale(corolla))

#using scalled values we will build another model.
model2 <- lm(scalling$Price~., data = scalling)
summary(model2)

pred_scale <- predict(model2)
data <- data.frame(scalling$Price, pred_scale, model2$residuals)
data

#Log  Transformation

model3<- lm(log(corolla$Price)~. , data = corolla)
summary(model3)
pred_log <- predict(model3)
data.frame(log(corolla$Price), pred_log, model3$residuals)
#using log transfomation we got better accuracy in the prediction.
