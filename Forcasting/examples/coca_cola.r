library(xlsx)

coca_cola <- read.xlsx(file.choose(),1)

View(coca)

summary(coca_cola)

sd(coca_cola$Sales) #Standard Deviation

var(coca_cola$Sales) #Variance

plot(coca_cola$Sales, type = "l") 

#create Dummy Variables

coca_cola["Qs"] <- rep(1:4, len=42)
coca_cola[paste0('Q', 1:4)] <- model.matrix(~ factor(coca_cola$Qs)-1)


#Lets add a new column "t" 
coca_cola["t"]<- 1:42
View(coca_cola)


coca_cola["t_square"] <- coca_cola["t"] * coca_cola["t"]
View(coca_cola)

coca_cola["log_sales"] <- log(coca_cola["Sales"])
View(coca_cola)

#Divide the data into Train and Test data
train <- coca_cola[1:28,]
test <- coca_cola[29:42,]

# Linear Regression Model

model1 <- lm(Sales~t , data = train)
summary(model1)
pred1 <- data.frame(predict(model1, interval = "predict", newdata = test)) #Here we are pedicting the values for test set
View(pred1)

rmse_model1 <- sqrt(mean((test$Sales - pred1$fit)^2, na.rm = T))
rmse_model1


# Exponential Model

model2 <- lm(log_sales~t, data = train) #Here we are Building a Exponential Model
summary(model2)
pred2 <- data.frame(predict(model2, interval = "predict", newdata = test))
View(pred2)

rmse_model2 <- sqrt(mean((test$Sales - exp(pred2$fit))^2, na.rm = T))
rmse_model2


# Quadratic Model

model3<-lm(Sales~t+t_square,data=train)
summary(model3)
pred3<-data.frame(predict(model3,interval='predict',newdata=test))
rmse_model3<-sqrt(mean((test$Sales-pred3$fit)^2,na.rm=T))
rmse_model3

# Additive Seasonality model

model4<-lm(Sales~Q1+Q2+Q3,data=train)
summary(model4)
pred4<-data.frame(predict(model4,newdata=test,interval='predict'))
rmse_model4<-sqrt(mean((test$Sales-pred4$fit)^2,na.rm = T))
rmse_model4



#make a Table
table_rmse<-data.frame('Model'=c("Linear Regression Model","Exponential Model","Quadratic Model","Additive Seasonality model"),'RMSE'=c(rmse_model1,rmse_model2,rmse_model3,rmse_model4))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

multi_model <- lm(log_sales~t+Q1+Q2+Q3, data = coca_cola)

resid <- residuals(multi_model)
resid[1:10]

hist(resid)

acf(resid,lag.max = 10)

X <- arima(resid, order=c(2,0,0))

acf(X$residuals,lag.max = 15)
pred_res<- predict(arima(resid,order=c(2,0,0)),n.ahead = 120)
str(pred_res)
pred_res$pred
acf(X$residuals)


#convert the data into Time Series data

library(tseries)
library(forecast)

coca_ts <- ts(coca_cola$Sales, frequency = 4, start = c(1986)) #Create a Time Series data
View(coca_ts)
plot(coca_ts) 

pacf(coca_ts) 

acf(coca_ts) 

# build an ARIMA model
arima_model <- arima(coca_ts, order = c(1,1,2), method = "ML")
arima_model

#plot the ARIMA model
plot(forecast(a,h=5), xaxt = "n")


ab <- auto.arima(coca_ts)

plot(forecast(ab, h=5), xaxt = "n")

prediction <- forecast(ab,h=4) #This will Predict for the next 4 Quarters
prediction

