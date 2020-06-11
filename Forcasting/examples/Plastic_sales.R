#plastic Sales

p_sales <- read.csv(file.choose())

View(p_sales)

summary(p_sales)

#Standard Deviation
sd(p_sales$Sales) 

#Variance
var(p_sales$Sales) 

plot(p_sales$Sales, type = "l") 

#create Dummy Variables
sales<- data.frame(outer(rep(month.abb,length = 60), month.abb,"==") + 0 )# Creating dummies for 12 months
View(sales)

colnames(sales) <- month.abb 
View(sales)

#bind Original data and Dummy Variables
new_data <- cbind(p_sales, sales) 
View(new_data)

new_data["t"]<- 1:60
View(new_data)

new_data["t_square"] <- new_data["t"] * new_data["t"]
View(new_data)

new_data["log_sales"] <- log(new_data["Sales"])
View(new_data)


#Divide the data into Train and Test
train <- new_data[1:36,]
test <- new_data[37:60,]


#Linear Regression Model

model1 <- lm(Sales~t , data = train) 
summary(model1)
pred1 <- data.frame(predict(model1, interval = "predict", newdata = test)) 
View(pred1)

rmse_model1 <- sqrt(mean((test$Sales - pred1$fit)^2, na.rm = T))
rmse_model1


# Exponential Model

model2 <- lm(log_sales~t, data = train) 
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

#Additive Seasonality model

model4<-lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(model4)
pred4<-data.frame(predict(model4,newdata=test,interval='predict'))
rmse_model4<-sqrt(mean((test$Sales-pred4$fit)^2,na.rm = T))
rmse_model4

table_rmse<-data.frame('Model'=c("Linear Regression Model","Exponential Model","Quadratic Model","Additive Seasonality model"),'RMSE'=c(rmse_model1,rmse_model2,rmse_model3,rmse_model4))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

Linear_model<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=new_data)

resid <- residuals(Linear_model)
resid[1:10]

hist(resid)

acf(resid,lag.max = 10)


X <- arima(resid, order=c(2,0,0))

acf(X$residuals,lag.max = 15)
pred_res<- predict(arima(resid,order=c(2,0,0)),n.ahead = 60)
str(pred_res)
pred_res$pred
acf(X$residuals)

#convert the data into Time Series data

library(tseries)
library(forecast)

sales_ts <- ts(p_sales$Sales, frequency = 12, start = c(1949))
View(sales_ts)
plot(sales_ts) 


#p values
pacf(sales_ts) 

#q-value
acf(sales_ts) 

#build an ARIMA model
arima_model <- arima(sales_ts, order = c(1,1,0), method = "ML")
arima_model

plot(forecast(arima_model,h=5), xaxt = "n")


ab <- auto.arima(sales_ts)

plot(forecast(ab, h=5), xaxt = "n")

prediction <- forecast(ab, h=12) 
prediction
