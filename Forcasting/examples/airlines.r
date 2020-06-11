install.packages("xlsx")
library(xlsx)
airlines <- read.xlsx(file.choose(),1)
attach(airlines)
View(airlines)

summary(airlines)

#Standard Deviation
sd(airlines$Passengers) 

#Variance
var(airlines$Passengers) 

#Lets do a line Plot of the data
plot(airlines, type = "l") 

#create Dummy Variables for 12 Months
x<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )
View(x)

colnames(x) <- month.abb 
View(x)


#bind original data and variable data
new_data <- cbind(airlines, x) 
View(new_data)

new_data["t"]<- 1:96
View(new_data)

new_data["t_square"] <- new_data["t"] * new_data["t"]
View(new_data)

new_data["log_passenger"] <- log(new_data["Passengers"])
View(new_data)



#divide data into Train and Test data
train <- new_data[1:72,]
test <- new_data[73:96,]


# Linear Regression Model

model <- lm(Passengers~t , data = train)
summary(model)
pred <- data.frame(predict(model, interval = "predict", newdata = test)) #Here we are pedicting the values for test set
View(pred)

#Lets find the RMSE value
rmse_model <- sqrt(mean((test$Passengers - pred$fit)^2, na.rm = T))
rmse_model


# Exponential Model

model2 <- lm(log_passenger~t, data = train) 
summary(model2)
pred2 <- data.frame(predict(model2, interval = "predict", newdata = test))
View(pred2)

#Lets find the RMSE value
rmse_model2 <- sqrt(mean((test$Passengers - exp(pred2$fit))^2, na.rm = T))
rmse_model2


# Quadratic Model

model3<-lm(Passengers~t+t_square,data=train)
summary(model3)
pred3<-data.frame(predict(model3,interval='predict',newdata=test))
rmse_model3<-sqrt(mean((test$Passengers-pred3$fit)^2,na.rm=T))
rmse_model3

# 4. Additive Seasonality model

model4<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(model4)
pred4<-data.frame(predict(model4,newdata=test,interval='predict'))
rmse_model4<-sqrt(mean((test$Passengers-pred4$fit)^2,na.rm = T))
rmse_model4



#Lets make a Table for the Models and its RMSE value
table_rmse<-data.frame('Model'=c("Linear Regression Model","Exponential Model","Quadratic Model","Additive Seasonality model"),'RMSE'=c(rmse_model,rmse_model2,rmse_model3,rmse_model4))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

multi_lin_model <- lm(log_passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = new_data)

resid <- residuals(multi_lin_model)
resid[1:10]

hist(resid)

acf(resid,lag.max = 10)
X <- arima(resid, order=c(6,0,0))

acf(X$residuals,lag.max = 15)
pred_res<- predict(arima(resid,order=c(1,0,0)),n.ahead = 120)
str(pred_res)
pred_res$pred
acf(X$residuals)

# Build a ARIMA model for whole dataset.

library(tseries)
library(forecast)

#create time series data
airline_times <- ts(airlines$Passengers, frequency = 12, start = c(1995)) 
View(airline_ts)
plot(airline_ts) #Plots the data into a Line chart By default as the data is a Time Series Data

pacf(airline_ts) 

#Lets Find the q-value by acf
acf(airline_ts) 

# build an ARIMA model
arima_model <- arima(airline_ts, order = c(1,1,1), method = "ML")
arima_model

plot(forecast(a,h=5), xaxt = "n")


ab <- auto.arima(airline_ts)

windows()
plot(forecast(ab, h=12), xaxt = "n")
#the forecast was accurate 

prediction <- forecast(ab, h=12) #This will predict for the next 12 months
