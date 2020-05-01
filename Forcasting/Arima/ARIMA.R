##ARIMA model
plot(train)
acf(train)
pacf(train)
a<-arima(train,order=c(1,1,8),method = "ML")

#Auto.Arima model on the price agg data

library(forecast)
model_AA<-auto.arima(train)#used to directly get the accurate acf and pacf values
model_AA
pred_AA<-data.frame(forecast(model_AA))#forecast the model
pred_AA


acf(model_AA$residuals)#check significant errors in the acf model
pacf(model_AA$residuals)#check significant errors in pacf model

window()
plot(forecast(model_AA,h=12),xaxt="n")#plot the forecasted model
