
data <- read.csv(file.choose())

summary(data)

model <- lm(data$price~. , data = data)
summary(model)
#The r-Square value of this model is 0.7778

pred <- predict(model)

computer <- data.frame(data$price, pred)

#log transformation
model1<- lm(log(data$price)~. , data = data)

summary(model1)
#here R-squared value is 0.7848
pred1 <- predict(model1)

computer_data <- data.frame(data$price, pred1)

