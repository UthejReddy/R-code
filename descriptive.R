head(airquality)
tail(airquality)
airquality[,c(1,2)]
airquality[,c(5,6)]
airquality$Temp

summary(airquality$Temp)
summary(airquality$Ozone)
summary(airquality$Wind)

plot(airquality$Ozone,airquality$Temp)
plot(airquality)
summary(airquality)

summary(airquality$Temp)

plot(airquality$Ozone,type = "l")
plot(airquality$Ozone,xlab = 'ozone concentration',ylab ='no of instances',main = 'ozone levels in ny city',col='blue' )

#barchart
barplot(airquality$Ozone,main = 'ozone concentration in air',xlab = 'ozone levels',col = 'blue',horiz = FALSE)


#histogram
hist(airquality$Solar.R)
hist(airquality$Solar.R,main = "solar radiation values in air",xlab = "solar rad.",col = "blue")

#single box plot
boxplot(airquality$Solar.R)

#multiple boxplot
boxplot(airquality[,1:4],main="multiple")


par(mfrow=c(3,3),mar=c(2,5,2,1),las=0,bty="n")
plot(airquality$Ozone,airquality$Wind)
plot(airquality$Ozone,type = "c")
plot(airquality$Ozone,type = "s")
plot(airquality$Ozone,type = "h")
barplot(airquality$Ozone,main = "ozone concentration in air",xlab = "ozone levels",col = "green",horiz = TRUE)
hist(airquality$Solar.R)
boxplot(airquality$Solar.R)
boxplot(airquality[,1:4],main="multiple boxplots")
plot(airquality$Month)
