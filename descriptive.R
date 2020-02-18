head(airquality)#shows first 6 data
tail(airquality)#shows last 6 data
airquality[,c(1,2)]
airquality[,c(5,6)]
airquality$Temp

summary(airquality$Temp)#shows the mean,median and mode of coloumn Temp
summary(airquality$Ozone)#shows the mean,median and mode of coloumn Ozone
summary(airquality$Wind)#shows the mean,median and mode of coloumn Wind

plot(airquality$Ozone,airquality$Temp)#plots the graph for coloumn Ozone and Temp
plot(airquality))#plots the graph for dataset airquality
summary(airquality))#shows the mean,median and mode for dataset airquality


plot(airquality$Ozone,type = "l")#plots the graph for coloumn Ozone with type "l"
plot(airquality$Ozone,xlab = 'ozone concentration',ylab ='no of instances',main = 'ozone levels in ny city',col='blue' )#plots the graph for coloumn Ozone and labbeling the x-axix & y-axix

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
