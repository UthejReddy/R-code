Orange<-datasets::Orange

head(Orange)
tail(Orange)
Orange[,c(1,2)]
Orange$age
#summary

summary(Orange$age)
summary(Orange$Tree)
summary(Orange$circumference)

#plot

plot(Orange$Tree)
plot(Orange$age)
plot(Orange$circumference)
plot(Orange$Tree,xlab="kjhdsfvj",ylab='wdhcgwgh',main="lkqjdo",)

#barchart
barplot(Orange$age,main = 'mhqsgd',xlab = 'lkjwd',col = 'red',horiz = FALSE)

#histogram
hist(Orange$age)
hist(Orange$age,xlab = 'lkjwd',col = 'red')

#boxplot
boxplot(Orange$circumference)

#multiple boxplot
boxplot(Orange[,1:3],main="multiple")

#combined charts
par(mfrow=c(3,3),mar=c(2,5,2,1),las=0,bty="n")
plot(Orange$Tree)
plot(Orange$age)
plot(Orange$circumference)
plot(Orange$Tree,xlab="kjhdsfvj",ylab='wdhcgwgh',main="lkqjdo",)
boxplot(Orange$circumference)
hist(Orange$age,xlab = 'lkjwd',col = 'red')
boxplot(Orange[,1:3],main="multiple")
