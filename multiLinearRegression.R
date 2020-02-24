#show the relation between parameter in graphical way 
pairs(Cars)

cor(Cars)#showcase the correlation between parameter


#Regression model and summary
model.car<-lm(MPG~.,data = Cars)
summary(model.car)

#multi-collinearity
install.packages("car")
library(car)
car::vif(model.car)
#diagnostic plots:
#REsidual plots,QQ-plots,sta.Residual vs Fitted
plot(model.car)
#Residual vs Regressors
residualPlots(model.car)

#Added variable plots
#avPlots(model.car,id.n=2,id.cex=0.7)
#QQ plots of studentized residuals
qqPlot(model.car)

#deletion Diagnostics
influenceIndexPlot(model.car)


#iteration 1
#remove 77th observation
Cars1<-Cars[-77,]
model1<-lm( Cars1$MPG~.,data=Cars1)
model1
car::vif(model1)
plot(model1)

residualPlots(model1)
qqPlot(model1)
influenceIndexPlot(model1)

#same process with outlier 79
Cars2<-Cars[-c(77,79),]
model2<-lm(Cars2$MPG~.,data = Cars2)
car::vif(model2)
plot(model2)
residualPlots(model2)
qqPlot(model2)
influenceIndexPlot(model2)
