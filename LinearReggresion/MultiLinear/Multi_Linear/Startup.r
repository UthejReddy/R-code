
comp<- read.csv(file.choose())
View(comp)
summary(comp)

cor(comp[,-4])

boxplot(comp)

pairs(comp)

model <- lm(comp$Profit~., data = comp)
summary(model)

#The R-Square value for the above model is 0.9508

plot(model)

install.packages("car")
library(car)

car::vif(model) 

residualPlots(model)

qqPlot(model)

influenceIndexPlot(model)
#Here we got row 46 and 50 as outliers.

#cycle 1
startup1<- comp[c(-46,-50),]
#create a new model
model1 <- lm(startup1$Profit~. , data =startup1)
summary(model1)
#The R-square value is 0.9645
car::vif(model1)
plot(model1)
residualPlots(model1)
qqPlot(model1)

influenceIndexPlot(model1)
#Here we got row 15 and 37 as outliers.

#cycle 2

startup2 <- comp[c(-15,-37),]
model2 <- lm(startup2$Profit~. , data = startup2)
summary(model2)
#The R-square value is 0.9712
car::vif(model2)
plot(model2)
residualPlots(model2)
qqPlot(model2)

#cycle 3

startup3<- comp[c(-46,-44,-48,-50),]
model3 <- lm(startup3$Profit~. , data = startup3)
summary(model3)
#The R-square value is 0.9767

car::vif(model3)
plot(model3)
residualPlots(model3)
qqPlot(model3)

