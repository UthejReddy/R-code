#random forest for company dataset
company_RF<-read.csv(file.choose())

install.packages("caret",dependencies = TRUE)

install.packages("randomForest")

install.packages("gmodel")


library(randomForest)
library(gmodels)
library(caret)

summary(company_RF)

#standard deviation
sd(company_RF$Sales)
sd(company_RF$CompPrice)
sd(company_RF$Income)
sd(company_RF$Advertising)
sd(company_RF$Population)
sd(company_RF$Price)
sd(company_RF$Age)
sd(company_RF$Education)

#variance
var(company_RF$Sales)
var(company_RF$CompPrice)
var(company_RF$Income)
var(company_RF$Advertising)
var(company_RF$Population)
var(company_RF$Price)
var(company_RF$Age)

#graphical visualization of all columns
ggplot(company_RF) + geom_histogram(aes(Sales),binwidth = 0.5, color = "cyan")

ggplot(company_RF) + geom_histogram(aes(CompPrice),binwidth = 0.5, color = "cyan")

ggplot(company_RF) + geom_histogram(aes(Income),binwidth = 0.5, color = "cyan")

ggplot(company_RF) + geom_histogram(aes(Advertising),binwidth = 0.5, color = "cyan")

ggplot(company_RF) + geom_histogram(aes(Population),binwidth = 0.5, color = "cyan")

ggplot(company_RF) + geom_histogram(aes(Price),binwidth = 0.5, color = "cyan")

ggplot(company_RF) + geom_histogram(aes(Age),binwidth = 0.5, color = "cyan")

#here sales column is dependent variable
#convert the sales data into categorical
catsales<-cut(company_RF$Sales,breaks = c(0,5,10,15,20),labels = c("A","B","C","D"),right = FALSE)

company_RF$Sales[1:20]

catsales[1:20]

#combine original data and categorical data
combine<-cbind(company_RF[,-1],catsales)


catsales <- as.factor(catsales)

#create random train and test data
datapart<-createDataPartition(combine$catsales,p=0.60,list = FALSE)
train_com<-combine[datapart,]
test_com<-combine[-datapart,]


#create a randomforest model for 500 trees
model<-randomForest(catsales~.,data = combine,ntree=500)

print(model)

print(importance(model))#it will show the columns which are important

pred<-predict(model,combine[,-11])
pred

a<- table(combine[,11],pred)

acc<-sum(diag(a))/sum(a)

acc


#model for training dataset
model2<-randomForest(catsales~.,data = train_com,ntree=100)

print(model2)

print(importance(model2))

pred2<-predict(model2,test_com[,-11])
pred2

a2<- table(test_com[,11],pred2)

acc2<-sum(diag(a2))/sum(a2)

acc2
#here we get accurary as 71%

#create model for different number of trees
model3<-randomForest(catsales~.,data = train_com,ntree=500)

print(model3)

print(importance(model3))

pred3<-predict(model3,test_com[,-11])
pred3

a3<- table(test_com[,11],pred3)

acc3<-sum(diag(a3))/sum(a3)

acc3
#here we get accuracy as 73%

