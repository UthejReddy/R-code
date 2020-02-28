#install package "arules"
install.packages("arules")
library(arules)#import the given package

Titanic<-Titanic[,-c(1)]#remove 1st coloumn from the dataset

rules<-apriori(Titanic)#apply the algorithm

arules::inspect(rules)#shows the pattern formed by algorithm

rules.sorted<-sort(rules,by="lift")#sorts the data according to the list ratio

arules::inspect(rules.sorted)#shows the sorted list

#rules with rhs containing "Survived" only
#show the number of people survived
rules<-apriori(Titanic,parameter = list(minlen=1,supp=0.1,conf=0.5),appearance=list(rhs=c("Survived=No","Survived=Yes")),control=list(verbose=F))

arules::inspect(rules)
