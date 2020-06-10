#install package "arules"
install.packages("arules")

install.packages("arulesViz")
library(arulesViz) 

library(arules)#import the given package

movies<-read.csv(file.choose())


#apply the algorithm

rule1 <- apriori(as.matrix(movies[,6:15]), parameter = list(supp = 0.05, conf = 0.8))
rule1 

inspect(head(rule1))

toprules <- head(rule1, by = "confidence", n=20)

rule_lift <- sort(rule1, by = "lift", decreasing = TRUE)
rule_conf <- sort(rule1, by = "confidence", decreasing = TRUE)
plot(rule_conf, method = "grouped")

plot(toprules, method = "graph", engine = "htmlwidget")

#rule_2

rule2 <- apriori(as.matrix(movies[,6:15]), parameter = list(supp = 0.05, conf = 0.8))
rule2 

inspect(head(rule2))

toprules <- head(rule2, by = "confidence", n=20)

rule2_lift <- sort(rule2, by = "lift", decreasing = TRUE)
rule2_conf <- sort(rule2, by = "confidence", decreasing = TRUE)
plot(rule2_conf, method = "grouped")

plot(toprules, method = "graph", engine = "htmlwidget")

#rule_3
rule3 <- apriori(as.matrix(movies[,6:15]), parameter = list(supp = 0.05, conf = 0.8))
rule3 

inspect(head(rule3))

toprules <- head(rule3, by = "confidence", n=20)

rule3_lift <- sort(rule3, by = "lift", decreasing = TRUE)
rule3_conf <- sort(rule3, by = "confidence", decreasing = TRUE)
plot(rule3_conf, method = "grouped")

plot(toprules, method = "graph", engine = "htmlwidget")



