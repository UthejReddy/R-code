

#upload dataset flights 
data<-nycflights13::flights

#create a seperate dataset dep_delay and ar_delay from original dataset
dep_delay<-data$dep_delay
ar_delay<-data$arr_delay

library(gmodels)#package istalled

dep_delay1<-dep_delay[!is.na(dep_delay)]#comand is used to remove the NA values
ci(dep_delay1)#confidential interval applied to showcase flights departure delay 

ar_delay1<-ar_delay[!is.na(ar_delay)]
ci(ar_delay1)#confidential interval applied to showcase flights arrival delay 

