#########################################################
##  Taylor Niedzielski
##          
##  Purpose: Create C50 classification tree
#########################################################


rm(list=ls())

filename<-file.choose()
bc<-read.csv(filename) 
#bc<-read.csv("/Users/taylorniedzielski/Desktop/cs513/breast-cancer-wisconsin.data.csv",
 #            na.strings = "?", colClasses=c("Class"="factor" ))


### remove all the records with missing value
bc2<-na.omit(bc)
head(bc2)
set.seed(111)

#create training and test sets
index<-sort(sample(nrow(bc2),round(.25*nrow(bc2))))
training<-bc2[-index,]
test<-bc2[index,]

 
library('C50')
C50_class <- C5.0( Class~.,data=training )
summary(C50_class)

plot(C50_class)
C50_predict<-predict( C50_class ,test , type="class" )
table(actual=test[,11],C50=C50_predict)

# get error rate
wrong<- (test[,11]!=C50_predict)
c50_rate<-sum(wrong)/length(test[,11])
c50_rate






