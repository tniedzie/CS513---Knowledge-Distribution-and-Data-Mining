
#########################################################

## Taylor Niedzielski
##           
## Purpose: Use the CART methodology to develop a classification model for the Diagnosis.  
#########################################################

library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer)     # colors needed for rattle


#bc<-read.csv("/Users/taylorniedzielski/Desktop/cs513/breast-cancer-wisconsin.data.csv",
#                     na.strings = "?", colClasses=c("Class"="factor" ))
filename<-file.choose()
bc<-read.csv(filename )


head(bc)
bc2<-na.omit(bc)
set.seed(111)


index<-sort(sample(nrow(bc2),round(.25*nrow(bc2))))
training<-bc2[-index,]
test<-bc2[index,]


#Grow the tree
CART_class<-rpart( Class~.,data=training, method = "class")
#rpart.plot(CART_class)
library(rpart.plot)
prp(CART_class)
# fancy graph
fancyRpartPlot(CART_class)
CART_predict2<-predict(CART_class,test, type="class") 
table(Actual=test[,-1],CART=CART_predict2)


#get error
CART_predict<-predict(CART_class,test, type="class")
CART_wrong<-sum(test[,11]!=CART_predict)
CART_error_rate<-CART_wrong/length(test[,11])
CART_error_rate




