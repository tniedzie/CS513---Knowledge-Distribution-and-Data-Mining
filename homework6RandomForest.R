#########################################################
##  Taylor Niedzielski
##          
##  Purpose: Create random forest classification
#########################################################

rm(list=ls())

filename<-file.choose()
bc<-read.csv(filename ) 
#bc<-read.csv("/Users/taylorniedzielski/Desktop/cs513/breast-cancer-wisconsin.data.csv",
 #            na.strings = "?", colClasses=c("Class"="factor" ))

dev.off()

# remove all the records with missing value
bc2<-na.omit(bc)
head(bc2)
set.seed(111)

# create training and test sets
index<-sort(sample(nrow(bc2),round(.25*nrow(bc2))))
training<-bc2[-index,]
test<-bc2[index,]

###########create Random Forest Plot##########
library(randomForest)

fit <- randomForest( Class~., data=training, importance=TRUE, ntree=100)
importance(fit)
varImpPlot(fit)
Prediction <- predict(fit, test)
table(actual=test[,11],Prediction)

########error rate#########
wrong<- (test[,11]!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate 





