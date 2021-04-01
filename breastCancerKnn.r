############################################## ###
#  Project    : Homework 02
#  Purpose    : Knn Analysis
#  First Name : Taylor
#  Last Name  : Niedzielski
#  Date       : March 16 2021
#  Comments   : Perform the supervised ML algorithm KNN analysis to develop classification models.

rm(list=ls())
############################################## ###
library(kknn)
# load and format data correctly
file<-file.choose()
breastCancer<-  read.csv(file) 
breastCancer$F6 <- as.integer(breastCancer$F6)
breastCancer_missing<-na.omit(breastCancer)

# create training and test sets
idx<-sort(sample(nrow(breastCancer_missing),as.integer(.70*nrow(breastCancer_missing))))
training<-breastCancer_missing[idx,]
test<-breastCancer_missing[-idx,]

# perform an unweighted KNN analysis for 3 nearest neighbors=
predict_k3 <- kknn(formula=Class ~., training, test, k=3,kernel ="rectangular")
fit <- fitted(predict_k3)
table(Actual= test$Class,Fitted=fit)

#	Measure the performance of KNN
wrong<- ( test$Class!=fit)
rate<-sum(wrong)/length(wrong)
rate


# perform an unweighted KNN analysis for 5 nearest neighbors
predict_k5 <- kknn(formula=Class ~., training, test, k=5,kernel ="rectangular")
fit <- fitted(predict_k5)
table(Actual= test$Class,Fitted=fit)

#	Measure the performance of KNN
wrong<- ( test$Class!=fit)
rate<-sum(wrong)/length(wrong)
rate


# perform an unweighted KNN analysis for 10 nearest neighbors
predict_k10 <- kknn(formula=Class ~., training, test, k=10,kernel ="rectangular")
fit <- fitted(predict_k10)
table(Actual= test$Class,Fitted=fit)

#	Measure the performance of KNN
wrong<- ( test$Class!=fit)
rate<-sum(wrong)/length(wrong)
rate
