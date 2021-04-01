############################################## ###
#  Project    : Homework 04
#  Purpose    : Naive Bayes
#  First Name : Taylor
#  Last Name  : Niedzielski
#  Date       : March 16 2021
#  Comments   : Perform Naive Bayes to classify the dataset.

rm(list=ls())
############################################## ###

# load and format data correctly
csvfile<-file.choose()
breastCancer<-  read.csv(csvfile)
breastCancer$F6 <- as.integer(breastCancer$F6)
breastCancer_missing<-na.omit(breastCancer)

# create training and test sets
idx<-sort(sample(nrow(breastCancer_missing),as.integer(.70*nrow(breastCancer_missing))))
training<-breastCancer_missing[idx,]
test<-breastCancer_missing[-idx,]



library(e1071)

nBayes_all <- naiveBayes(factor(Class)~., data =training[2:11][])

## Naive Bayes classification using all variables 
category_all<-predict(nBayes_all,test[2:11][] )

## Compare the prediction to actual
table(nBayes_all=category_all, Samples=test$Class)

NB_wrong<-sum(category_all!=test$Class)

NB_error_rate<-NB_wrong/length(category_all)
NB_error_rate






