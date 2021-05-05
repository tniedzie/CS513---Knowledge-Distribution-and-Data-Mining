#  First Name       : Taylor
#  Last Name        : Niedzielski   
#  Purpose          : Create an ANN analysis for Breast Cancer dataset 
#  Creation date    : May 2 2021

rm(list=ls())

filename<-file.choose()
bc<-read.csv(filename, na.strings = "?", colClasses=c("diagnosis"="factor" )) 
#bc<-read.csv("/Users/taylorniedzielski/Desktop/cs513/wisc_bc_ContinuousVar.csv",na.strings = "?", colClasses=c("diagnosis"="factor" ))

##clean data##
bc<-na.omit(bc)
head(bc)
bc2<-data.frame(lapply(na.omit(bc),as.numeric))

###training and testing datasets####
index<-sort(sample(nrow(bc2 ),round(.30*nrow( bc2))))
training<- bc2[-index,]
test<- bc2[index,]

index <- seq (1,nrow(bc2),by=5)
test<- bc2[index,]
training<-bc2[-index,]


###### create Neural Net ########
library("neuralnet")
class(training$diagnosis)
net_bc2<- neuralnet( diagnosis~.,training[-1], hidden=5, threshold=0.01)

#Plot the neural network
plot(net_bc2)


ann <-compute(net_bc2 , test)
ann$net.result 

ann_cat<-ifelse(ann$net.result <1.5,1,2)
length(ann_cat)

table(Actual=test$diagnosis,predition=ann_cat)

########error Rate#######
wrong<- (test$diagnosis!=ann_cat)
error_rate<-sum(wrong)/length(wrong)
error_rate

