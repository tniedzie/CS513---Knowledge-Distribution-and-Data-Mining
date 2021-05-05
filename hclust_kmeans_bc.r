#  First Name      : Taylor
#  Last Name       : Niedzielski
#  purpose         : performing hclust and kmeans to categorize data 
#                  : from Wisconsin Breast Cancer data


rm(list=ls())

filename<-file.choose()
bc<-read.csv(filename, na.strings = "?", colClasses=c("diagnosis"="factor" )) 
#bc<-read.csv("/Users/taylorniedzielski/Desktop/cs513/wisc_bc_ContinuousVar.csv",na.strings = "?", colClasses=c("diagnosis"="factor" ))

bc<- na.omit(bc)
head(bc)

########using hclust to categorize wisconsin breast cancer data ##############
#make into distance structure for hclust
bc_dist<-dist(bc[,-2])
hclust_results<-hclust(bc_dist)
plot(hclust_results) 
dev.off()
hclust_2<-cutree(hclust_results,2) 
table(hclust_2,bc$diagnosis) 

########using kmeans to categorize wisconsin breast cancer data ##############
bc_kmeans <- bc[,-2]
View(bc_kmeans)
kmeans_2<- kmeans(bc_kmeans,2,nstart = 10)
kmeans_2$cluster
kmeans_2$centers
table(kmeans_2$cluster,bc$diagnosis)


