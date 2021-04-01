############################################## ###
#  Project    : Homework 02
#  Purpose    : EDA Analysis
#  First Name : Taylor
#  Last Name  : Niedzielski
#  Date       : March 15 2021
#  Comments   : Perform Exploratory Data Analysis with a data set that has missing values.


rm(list=ls())
############################################## ###

# load and fix data
file<-file.choose()
breastCancer<-  read.csv(file) 
class(breastCancer$F6)
breastCancer$F6 <- as.integer(breastCancer$F6)

# I.	Summarizing each column (e.g. min, max, mean )
summary(breastCancer[,3:11])

# II.	Identifying missing values
missing <- is.na(breastCancer$F6)
missing

# III.	Replacing the missing values with the “mean” of the column.
for(i in 2:ncol(breastCancer)){
  breastCancer[is.na(breastCancer[,i]), i] <- round(mean(breastCancer[,i], na.rm = TRUE))
}
summary(breastCancer)

# IV.	Displaying the frequency table of “Class” vs. F6
table(Class = breastCancer$Class, F6 = breastCancer$F6)

# V.	Displaying the scatter plot of F1 to F6, one pair at a time
pairs(breastCancer[2:7],main = "Breast Cancer vs Class",
      pch = 21,bg =c("red","blue")[factor(breastCancer$Class)])

# VI.	Show histogram box plot for columns F7 to F9
x <- matrix(unlist(breastCancer), ncol = 12, byrow = FALSE)
hist(x[,7:9], xlab = "F7 to F9", main = "Histogram of F7 to F9")


# Delete all objects from R environment and reload Breast Cancer data set
rm(list=ls())

file<-file.choose()
breastCancer<-  read.csv(file) 
breastCancer$F6 <- as.integer(breastCancer$F6)
nrow(breastCancer)

# Remove row with missing value in column
breastCancer_missing<-na.omit(breastCancer)
nrow(breastCancer_missing)







