#Ian ocholla, 09/11/2021

#read the full learning2014 data
lrn14<- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep = "\t",
                   header = TRUE, stringsAsFactors = TRUE)

#Explore the structure and dimensions of the data

#structure of the data
str(lrn14)
#dimension of the data
dim(lrn14)
#184 obs against 60 variables


#Create an analysis dataset with the variables gender, age, attitude, deep stra,
#surf and points by combining questions in the learning 2014 data as defined in 
#the datacamp exercise and also on the bottom part of following page

#print the attitude column vector
lrn14$Attitude
#create column 'attitude' by scaling the column"Attitude"
lrn14$attitude<-(lrn14$Attitude/10)


#install.packages("dplyr")
library(dplyr)

#question related to deep, stra, surf and points by 
#combining questions in the learning2014 data
deep_questions<- c("D03","D11","D19","D22", "D07","D11","D19","D30",
                   "D06","D15","D23","D31")
surface_questions<- c("SU02","SU10","SU18","SU26","SU08","SU16","SU24"
                      ,"SU32","SU05","SU13","SU21","SU29")
strategic_questions<- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")

#select columns related to deep learning and create column 'deep' by averaging
deep_columns<- select(lrn14, one_of(deep_questions))
lrn14$deep<- rowMeans(deep_columns)

#select columns related to surface learning and create column 'surf' by averaging
surface_columns<-select(lrn14, one_of(surface_questions))
lrn14$surf<- rowMeans(surface_columns)

#select columns related to strategic learning and create column 'stra' by averaging
strategic_columns<-select(lrn14, one_of(strategic_questions))
lrn14$stra<- rowMeans(strategic_columns)

#print out the column names of the data
colnames(lrn14)
#change name of Age to age
colnames(lrn14)[57]<- "age"
#change the name of "Points" to "points" using the index
colnames(lrn14)[59]<-"points"

#print out the new column names
head(lrn14)

#exclude observation where the exam point variable is zero
lrn14<-filter(lrn14, points > 0)


keep_columns<- c("gender","age","attitude","deep","stra","surf","points")

#select the keep_columns to create a new dataset
learning2014<- select(lrn14, one_of(keep_columns))

#see the struture of the new dataset
#(The data should have 166 observations and 7 variables)
str(learning2014)

#set the working directory of your R session the IODS project folder
setwd("Z:/Courses/Open data science/R/IODS-project/Data")

#save the analysis datasets to the data folder

write.csv(learning2014, "learning2014.csv")

#read the file uploaded
data2<-read.csv("learning2014.csv", header = TRUE, stringsAsFactors = TRUE)
str(data2)
#using stringAsFactors to convert the gender variable from string to factor

my_model<- lm(points~ surf+stra+age, data = data2)
summary(my_model)