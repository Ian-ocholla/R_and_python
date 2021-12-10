#Ian Ocholla
#Data wrangling for Longitudinal Analysis

#* Data taken from Davis 2002: here 40 male subjects were randomly assigned to one 
#* or two treatment groups and each subject was rated on the brief psychiatric rating scale
#* (BPRS) measured before treatment began (week 0) and then at weekly interval for eight weeks. 
#* The BPRS assesses the level of 18 symptoms construct such as hostility, suspiciousness,
#* hallucinations and grandiosity; each of these is rated from one (not presented) to seven (extremely severe)
#* The scale is used to evaluate patients suspected of having schizophrenia.


#Libraries
library(dplyr)
library(tidyr)
library(ggplot2)



#specify URL where file is stored
url1<- "https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt"
url2<- "https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt"




#set file destination in the IODS-Project data folder
destfile1 <- "C:/LocalData/ocholla/IODS-project/Data/BPRS.txt"
destfile2<- "C:/LocalData/ocholla/IODS-project/Data/Rats.txt"




#download the files 
download.file(url1, destfile1)
download.file(url2, destfile2)



#set working directory
setwd("C:/LocalData/ocholla/IODS-project/Data")




#Load the data sets (BPRS)
BPRS<- read.table("BPRS.txt", sep= " ", header = T)

#*Explore the datasets: check their variable names, view the data contents 
#*and structure and create some brief summaries of the variables so that 
#*you understand the point of the wide form data


#check the variable names
names(BPRS)
#The dataset has 11 variables: treatment, subject and Week0-week8 



#check the data set variable
str(BPRS)
#The data has 40 observations of the 11 variables.All the data are integers in format




#print out summaries of the variables
summary(BPRS)
#The bprs mean across the nine weeks varied between 48 and 31.43
#Treatment variable looks like a factor variable as it contains only two values/levels



#convert subject and treatment from integer to factors
BPRS<- within(BPRS, {
  treatment<- factor(treatment)
  subject<- factor(subject)
})




#Convert the data to long form
BPRSL<- gather(BPRS, key = weeks, value = bprs, week0:week8) %>%
  #extract the week number- adding the variable week into the dataset
  mutate(week = as.integer(substr(weeks, 5, 5)))




#take a glimpse at the BPRSL data
glimpse(BPRSL)
head(BPRSL)
tail(BPRSL)
str(BPRSL)
summary(BPRSL)


#export the long form data
write.table(BPRSL, "BPRSL.txt", sep = "\ t", row.names = FALSE)


#* Brief summaries
#* 1. In Long form set, the data set has 360 observation and 5 variables,
#compared to 40 observation and 11 variables in the wide form data.
#* 2. The different week variables in wide form have been condensed under one variable "weeks" under long form.



#**********************
#*Meet and Repeat: part II
#*************************

#* RATS  data from a nutrition study conducted in three groups of rats.
#* the groups were put on different diets and each animals body weight (grams)
#* was recorded repeatedly (approximately) weekly, expect in week seven when two recording were taken)
#* over a 9-week period.



#Load the data sets (BPRS)
RATS<- read.table("Rats.txt", header = TRUE, sep = '\t')



#*Explore the datasets: check their variable names, view the data contents 
#*and structure and create some brief summaries of the variables so that 
#*you understand the point of the wide form data



#check the variable names
names(RATS)
#The dataset has 13 variables: ID, Group and WD1, WD8, WD15, WD22, WD29, WD36,WD43, WD44, WD50, WD57, WD64 



#check the data set variable
str(RATS)
#The data has 16 observations of the 13 variables.All the data are integers in format



#print out summaries of the variables
summary(RATS)
#ID and Group seems like categorical data



#convert variables ID and Group to factors
RATS<- within(RATS, {
  ID<- factor(ID)
  Group<- factor(Group)
})



#Convert data to long form
RATSL<- RATS%>% 
  gather(key = WD, value = Weight, -ID, -Group) %>% 
  #mutate a new variable Time by extracting the number of the day from WD
  mutate(Time = as.integer(substr(WD,3,4)))



#glimpse the data
glimpse(RATSL)
head(RATSL)
tail(RATSL)
str(RATSL)



#export the long form data
write.table(RATSL, "RATSL.txt", sep = "\ t", row.names = FALSE)




#* Brief summaries
#* In Long form, there are 176 observation of 5 variables compared to wide form.
#* The WD values have been condensed into WD variable
#* 
