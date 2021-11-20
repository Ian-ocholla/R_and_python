#Ian Ocholla, 15.11.2021, Rscript on logistic regression
#data are from two identical questionnaires related to secondary school student 
#alcohol consumption in portugal

#Mat-mathematics and por- portuguese language

source<- "https://archive.ics.uci.edu/ml/machine-learning-databases/00320/student.zip"
dest<- "Z:/Courses/Open data science/R/IODS-project/Data/student.zip"

#Load Data from the web and unzip it
#set directory
setwd("C:/LocalData/ocholla/IODS-project")
download.file(source, dest)
unzip(dest, exdir = "~/IODS-project/Data/student")

#read the csv files

math<- read.csv("student-mat.csv", sep = ";",header = TRUE)
por<-read.csv("student-por.csv",sep = ";", header = TRUE)

#explore the dataset
str(math)
str(por)
dim(math)
dim(por)

#look at the column names of both data
colnames(math)
colnames(por)

#columns that can not be used as identifiers
# failures, paid, absence, G1, G2, G3

#Define own ID for both datasets
library(dplyr)
por_id<- por%>% mutate (id=1000+row_number())
math_id<-math%>%mutate(id=2000+row_number())

#which columns vary in dataset
free_cols<- c("id","failures","paid","absences","G1","G2","G3")

#The rest of the columns are common identifiers used for joining the datasets
join_cols<- setdiff(colnames(por_id),free_cols)
pormath_free<- por_id%>%bind_rows(math_id)%>%select(one_of(free_cols))

#combine datsets to one long data
pormath<- por_id %>%
  bind_rows(math_id) %>%
  #aggregate data (more joining variables than in the example)
  group_by(.dots=join_cols) %>%
  #calculating required variables from two obs
  summarise(
    n=n(),
    id.p=min(id),
    id.m=max(id),
    failures=round(mean(failures)), #Rounded mean for numerical
    paid=first(paid), #and first for chars
    absences=round(mean(absences)),
    G1=round(mean(G1)),
    G2=round(mean(G2)),
    G3=round(mean(G3))
  )%>%
  #Remove lines that do not have exactly one obs from both datasets
  #There must be exactly 2 observations found in order to joining be successful
  #In addition, 2 obs to be joined must be 1 from por and 1 from math
  filter(n==2, id.m-id.p>650)%>%
  #join original free fields, because rounded means r fiest values may not be relevant
  inner_join(pormath_free, by=c("id.p"="id"), suffix=c("",".p"))%>%
  inner_join(pormath_free, by=c("id.m"="id"), suffix=c("",".m")) %>%
  
  
  ungroup %>% mutate(
    #the average of weekday and weekend alcohol consumption to create a new column 'alc_use'
    alc_use = (Dalc + Walc)/2,
    #using the created 'alc_use'  to create a new logical column 'high_use' which 
    #is TRUE for students which 'alc_use' is greater than 2
    high_use=alc_use>2,
    cid=3000+row_number()
  )

#structure of the new data
str(pormath)
#370 observations 

#save created data as an excel worksheet
library(openxlsx)
write.csv(pormath, "pormath.csv", row.names = FALSE)


