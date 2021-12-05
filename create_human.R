# Ian Ocholla
# Data wrangling 

# read datasets
hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)
gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

#Description of the datasets
#* Human Development Index (HDI) was created to empahsize that people and theor cpabilities should be
#* the ultimate criteria for assessing the development of a country, not economic growth alone.
#* HDI summarises average achievement in key dimensions of human development: a long and heathly life, 
#* being knowledgeable and have a decoent standard of living. More information can be found in the link
#* 
#* Gender Inequality Index- reflects gender based disadvantage in three dimensions-reproductive health, 
#* empwerment and labour market for as many cuntires as data of reasonable quality allow.
#*  Its shows the loss in potential human development due to inequality between female and male achievements in these dimensions.
#*  It often ranges between 0, where women and men are fare equally, and 1 where one gender fares as poorly as possible in all meaured dimensions


# explore datasets
str(hd)
dim(hd)
#create summaries of the variables
summary(hd)

str(gii)
dim(gii)
#create summaries of the variables
summary(gii)

# rename variables with shorter descriptive names
library(tidyverse)
hd <- hd %>% rename( 
  HDI.Rank = HDI.Rank, 
  Country = Country, 
  HDI = Human.Development.Index..HDI., 
  Life.Exp =  Life.Expectancy.at.Birth, 
  Edu.Exp = Expected.Years.of.Education, 
  Edu.Mean = Mean.Years.of.Education,
  GNI = Gross.National.Income..GNI..per.Capita, 
  GNI.MINUS.Rank = GNI.per.Capita.Rank.Minus.HDI.Rank)

#view the new colum heads
colnames(hd)

gii <- gii %>% rename(
  GII.Rank = GII.Rank,
  Country = Country,
  GII = Gender.Inequality.Index..GII.,
  Mat.Mor = Maternal.Mortality.Ratio,
  Ado.Birth = Adolescent.Birth.Rate,
  Parli.F = Percent.Representation.in.Parliament,
  Edu2.F = Population.with.Secondary.Education..Female.,
  Edu2.M = Population.with.Secondary.Education..Male.,
  Labo.F = Labour.Force.Participation.Rate..Female.,  
  Labo.M = Labour.Force.Participation.Rate..Male.)

#view the new column head names
colnames(gii)

# mutate gii data to create two new variables
# First one - ratio of female and male population with secondary education in each country (i.e edu2F/edu2M)
# Second one - ratio of labour force participation of females and males in each country (i.e. labF/labM)
gii <- mutate(gii, Edu2.FM = Edu2.F/Edu2.M, Labo.FM = Labo.F/Labo.M)

# join datasets
#Join dataset using the variable Country as the identifier
human <-inner_join(gii, hd, by = "Country")
#view the new dataset
str(human)
dim(human) 
# 195*19

#set the workdirectory
setwd("C:/LocalData/ocholla/IODS-project/Data")
#save the new dataset as a csv file
write.csv(human, file = "human.csv", row.names = FALSE)



#**************************Data wrangling Part 2
#set the workdirectory
setwd("C:/LocalData/ocholla/IODS-project/Data")
#Load the human data 
human<- read.csv("human.csv", header = TRUE, stringsAsFactors = TRUE)

#Explore the strucuture and the dimensions of the data and describe the dataset briefly.

#look at the column names of human data
names(human)
#look at the struture
str(human)
#summaries of the variable
summary(human)

#Mutate the data: transform the GNI variable to numeric (using string manipulation)
library(stringr)

#Manipulation of GNI factor to numeric
str(human$GNI)
human$GNI<- as.numeric(human$GNI)
str(human$GNI)

#2. Exclude unneeded variables
#columns to keep
keep<- c("Country", "Edu2.FM", "Labo.FM","Edu.Exp", "Life.Exp","GNI","Mat.Mor",
         "Ado.Birth","Parli.F")

library(dplyr)
#select the keep columns
human<- dplyr::select(human,one_of(keep))
#print out a completeness indicator of the human data
complete.cases(human)
#print out the data along with a completeness indicator as the last column
data.frame(human[-1], comp= complete.cases(human))
#3. Remove all rows with missing values (NA values)
human_<- filter(human, complete.cases(human))

#look at the data structure and dimension of the new data frame
str(human_)
#162 observation and 9 variables

#4. Remove the observation which relate to regions instead of countries.

#look at the last 10 observations
tail(human_, 10)
#last indice we want to keep
#The regions are found in the last 7 rows
last<- nrow(human_)-7
#choose everything until the last 7 observations
human_<-human_[1:last, ]
#add countries as rownames
rownames(human)<- human$Country

#remove the Country variable
human_<- dplyr::select(human_, -Country)

#check the strcuture of the new data
str(human_)
# 155 observations of 8 variables

#save the new data including the row names
write.csv(human_, file = "human.csv", row.names = TRUE)

