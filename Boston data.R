#Ian Ocholla, Date: 25.11.2021
#Clustering

library(MASS)
library(corrplot)
library(tidyr)
library(ggplot2)


#Housing values in suburbs of Boston
#Data frame has 506 rows and 14 columns

#show graphical overview of the data and show summaries of the variables
#*in the data. Describe and interpret the outputs, commenting on the 
#*distributions of the variables and the relationship between them

#explore the dataset
str(Boston)
summary(Boston)
#plot matrix of the variables
pairs(Boston)

#create a correlation matrix to show the correlation 
#between variables in the data
cor_matrix<- cor(Boston)%>% round(digits = 2)
#print the correlation matrix
cor_matrix
#visualize the correlation matrix
corrplot(cor_matrix, method="circle",type= "upper",
         cl.pos="b", tl.pos="d",tl.cex=0.6)

#* standardize the dataset and print out summaries of the scaled data
#* In scaling, we subtract the column means from the corresponding columns and 
#* divide the difference with standard deviation
#* 
#* center and standardize variables
boston_scaled<- scale(Boston)
#summaries of the scaled variables
summary(boston_scaled)
#* How did the variables change? 
#class of the boston_scaled object
class(boston_scaled)
#change the object to data frame
boston_scaled<- as.data.frame(boston_scaled)

#create a categorical variable of the 
#* crime rate in the Boston dataset 7from the scaled crime rate)
#summary of the scaled crime rate
summary(boston_scaled$crim)
#create a quantile vector of crim and print it
bins<- quantile(boston_scaled$crim)
bins
#create categorical variable 'crime'Use the quantiles as the break points in the categorical variable
crime<- cut(boston_scaled$crim, breaks= bins, include.lowest= TRUE, labels=c("low","med_low","med_high","high"))

#look at the table of the new factor crime
table(crime)
#* remove the old crime rate variable from the dataset.
boston_scaled<- dplyr::select(boston_scaled, -crim)
#add the new categorical value to scaled data

boston_scaled<- data.frame(boston_scaled, crime)

# Divide the dataset to train (80%) and test sets (20%)

#number of rows in the Boston dataset
n<-nrow(boston_scaled)
#choose randomly 80% of the rows
ind<- sample(n, size=n * 0.8)
#create train set
train<- boston_scaled[ind,]
#create test set
test<- boston_scaled[-ind,]
#save the correct classes from test data
correct_classes<- test$crime
#remove the crime variable from test data
test<- dplyr::select(test, -crime)

#******************Linear Discriminant Analysis************* 
#*LDA- is a clasification( and dimension reduction) method. It finds the (linear)
#*combination of the variable that separate the target variable classes

#*Fit the liner discriminant analysis on the train set.
#*  use the categorical crime rate as the target variable
#*  and all other variables in the dataset as predictor variable
lda.fit<- lda(crime~., data= train)
# print the lda.fit object
lda.fit
#*  Draw the LDA (bi)plot

#create function for lda biplor arrows
lda.arrows<- function(x, myscale=1, arrow_heads=0.1, color="orange",
                      tex= 0.75, choices=c(1,2)){
  heads<- coef(x)
  arrows(x0 =0, y0=0,
         x1= myscale * heads[,choices[1]],
         y1= myscale* heads[,choices[2]], col = color, length=
           arrow_heads)
  text(myscale*heads[,choices], labels=row.names(heads),
                     cex= tex, col=color, pos=3)
}
#target classes as numeric 
classes<- as.numeric(train$crime)
#plot the lda results
plot(lda.fit, dimen=2, col=classes, pch= classes)
lda.arrows(lda.fit, myscale=1)

#*  save the crime categories from the test set and then
#save the correct classes from test data
#correct_classes<- test$crime
#*  remove the categorical crime variable from the test dataset.
#test<- dplyr::select(test, -correct_classes)
  
  
#*  then predict the classes with the LDA model on the test data
lda.pred<- predict(lda.fit, newdata=test)
#*  cross tabulate the results with the crime categories from the test
#*  set.
table(correct= correct_classes, predicted= lda.pred$class)
# Comment on the results
#*  

#euclidean distance matrix
dist_eu<- dist(Boston)
#look at the summary of the distances
summary(dist_eu)
#manhattan distance matrix
dist_man<- dist(Boston, method = 'manhattan')
#summary of the distances
summary(dist_man)

#Towards clustering:distance measures (we did not do
#*  this in the datacamp exercise, but you should scale the variables to get comparable distances)
#*  calculate the distance between the observations. 
#*  run k-means
#*  algorithm on the data set. investigate what is the optimal number of clusters and run the algorithm again.
#*  Visualize the clusters (for example with the pairs() or ggpairs() functions,
#*  where the clusters are separated with colors) and interpret the results.


#Kmeans
km<- kmeans(Boston, centers = 3)
#plot the Boston dataset with clusters
pairs(Boston[6:10], col=km$cluster)

#determine the optimal number of k means
#* look at how the total within cluster sum of squares (WCSS)
#* behaves when the number of cluster changes.
#* when you plot the number pf clusters and the total WCSS, the optimal number 
#* of clusters is when the total WCSS drops radically
#* K-means might produce different results every time, because it randomly
#* assigns the initial cluster centers. the function set.seed8) can be used to deal with that

set.seed(123) #to avoid random
#determine the number of clusters
k_max<-10
#calculate the total within sum of squares
twcss<- sapply(1:k_max, function(k)
  {kmeans(Boston, k)$tot.withinss})
#visualize the results
qplot(x = 1:k_max, y= twcss, geom= 'line')
#k-means clustering
km<- kmeans(Boston, centers= 2)
#plot the Boston dataset with clusters
pairs(Boston[6:10],col = km$cluster)


#*  Bonus: perform k-means on the original Boston data with some
#*  reasonable number of clusters (>2). remember to standardize the
#*  dataset. then perform LDA using the clusters as target classes. include all
#*  the variables in the boston data in the LDA model

#Reload the Boston datset and standardize the dataset
library(MASS)
data("Boston")

#* center and standardize variables
boston_scaled2<- scale(Boston)
#summaries of the scaled variables
summary(boston_scaled2) 

#k-means clustering
km<- kmeans(boston_scaled2, centers= 5)
summary(km)
#plot the Boston dataset with clusters, vary the number of pairs
pairs(boston_scaled2,col = km$cluster)

#add the clusters into the boston dataset
boston_scaled2<- data.frame(boston_scaled2, km$cluster)

#Create training and test datasets
#number of rows in the Boston dataset
n<-nrow(boston_scaled2)
#choose randomly 80% of the rows
ind<- sample(n, size=n * 0.8)
#create train set
train2<- boston_scaled2[ind,]
#create test set
test2<- boston_scaled2[-ind,]

#*perform LDA using the clusters as target classes
#Fit the linear discriminant analysis on the train set.
lda.fit<- lda(km.cluster~., data= train2)
# print the lda.fit object
lda.fit

#*****Draw the LDA (bi)plot
#create function for lda biplor arrows
lda.arrows<- function(x, myscale=1, arrow_heads=0.1, color="orange",
                      tex= 0.75, choices=c(1,2)){
  heads<- coef(x)
  arrows(x0 =0, y0=0,
         x1= myscale * heads[,choices[1]],
         y1= myscale* heads[,choices[2]], col = color, length=
           arrow_heads)
  text(myscale*heads[,choices], labels=row.names(heads),
       cex= tex, col=color, pos=3)
}
#target classes as numeric 
classes<- as.numeric(train2$km.cluster)
#plot the lda results
plot(lda.fit, dimen=2, col=classes, pch= classes)
lda.arrows(lda.fit, myscale=1)

###############################################################
#******************supper bonus********************************

#* Run the code below for the (scaled ) train data that you used to fit the LDA
#* The code creates a matrix product which is a projection of the data points
model_predictors<- dplyr::select(train,-crime)
#check the dimensions
dim(model_predictors)
dim(lda.fit$scaling)
#matrix multiplication
matrix_product<- as.matrix(model_predictors)%*%
  lda.fit$scaling
matrix_product<-as.data.frame(matrix_product)

library(plotly)
#create a 3D plot of the columns of the matrix product 
plot_ly(x= matrix_product$LD1, y= matrix_product$LD2, 
        z= matrix_product$LD3, type='scatter3d', mode='markers')

