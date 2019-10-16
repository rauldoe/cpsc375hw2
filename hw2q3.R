# install.packages("class")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(class)



setwd("c:/temp/cpsc375hw2")

# a. Load and pre-process the data. Show code to: 
#   i. Load the data file on Titanium. 
data <- read_csv("data_banknote_authentication.csv")

#   ii. How many rows and columns are there? 
nrow(data)
ncol(data)

# b. Split the dataset into train and test datasets with the rows 1, 3, 5, ... for training, and the 
#  remaining rows for test (i.e, test using rows 2, 4, 6, .). Do NOT randomly sample the 
#  data (though resampling is usually done, this hw problem does not use this step for ease 
#  of grading). (code) 
traindata <- data %>% dplyr::filter(row_number() %% 2 == 1)
testdata <- data %>% dplyr::filter(row_number() %% 2 == 0)

# c. Train and test a k-nearest neighbor classifier with the above datasets. Consider only 
#  variance and skewness columns. Set k=1. What is the error rate (number of 
#  misclassifications)? (code) 
trainfeatures <- traindata[1:4]
trainlabels <- traindata['forged']
predictedlabels <- knn(train = trainfeatures, cl = trainlabels, test=testdata, k = 1)