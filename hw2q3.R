# install.packages("class")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(class)



setwd("c:/temp/cpsc375hw2")

# a. Load and pre-process the data. Show code to: 
#   i. Load the data file on Titanium. 
data <- read_csv("data_banknote_authentication.csv")
str(data)

#   ii. How many rows and columns are there? 
nrow(data)
ncol(data)

# b. Split the dataset into train and test datasets with the rows 1, 3, 5, ... for training, and the 
#  remaining rows for test (i.e, test using rows 2, 4, 6, .). Do NOT randomly sample the 
#  data (though resampling is usually done, this hw problem does not use this step for ease 
#  of grading). (code) 

# 1, 3, 5, ... for training
# odd
traindata <- data %>% dplyr::filter(row_number() %% 2 == 1)

# remaining rows for test (i.e, test using rows 2, 4, 6, .)
# even
testdata <- data %>% dplyr::filter(row_number() %% 2 == 0)

# c. Train and test a k-nearest neighbor classifier with the above datasets. Consider only 
#  variance and skewness columns. Set k=1. What is the error rate (number of misclassifications)? (code) 

#only variance and skewness columns 1:2
colsToConsider <- 1:2
k <- 1
trainfeatures <- traindata[colsToConsider]
trainlabels <- factor(traindata$forged)

testfeatures <- testdata[colsToConsider]
testlabels <- factor(testdata$forged)

predictedlabels <- knn(train = trainfeatures, cl = trainlabels, test=testfeatures, k = k)

actualVsPredicted <- table(testlabels, predictedlabels)
actualVsPredicted

errorRate <- sum(actualVsPredicted) - sum(diag(actualVsPredicted))

# d. Repeat part (c) but consider only variance , skewness, and curtosis columns. Set k=1. 
# (show code.) What is the error rate? Will the error rate always decrease with larger 
# number of parameters? Why or why not: answer in 2-3 sentences? 

#consider only variance , skewness, and curtosis columns 1:3
colsToConsider <- 1:3
k <- 1
trainfeatures <- traindata[colsToConsider]
trainlabels <- factor(traindata$forged)

testfeatures <- testdata[colsToConsider]
testlabels <- factor(testdata$forged)

predictedlabels <- knn(train = trainfeatures, cl = trainlabels, test=testfeatures, k = k)

actualVsPredicted <- table(testlabels, predictedlabels)
actualVsPredicted

errorRate <- sum(actualVsPredicted) - sum(diag(actualVsPredicted))

# Will the error rate always decrease with larger number of parameters? 
# answer: (in answer sheet)

# Why or why not: answer in 2-3 sentences?
# answer: (in answer sheet)

# e. Repeat part (d) but set k=5. What is the error rate? 

#consider only variance , skewness, and curtosis columns 1:3
colsToConsider <- 1:3
k <- 5
trainfeatures <- traindata[colsToConsider]
trainlabels <- factor(traindata$forged)

testfeatures <- testdata[colsToConsider]
testlabels <- factor(testdata$forged)

predictedlabels <- knn(train = trainfeatures, cl = trainlabels, test=testfeatures, k = k)

actualVsPredicted <- table(testlabels, predictedlabels)
actualVsPredicted

errorRate <- sum(actualVsPredicted) - sum(diag(actualVsPredicted))
  
# f. Repeat part (e) but set k=11. What is the error rate? Considering your observations from
# (d)-(f), which is the best value for k?

#consider only variance , skewness, and curtosis columns 1:3
colsToConsider <- 1:3
k <- 11
trainfeatures <- traindata[colsToConsider]
trainlabels <- factor(traindata$forged)

testfeatures <- testdata[colsToConsider]
testlabels <- factor(testdata$forged)

predictedlabels <- knn(train = trainfeatures, cl = trainlabels, test=testfeatures, k = k)

actualVsPredicted <- table(testlabels, predictedlabels)
actualVsPredicted

errorRate <- sum(actualVsPredicted) - sum(diag(actualVsPredicted))

# Considering your observations from (d)-(f), which is the best value for k?
# answer: The best value for k is 1

# g. Consider only the ranges of the features - is normalization required? 
# answer: (on answer sheet)

# h. Normalize each column by scaling the minimum-maximum range of each column to 0-1.
# (Hint: the built-in R function scale() can be used for this)  (code) 

colsToConsider <- 1:3
k <- 11
trainfeatures <- traindata[colsToConsider]
trainlabels <- factor(traindata$forged)

testfeatures <- testdata[colsToConsider]
testlabels <- factor(testdata$forged)

trainfeatures$variance.scaled <- scale(trainfeatures$variance)
trainfeatures$skewness.scaled <- scale(trainfeatures$skewness)
trainfeatures$curtosis.scaled <- scale(trainfeatures$curtosis)

testfeatures$variance.scaled <- scale(testfeatures$variance)
testfeatures$skewness.scaled <- scale(testfeatures$skewness)
testfeatures$curtosis.scaled <- scale(testfeatures$curtosis)

# adjust the cols to use the scaled columns
colsToConsider <- 4:6
trainfeatures <- trainfeatures[colsToConsider]
testfeatures <- testfeatures[colsToConsider]

# i. Train and test a k-nearest neighbor classifier with the normalized dataset. Consider only
# variance, skewness, and curtosis columns. Set k=1. What is the error rate?
predictedlabels <- knn(train = trainfeatures, cl = trainlabels, test=testfeatures, k = k)

actualVsPredicted <- table(testlabels, predictedlabels)
actualVsPredicted

errorRate <- sum(actualVsPredicted) - sum(diag(actualVsPredicted))
  