
library(tidyverse)
library(ggplot2)

setwd("c:/temp/cpsc375hw2")

# a. Load the autompg.csv file on Titanium and convert cylinders variable to a factor. 
# (code, output of str()) 

data <- read_csv("autompg.csv")

cylinderData <- factor(data$cylinders)

str(cylinderData)

# b. Which is the dependent variable? Which are the independent variables? 


# c. Plot mpg vs. displacement (code, plot)
g <- ggplot(data = data)
p <- g +
  geom_point(mapping = aes(x = mpg, y = displacement))

pdf("hw2q1c_graph.pdf")
print(p)
dev.off()

p

# d. Create a linear model of mpg vs. displacement (only one independent variable). 
# What is the R2? (code, output of summary(model), R2 value) 

