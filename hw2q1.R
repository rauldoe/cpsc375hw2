
library(tidyverse)
library(ggplot2)
library(modelr)

setwd("c:/temp/cpsc375hw2")

# a. Load the autompg.csv file on Titanium and convert cylinders variable to a factor. 
# (code, output of str()) 

data <- read_csv("autompg.csv")

cylinderData <- factor(data$cylinders)

data$cylinders <- cylinderData

str(cylinderData)

# b. Which is the dependent variable? Which are the independent variables? 


# c. Plot mpg vs. displacement (code, plot)
g <- ggplot(data = data)
p <- g +
  geom_point(mapping = aes(x = displacement, y = mpg))

pdf("hw2q1c_graph.pdf")
print(p)
dev.off()

p

# d. Create a linear model of mpg vs. displacement (only one independent variable). 
# What is the R2? (code, output of summary(model), R2 value) 
# Residual standard error: 4.651 on 396 degrees of freedom
mod1 <- lm(mpg ~ displacement, data = data)
# or coef(mod1)
c1 <- mod1$coefficients

p1 <- p + geom_abline(slope = c1[2], intercept = c1[1])

p1

summary(mod1)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  35.174750   0.491824   71.52   <2e-16 ***
#   displacement -0.060282   0.002239  -26.93   <2e-16 ***

# Residual standard error: 4.651 on 396 degrees of freedom
# Multiple R-squared:  0.6467,	Adjusted R-squared:  0.6459 
# F-statistic:   725 on 1 and 396 DF,  p-value: < 2.2e-16


# e. Create a new transformed variable that is sqrt(displacement). 
# Create a linear model of mpg vs. sqrt(displacement).
# i. Give R code, output of summary(model)
data$sqrt_displacement <- sqrt(data$displacement)
mod2 <- lm(mpg ~ sqrt_displacement, data = data)

summary(mod2)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       47.22067    0.85874   54.99   <2e-16 ***
#   sqrt_displacement -1.76569    0.06175  -28.60   <2e-16 ***

# Residual standard error: 4.47 on 396 degrees of freedom
# Multiple R-squared:  0.6737,	Adjusted R-squared:  0.6729 
# F-statistic: 817.7 on 1 and 396 DF,  p-value: < 2.2e-16

# ii. Is this a better fit than in part (d)?
# yes because R-squared2 = 0.6737 > R-squared1 = 0.6467

# iii. Plot mpg vs. sqrt(displacement) and overlay the best fit model as a straight line. (code, plot)

# or coef(mod2)
c2 <- mod2$coefficients

g <- ggplot(data = data)
psqrt <- g + geom_point(mapping = aes(x = sqrt_displacement, y = mpg))
p2 <- psqrt + geom_abline(slope = c2[2], intercept = c2[1])

p2

# iv. Plot mpg vs. displacement and overlay the best fit model as a curve. (code, plot) 
# [Hint: plot the predictions; use add_predictions() and geom_line(). You don't have to use data_grid()]
mod1 <- lm(mpg ~ displacement, data = data)
# or coef(mod1)
c1 <- mod1$coefficients

dataResid <- data %>% add_residuals(mod1)
View(dataResid)
g3Resid <- ggplot(data = dataResid) + geom_histogram(mapping = aes(x=resid))
g3Resid

# using best model = mod2
# mygrid3 <- data %>% data_grid(mod2)
mygrid3 <- data
mygrid3 <- mygrid3 %>% add_predictions(mod2) 
g3 <- ggplot(data = data)
p3 <- g3 + geom_point(mapping = aes(x = displacement, y = mpg))

p3 <- p3 + geom_line(data = mygrid3, mapping = aes(x=displacement, y=pred), color="blue")

p3


data$sqrt_displacement <- sqrt(data$displacement)
mod2 <- lm(mpg ~ sqrt_displacement, data = data)

c2 <- mod2$coefficients
mygrid4 <- data %>% add_predictions(mod2)
g4 <- ggplot(data = data)
p4 <- g4 + geom_point(mapping = aes(x = sqrt_displacement, y = mpg))

p4 <- p4 + geom_line(data = mygrid4, mapping = aes(x=sqrt_displacement, y=pred))

p4

# f. Create a linear model of mpg vs. sqrt(displacement) and cylinders. 
mod5 <- lm(mpg ~ sqrt_displacement+cylinders, data = data)
c5 <- mod5$coefficients

# i.   Give R code, output of summary(model)
summary(mod5)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        36.2894     2.7096  13.393  < 2e-16 ***
#   sqrt_displacement  -1.8493     0.1956  -9.452  < 2e-16 ***
#   cylinders4         12.2841     2.1909   5.607 3.89e-08 ***
#   cylinders5         13.2524     3.3355   3.973 8.44e-05 ***
#   cylinders6         10.9303     2.5032   4.367 1.62e-05 ***
#   cylinders8         12.9472     2.9312   4.417 1.30e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 4.275 on 392 degrees of freedom
# Multiple R-squared:  0.7046,	Adjusted R-squared:  0.7008 
# F-statistic:   187 on 5 and 392 DF,  p-value: < 2.2e-16

# ii.  How many dummy (i.e., 0-1) variables were created in the model?
# 4 variables: cylinders4, cylinders5, cylinders6, cylinders8

# iii. Is this a better fit than in part (e)?
# yes, Adjusted R-squared3 = 0.7008 > Adjusted R-squared2 = 0.6729

# iv.  Plot mpg vs. sqrt(displacement) and overlay the multiple linear fit lines: one for each value of the discrete variable. (code, plot)
g5 <- ggplot(data = data)
p5 <- g5 + geom_point(mapping = aes(x = sqrt_displacement, y = mpg, color=cylinders))

p5 <- p5 + geom_abline(slope = c5[2], intercept = c5[1]) 
p5 <- p5 + geom_abline(slope = c5[2], intercept = c5[1] + c5[3])
p5 <- p5 + geom_abline(slope = c5[2], intercept = c5[1] + c5[4])
p5 <- p5 + geom_abline(slope = c5[2], intercept = c5[1] + c5[5])
p5 <- p5 + geom_abline(slope = c5[2], intercept = c5[1] + c5[6])
p5 <- p5 + labs(x="sqrt(displacement)") 

p5


# v.   Plot mpg vs. displacement and overlay the best fit model as a curve. (code, plot) 
# [Hint: plot the predictions; use add_predictions() and geom_line() and use the color aesthetic for cylinders]
mygrid5 <- data %>% add_predictions(mod5)
g5 <- ggplot(data = data, mapping = aes(x = displacement))
p5 <- g5 + geom_point(mapping = aes(y = mpg, color=cylinders))

p5 <- p5 + geom_line(data = mygrid5, mapping = aes(x=displacement, y=pred, color=cylinders))
p5 <- p5 + labs(x="sqrt(displacement)") 

p5
