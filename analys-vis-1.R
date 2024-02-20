################################################################################
### Packages
################################################################################
# Reading from Excel files
library(xlsx)
# Data visualisation
library(ggplot2)



################################################################################
### Data
################################################################################
# Iris dataset
data(iris)
summary(iris)



################################################################################
### Summary statistics
################################################################################
# Frequency for categorical variable
table(iris$Species)
# Relative frequency
species.freq <- table(iris$Species)
species.relfreq <- species.freq/nrow(iris)
species.relfreq
# Percentages
species.percentages <- species.relfreq*100
species.percentages

# Summary statistics for numerical variables
min(iris$Sepal.Length)
max(iris$Sepal.Width)
mean(iris$Petal.Length)
sd(iris$Petal.Width)    # standard deviation
var(iris$Sepal.Length)  # variance returns min and max
quantile(iris$Petal.Length)
#   0%  25%  50%  75% 100% 
# 1.00 1.60 4.35 5.10 6.90 
quantile(iris$Petal.Length, probs = seq(0, 1 , 0.2) ) # 0% 20% 40% 60% 80% 100%
# Get means for numerical variables in iris data frame
# excluding missing values
sapply(iris[,1:4], mean, na.rm=TRUE) 

# Example n = 6 numbers
num <- c(2,2,4,4,4,14)
# mean = 30/6 (sum values/numbers)
mean(num)
# median = 2,2,4|4,4,14
median(num)
# variance
# = (2-5)²+(2-5)²+(4-5)²+(4-5)²+(4-5)²+(14-5)² / 5
# = (9 + 9 + 1 + 1 + 1 + 81) / 5 = 102 / 5 = 20.4
var(num)
# standard deviation = square(20.4)
sd(num)
# 4-quantiles = quartiles
quantile(num)
#  0%  25%  50%  75% 100% 
# 2.0  2.5  4.0  4.0 14.0 



################################################################################
### Data analysis and visualization
################################################################################
plot(iris$Sepal.Width)
# Simple Bar Plot # species.freq <- table(iris$Species)
barplot(species.freq)
# Pie chart
pie(species.freq)
# Stem-and-leaf plot
stem(num)
stem(iris$Sepal.Width)
# Histogram
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, freq = FALSE)
hist(iris$Sepal.Width, breaks = 5)
hist(iris$Sepal.Width, breaks = 30)

### Skewness and kurtosis
time_min <- c(19.09, 19.55, 17.89, 17.73, 25.15, 27.27, 25.24, 21.05, 21.65, 20.92, 22.61, 15.71, 22.04, 22.60, 24.25)
library(moments)
skewness(time_min)
# [1] -0.01565162
kurtosis(time_min)
# [1] 2.301051


### Examples for normal distribution
### dnorm function 
# takes three main arguments x, mu (mean) and sigma (sd)
dnorm(0)                   # [1] 0.3989423
dnorm(0, mean = 0, sd = 1) # [1] 0.3989423
dnorm(2, mean = 5, sd = 3) # [1] 0.08065691
# make a vector
xseq <- seq(-4,4,.1)
# print the vector
xseq
# dnorm with default value for mean 0 and for sd 1
densities <- dnorm(xseq)
# examine values
densities
# plot the densities values plot(x=xseq, y=densities,...)
plot(densities,  # y = values and x = index of the value in the vector xseq
     main = "dnorm probability density function")
plot(densities, 
     type = "l", # line plot
     main = "dnorm probability density function") 
# cumulative and random
cumulative <- pnorm(xseq, 0, 1)
random <- rnorm(1000,0,1)
plot(cumulative, 
     type="l",
     main="Standard normal CDF")
plot(random, 
     main="Random draws from standard normal")
hist(random, 
     main="Random draws from standard normal",
     xlim=c(-4,4))


### Kernel density plot
# get the density data in d
d <- density(iris$Sepal.Width) 
# plot the results
plot(d)

# get the density data for versicolor species in d2
                   which(iris$Species=="versicolor")
              iris[which(iris$Species=="versicolor"),]
              iris[which(iris$Species=="versicolor"),"Sepal.Width"]
d2 <- density(iris[which(iris$Species=="versicolor"),"Sepal.Width"]) 
plot(d2) 

### Boxplot
boxplot(Sepal.Width ~ Species, data = iris)

### Violin plot
library(vioplot) # install.packages("vioplot")
levels(iris$Species)
sw1 <- iris$Sepal.Width[iris$Species=="setosa"]
sw2 <- iris$Sepal.Width[iris$Species=="versicolor"]
sw3 <- iris$Sepal.Width[iris$Species=="virginica"]
vioplot(sw1, sw2, sw3, names=c("setosa", "versicolor", "virginica"))

### Q-Q-plot
# Normal quantile-quantile plot
qqnorm(iris$Sepal.Width)
qqline(iris$Sepal.Width)
# Q-Q plot of two datasets
qqplot(iris$Sepal.Length[iris$Species=="versicolor"], 
       iris$Sepal.Length[iris$Species=="virginica"])
# sorted values of the two datasets
sort(iris$Sepal.Length[iris$Species=="versicolor"])
sort(iris$Sepal.Length[iris$Species=="virginica"])

### Fitting distributions 
library(MASS)
swfit <- fitdistr(iris$Sepal.Width, "normal")
swfit
#     mean          sd    
# 3.05733333   0.43441097 
# (0.03546951) (0.02508073)
swfit$estimate
#     mean       sd 
# 3.057333 0.434411
hist(iris$Sepal.Width, prob=TRUE)
curve(dnorm(x, swfit$estimate[1], swfit$estimate[2]), add = TRUE)


### Scatter plot
plot(iris$Petal.Length, iris$Petal.Width)



################################################################################
### Exercises
################################################################################

# using epanechnikov smoothing kernel
dEpan <- density(iris$Sepal.Width, kernel = "epanechnikov") 
plot(dEpan) 



################################################################################
# Example code for making a histogram and curve with normal distribution
xseq<-seq(-4,4,.01)
y<-2*xseq + rnorm(length(xseq),0,5.5)
hist(y, prob=TRUE, ylim=c(0,.06), breaks=20)
curve(dnorm(x, mean(y), sd(y)), add=TRUE, col="darkblue", lwd=2)


