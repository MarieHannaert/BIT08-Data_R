#3.1
#Load the mice weight data from the previous lecture
setwd("C:/Users/11901250/Documents/SF/BIT08-Data_R") #windows
read.csv(file="Rdatasets-v2.1/mice_weights.csv", header = TRUE)
mice_data <- read.csv(file="Rdatasets-v2.1/mice_weights.csv", header = TRUE)
#Look at the structure and five-number summary statistics (min, max, mean, Q1, Q3)
summary(mice_data)
#What is the standard deviation of the body weight
sd(mice_data$Bodyweight)
#Make a barplot of the mice diet frequencies
mice_data.freq <- table(mice_data$Diet)
barplot(mice_data.freq)
#Make a histogram of the body weight
hist(mice_data$Bodyweight,
     breaks = 10, 
     xlim = c(15,40))
  
#Make a boxplot of the mice body weight by diet group
boxplot(mice_data$Bodyweight ~ mice_data$Diet, data=mice_data) #first .. second is the categorial)
#Make a Q-Q plot of the mice body weight “show” vs “hf”
qqplot(mice_data$Bodyweight[mice_data$Diet=="chow"], 
       mice_data$Bodyweight[mice_data$Diet=="hf"])

#3.2
#Simulate a dataset of 10000 random values having a right skewed distribution   using rnbinom() with mean = 10 and sd = .5
x <- 0:10000
random_dataset <- rnbinom(x, mu = 10, size = 0.5 );
#Make a hist() and set the x-axis limits (xlim) to the min and max of the dataset
hist(random_dataset, 
     breaks = 10, 
     xlim = c(min(random_dataset),max(random_dataset)))

#Make another histogram using density on y-axis (prob = TRUE) instead of frequency
hist(random_dataset, 
     xlim = c(min(random_dataset),max(random_dataset)),
     prob = TRUE, ylim = c(0,0.1))
# Compute the density estimates (default gaussian kernel)  and use this to plot the density using lines()
lines(density(random_dataset), col="red")
#Determine the skewness and kurtosis
library(moments)
skewness(random_dataset)
kurtosis(random_dataset)
#3.3
# Load the hospital patients dataset from the previous Exercises 2.
setwd("C:/Users/11901250/Documents/SF/BIT08-Data_R") #windows
read.csv(file="Rdatasets-v2.1/hospitalpatients.csv", header = TRUE)
hosppat_data <- read.csv(file="Rdatasets-v2.1/hospitalpatients.csv", header = TRUE)
#Look at the structure. What are the variable types?
dim(hosppat_data)
colnames(hosppat_data)
summary(hosppat_data)
str(hosppat_data)

#Are the classes of the variables matching the types? If not change (as.nnn).
hosppat_data$Gender <- as.factor(hosppat_data$Gender)
hosppat_data$HospitalSurvival <- as.factor(hosppat_data$HospitalSurvival)
#Make a barplot of gender and a barplot of hospital survival (frequencies)
Gender.freq <- table(hosppat_data$Gender)
barplot(Gender.freq)
HosSur.freq <- table(hosppat_data$HospitalSurvival)
barplot(HosSur.freq)
#What are the quartiles (4-quantiles) of the ages? 
median(hosppat_data$Age)
sd(hosppat_data$Age)
#Can you also get the 20%, 40%, 60% and 80% quantiles? Use the help: “probs =“
quantile(hosppat_data$Age)
quantile(hosppat_data$Age,probs = 20, 40, 60, 80)
#Plot a histogram of the patients ages. Next make a density plot of the ages.
hist(hosppat_data$Age)
#Now plot the histogram again with a gray density line. What is the distribution mode?  

#Does this change if you increase the bins (e.g. breaks=10)

#Make a boxplot of the patients weights. Any outliers? 

#How could you know of which patient the weight is an outlier?

#Now program a loop by going over the features Length, Weight and HospitalDays   

#and for each of these features make a histogram and boxplot.


