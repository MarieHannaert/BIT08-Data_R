#2A/ Give the type of variable(s) you can identify in this dataset. (1 p)
#The type of variable is 1 nominal and 3 measurements  


#2B/ Using the given variable(s), give the name of the test to analyze this dataset. (1 p)
#The test That can be used is a two sample t test


#2C/ Prepare/load the data in the appropriate structure in R(Studio) and view before running the test. (1 p)
if(!('xlsx' %in% pkg)) { install.packages("xlsx") }
library(xlsx)
setwd("C:/Users/11901250/Documents/SF/BIT08-Data_R/Exam/") #windows
tomato_data <- read.xlsx(file="tomatoes.xlsx", header = TRUE, sheetIndex = 1)
str(tomato_data)
mean(tomato_data$yield_sidedressed_tomatoes)
mean(tomato_data$yield_nonsidedressed_tomatoes)

#2D/ Perform the test and explain the outcome of the test (written as comments in the R script). (1 p)
t.test(tomato_data$yield_sidedressed_tomatoes,tomato_data$yield_nonsidedressed_tomatoes, var.equal = TRUE, paired = TRUE)
#Paired t-test

#data:  tomato_data$yield_sidedressed_tomatoes and tomato_data$yield_nonsidedressed_tomatoes
#t = 1.9776, df = 9, p-value = 0.07936
#alternative hypothesis: true mean difference is not equal to 0
#95 percent confidence interval:
#  -2.143753 31.943753
#sample estimates:
#  mean difference 
#14.9

# There is not a significant difference between the groups of being side dressed and not being sidedressed, The p-value is 0,07 > 0,05. 

#2E/ Give the name of a typical visualization technique that can be used to accompany this data and test. (0.5 p)
#this can be visualized by a boxplot
#Additionally, provide a one-liner of R code to generate this plot (using default settings). (0.5 p)
boxplot(c(tomato_data[,2:3]))
