install.packages("Hmisc")
library(Hmisc)
#for linux setwd("/home/guest/Shared/BIT08-Data_R")
setwd("C:/Users/11901250/Documents/SF/BIT08-Data_R") #windows
read.csv(file="Rdatasets-v2.1/urine.csv", header = TRUE)
urine_data <- read.csv(file="Rdatasets-v2.1/urine.csv", header = TRUE)
sink("urine_descriptive.txt", append=FALSE, split=FALSE) #split is to give also the output in the console and not only in the file
describe(urine_data) 
sink()
