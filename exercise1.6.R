patient <- read.csv(file="C:/Users/11901250/Documents/SF/BIT08-Data_R/Rdatasets-v2.1/hospitalpatients.csv", header = TRUE)
dim(patient)
str(patient)
ages <- sort(patient$Age) #this is a vector

#kijk naar oplossing van Paco zelf 
