################################################################################
### Exercise 1.4
################################################################################
## Install/load the Hmisc library
install.packages("Hmisc")
library(Hmisc)

## Use the describe() function on the urine dataset
## Save the output in a file urine_descriptive.txt
sink("urine_descriptive.txt", append=FALSE, split=TRUE)
describe(urine_data)
sink()





################################################################################
### Exercise 1.5
################################################################################
## Load the ChickWeight data
data(ChickWeight)
dim(ChickWeight)
summary(ChickWeight)
str(ChickWeight)

## From the help for ChickWeight
# weight = body weight of the chicks (gram)
# time = number of days since birth when measurement was made
# chick = ordered factor with levels giving unique identifier for the chicks
# diet = a factor with levels 1 to 4 indicating which diet the chick received
## Save the data frame in an xlsx file
# Load the xlsx package
library(xlsx)
# Set the working directory where you want to save the file
#setwd("/home/username/foldername") 
# Save the file
write.xlsx(x = ChickWeight, 
           file = "ChickWeight.xlsx",
           sheetName = "Weight vs age of chicks on different diets",
           row.names = FALSE)

## Make column Days and fill it with day 0, day 2,...
ChickWeight$Day <- paste0("day ",ChickWeight$Time)

## Replace the d by D in Day
sub("d", "D", ChickWeight$Day)
ChickWeight$Day <- sub("d", "D", ChickWeight$Day)

## Get a logical vector (TRUE/FALSE) which rows are Day 0
grepl("Day 0",ChickWeight$Day)

## What is the class of Day and set it to factor
class(ChickWeight$Day)
typeof(ChickWeight$Day)
ChickWeight$Day
ChickWeight$Day <- as.factor(ChickWeight$Day)
ChickWeight$Day

## Have a look at descending sort of ChickWeight$Chick 
sort(ChickWeight$Chick, decreasing = TRUE)
# To see the unique ones you can use
unique(sort(ChickWeight$Chick, decreasing = TRUE))
# first two different values are 48 and 42
#  last two different values are 16 and 18 (sorted by ordered factor!)

## Numeric column ChickWeight$Chick2 and sort descending
ChickWeight$Chick2 <- as.numeric(ChickWeight$Chick) # index is taken, not value!
sort(ChickWeight$Chick2, decreasing = TRUE)
unique(sort(ChickWeight$Chick2, decreasing = TRUE))
# now the first two are 50 and 49, the last two are 2 and 1 (numeric sort!)

## Have a look at the first row and last row
ChickWeight[c(1,nrow(ChickWeight)),]
# Chick and Chick2 values are different
# because as.numeric(ChickWeight$Chick) takes the indexes, not the levels/values

## To use the levels/values of Chick in Chick2:
ChickWeight$Chick2 <- as.numeric(levels(ChickWeight$Chick))[ChickWeight$Chick]
# or
ChickWeight$Chick2 <- NULL # this will first remove Chick2 
ChickWeight$Chick2 <- as.numeric(as.character(ChickWeight$Chick))
# Make four subsets of the chicks based on the same diet
ChickWeight[ChickWeight$Diet=="1",]
ChickWeight1 <- ChickWeight[which(ChickWeight$Diet=="1"),]
# or
ChickWeight1subset <- subset(ChickWeight, ChickWeight$Diet=="1")
ChickWeight2subset <- subset(ChickWeight, ChickWeight$Diet=="2")
ChickWeight3subset <- subset(ChickWeight, ChickWeight$Diet=="3")
ChickWeight4subset <- subset(ChickWeight, ChickWeight$Diet=="4")

## Create all four dataframes in a loop
subsets <- c("1","2","3","4")
listofdfs <- list() # create a list in which the dataframes will be saved
for(subset in subsets) {
  print(subset)
  dataframe <- paste0("ChickWeight",subset)
  listofdfs[[dataframe]] <- ChickWeight[which(ChickWeight$Diet==subset),]
}
listofdfs # all four dataframes
listofdfs$ChickWeight1 # one of the four dataframes

## Make a dataframe FatChicks with the chicks having weight > 300 grams
ChickWeight[which(ChickWeight$weight>300),]
FatChicks <- ChickWeight[which(ChickWeight$weight>300),]
nrow(FatChicks)
# to know in one line of R code how many chicks are weighing > 300 grams 
nrow(ChickWeight[which(ChickWeight$weight>300),]) # [1] 14





################################################################################
### Exercise 1.6
################################################################################
## Load the data of the hospital patients and inspect the data (types)
setwd("/media/sf_VMshare/BIT04-R/Rdatasets/")
patients <- read.csv(file="hospitalpatients.csv", header = TRUE)
dim(patients)
colnames(patients)
str(patients)     # shows for each column the type of date en some values
summary(patients) # shows for factors the frequencies
# data types: gender factor, length numeric and the rest integer 
# 34 males and 16 females
class(patients$Gender)
nrow(patients[patients$Gender=="M",])
nrow(patients[patients$Gender=="F",])

## Order the patients by age
ages <- sort(patients$Age)

## Male patients with length > 180 and weight < 60
patients[which(patients$Gender=="M" & patients$Length>180 & patients$Weight>100),]
nrow(patients[which(patients$Gender=="M" & patients$Length>180 & patients$Weight>100),])
# two patients (no. 9 and 12)

## Female patients died in hospital
patients[which(patients$Gender=="F" & patients$HospitalSurvival==0),]
nrow(patients[which(patients$Gender=="F" & patients$HospitalSurvival==0),])
# 6 patients

## Replace the gender (levels): F --> Female, M --> Male
levels(patients$Gender)
patients$Gender <- as.factor(gsub("F","Female",patients$Gender))
# without as.factor class would be character
levels(patients$Gender)
patients$Gender <- as.factor(gsub("M","Male",patients$Gender))
levels(patients$Gender)
# an even quicker way is to replace the levels with a new list of levels
levels(patients$Gender) <- list(girls="F",boys="M")
levels(patients$Gender)
levels(patients$Gender) <- list(Female="girls",Male="boys")
levels(patients$Gender)

## New column BMI
patients$Length/100 # length in meters
patients$BMI <- patients$Weight/(patients$Length/100)^2
# or using the function in own-functions.R (length in cm/100 to get in meters)
setwd("/media/sf_VMshare/BIT04-R/")
source("own-functions.R")
patients$BMI2 <- BMI(patients$Length/100,patients$Weight)
patients[,c("BMI","BMI2")]



################################################################################