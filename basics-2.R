### R environment
# list defined variables/data in workspace
ls()
# remove str1
str1 <- "This string needn't escaped char"
str1
rm(str1)
# show current working directory
getwd()


### Library/packages
install.packages("xlsx")
library(xlsx)


### Bioconductor packages
install.packages("BiocManager")
library(BiocManager)
# e.g. Limma package for analysis of gene expression data (microarray, RNA-Seq)
BiocManager::install("limma")


### Reading data
setwd("/media/sf_VMshare/BIT04-R")
read.csv(file="Rdatasets/urine.csv", header = TRUE)
urine_data <- read.csv(file="Rdatasets/urine.csv", header = TRUE)
class(urine_data)
dim(urine_data)
colnames(urine_data)
rownames(urine_data)
str(urine_data)
# Reading from url
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
mice_data <- read.csv(url)
str(mice_data)


### Writing data
# Writing data to csv file
write.csv(mice_data, file = "Rdatasets/mice_weights.csv", row.names = FALSE)

# Writing data to Excel
# library(xlsx) # to load the package
write.xlsx(x = mice_data, 
           file = "Rdatasets/mice_weights.xlsx",
           sheetName = "Females diet weight",
           row.names = FALSE)

# Direct output to a file
sink("output.txt", append=FALSE, split=FALSE)
summary(mice_data)
# Return output to the terminal
sink()


### More functions
str(urine_data)
is.numeric(urine_data$r)
as.logical(urine_data$r)
urine_data$r <- as.logical(urine_data$r)
str(urine_data)

### Example of the paste function
letters <- c("A","B","C","D","E","F","G","H","I","J")
# 1st, 2nd, … , 10th
nth <- paste0(1:10, c("st", "nd", "rd", rep("th", 7)))
paste(letters,"is the",nth,"letter")
letters_numbers <- paste(letters,"is the",nth,"letter")

### Replace "hf" by "high fat" in the mice_dataset
gsub("hf", "high fat", mice_data$Diet)
mice_data$Diet2 <- gsub("hf", "high fat", mice_data$Diet)
mice_data[c(1,20),]

### Examples of grep and grepl functions
grep("a+", c("abc", "def", "cba a", "aa"), perl=TRUE, value=FALSE)
# [1] 1  3  4
grep("a+", c("abc", "def", "cba a", "aa"), perl=TRUE, value=TRUE)
# [1] "abc" "cba a" "aa"
grepl("a+", c("abc", "def", "cba a", "aa"), perl=TRUE)
# [1] TRUE  FALSE TRUE  TRUE

### Example of sorting functions
sort(mice_data$Bodyweight)
sort(mice_data$Bodyweight, decreasing = TRUE)
sort(mice_data$Bodyweight, decreasing = TRUE, index.return = TRUE)
order(mice_data$Bodyweight)
order(-mice_data$Bodyweight)
# mice_data_weight_sort <- mice_data[sort(mice_data$Bodyweight),] # will not work
mice_data[order(mice_data$Bodyweight),] 
mice_data_weight_sort <- mice_data[order(mice_data$Bodyweight),]


### Subsetting data
# First 4 observations
urine_subset1 <- urine_data[1:4,]
# First 4 observations, first 3 columns
urine_subset2 <- urine_data[1:4,1:3]
# First 4 observations, selection of columns  
urine_subset3 <- urine_data[1:4,c("X","ph","calc")]
# Select 1st and 5th to 10th 
urine_subset4 <- urine_data$ph[c(1,5:10)]
urine_subset4 <- urine_data[c(1,5:10),]
# Exclude 1st and 5th to 10th 
urine_subset5 <- urine_data[c(-1,-5:-10),]
# Based on variable values
mice_subset1 <- mice_data[which(mice_data$Diet=='hf'),]
mice_subset2 <- mice_data[which(mice_data$Diet=='hf' & mice_data$Bodyweight>30),]

# Using conditional selection 
mice_data$Bodyweight<23   # [1]  TRUE FALSE FALSE ...
mice_data[mice_data$Bodyweight<23,]

# Using the subset() function
mice_subset3 <- subset(mice_data, Diet=="chow", select=c(Diet,Bodyweight))
mice_subset4 <- subset(mice_data, Diet=="chow" & Bodyweight<23, select=c(Diet,Bodyweight))


### Merging data
# Divide the urine data in a dataframe of 
# the first 4 columns and one with the rest
urine1to4 <- urine_data[,1:4]
urine5to8 <- urine_data[,c(1,5:8)]
dim(urine1to4)
colnames(urine1to4)
dim(urine5to8)
colnames(urine5to8)
# Now merge them back together
urine1to8 <- merge(urine1to4, urine5to8, by = "X")
# Divide the rows of the urine data 
# in a data frame with r=0 and a data frame with r=1
urine_r0 <- urine_data[which(urine_data$r==0),]
urine_r1 <- urine_data[which(urine_data$r==1),]
dim(urine_r0) # [1] 45  8
dim(urine_r1) # [1] 34  8
# Merge the rows with r=0 and r=1 again
urine_r0_r1 <- rbind(urine_r0,urine_r1)
# cbind can be used like merge to merge columns
urinecbind <- cbind(urine1to4,urine5to8) # X column twice!
urinecbind[5] <- NULL 


### apply(X, MARGIN, FUNCTION)
# show the max values for the urine dataset
apply(urine_data, 2, max)
# add na.rm=TRUE if some values are missing (NA)
apply(urine_data, 2, max, na.rm=TRUE)
apply(urine_data[c(4,7)], 2, max)
# lapply()
lapply(urine_data, max)
lapply(urine_data[4:7], max, na.rm=TRUE)


### Programming loops
# FOR and IF loop example
count30plus <- 0
val30plus <- c()
for (val in mice_data$Bodyweight) {
  if(val > 30) { 
    count30plus = count30plus+1
    val30plus <- c(val30plus, val)
  }
}
print(paste0("Mice with bodyweight >30: ",count30plus))
#print(paste0("Values >30: ",val30plus))
print(val30plus)


### Writing and using your own function
getwd()
source("own-functions.R")
patientWeight <- c(65,73,58,87)
patientHeight <- c(1.83,1.87,1.65,1.75)
patientData <- data.frame(patientWeight,patientHeight)
BMI(patientData$patientHeight,patientData$patientWeight)
patientData$BMI <- BMI(patientData$patientHeight,patientData$patientWeight)



################################################################################