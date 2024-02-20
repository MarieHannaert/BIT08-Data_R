################################################################################
### Exercise 1.1
################################################################################
# a) atomic vector types holds data of a single type
charactertype <- 'A'
integertype <-10
logicaltype <- TRUE

# b) check is.vector and class
is.vector(charactertype)
class(charactertype)
is.vector(integertype)
class(integertype)
is.vector(logicaltype)
class(logicaltype)

# c) a 4x3 matrix with values from 2 to 24 (increasing by 2) column wise
matrixdata <- c((1:12)*2)
mat4x3 <- matrix(matrixdata, nrow = 4, ncol=3, byrow = FALSE)
print(mat4x3)



################################################################################
### Exercise 1.2
################################################################################
# a) data frame of 6 patients, with names and ages
names <- c("Frodo","Legolas","Elrond","Gimli","Gandalf","Arwen")
ages <- c(17,28,65,53,78,23)
patients <- data.frame(names,ages)
# b) display the summary of the patient data
summary(patients)
colnames(patients) <- c("Name","Age")
summary(patients)
# c) display the details of the first patient
patients[1,]
# d) display the details of patients 2, 3 and 4
patients[2:4,]
# e) display the ages of the patients
patients[,2]
patients[,"Age"]
patients$Age
# f) what is the class of patients name
class(patients$Name)
# g) Change the class of patients name to character
patients$Name <- as.character(patients$Name)
class(patients$Name)
# h) Add the gender for the patients as factor
patients$Gender <- as.factor(c("Male","Male","Male","Male","Male","Female"))
class(patients$Gender)
summary(patients)



################################################################################
### Exercise 1.3
################################################################################
# a) Make a vector of class factor with 5 sizes
#    either S (small), M (medium) or L (large)
sizes <- c("S", "L", "S","M","L")
sizes # values not factors!
# to make sure they are factors
sizes <- factor( c("S", "L", "S","M","L"))

# b) Check the values and levels
sizes
# [1] S L S M L
# Levels: L M S
levels(sizes) 
# [1] "L" "M" "S"

# c) Change the levels to their full name
levels(sizes) <- c("large", "medium","small")
sizes
# [1] small  large  small  medium large 
# Levels: large medium small

# d) Set the 4th value to NA and check values and levels again
sizes[4] <- NA
sizes
# [1] small large small <NA>  large
# Levels: large medium small

# e) Check which values in the vector are missing/NA
is.na(sizes)
# [1] FALSE FALSE FALSE  TRUE FALSE



################################################################################