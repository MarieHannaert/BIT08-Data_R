### First R code 
# Comments start with # symbol
# Assignment is specified with the <- symbol
x <- 2
x
# What kind of object/data type is x?
class(x)
typeof(x)
length(x)

# Store a string by using quotes (both single and double quotes work)
t <- "hello"
t
# What kind of object/data type is t? 
class(t)
typeof(t)
length(t)


### Combine values into a vector with the c() function
# numeric vector
vnum <- c(1,4,5.3,-2)
# character vector
vchar <- c("one","two","three") 
# logical vector
vlogic <- c(TRUE,TRUE,FALSE,TRUE,FALSE)

# What kind of object/data type? 
class(vnum)
typeof(vnum)
length(vnum)

class(vchar)
typeof(vchar)
length(vchar)

class(vlogic)
typeof(vlogic)
length(vlogic)

vnum
vchar
vlogic


### Lists
# Collection of different objects (components)
mylist <- list(name="Paco", number=681, length=1.83)
# Identify element in list
mylist[[2]]
mylist[["name"]]
class(mylist)
typeof(mylist)
length(mylist)

mylist$length
# [1] 1.83


### Matrix
# Generate a 4 x 3 numeric matrix
mtrx <- matrix(1:12, nrow=4, ncol=3)
mtrx[1,2] # value on row 1 in col 2
mtrx[4,] # values row 4
mtrx[,2] # values col 2


### Data frame
# HairEyeColor dataset
hair <- c("black","brown","red","blond","black","brown","red","blond")
eye <- c("brown","brown","brown","brown","blue","blue","blue","blue")
sex <- c("male","male","male","male","male","male","male","male")
freq <- c(32,53,10,3,11,50,10,30)
df_hec <- data.frame(hair,eye,sex,freq)
df_hec
summary(df_hec)
# Elements in data frame
df_hec[1:2]
df_hec[1:2,1]
df_hec[1:2,1:2]
df_hec[c(1,4),1:2]
df_hec[1:2,c(1,4)]
# Use column names (no rows!)
df_hec["freq"]
df_hec[c("hair","freq")]
length(df_hec)
length(df_hec$hair)
class(df_hec)
class(df_hec$hair)
class(df_hec$freq)
typeof(df_hec$hair)


### Factors
# gender data
gender <- c("male","male","female","male","female") 
gender
class(gender)
gender <- factor(gender)
gender
class(gender)


### Special characters
# Single quote needn't be escaped inside double quotes
str1 <- "This string needn't escaped char"
str1
# Double quotes needn't be escaped inside single quotes
# R prints embedded quotes by preceding them with backslash
# Backslash character is not part of string, only visible when you print string
str2 <- 'This string has "double quotes"'
str2
# Escape required within same quotes # Use backslash character to escape
str3 <- "This string has \"double quotes"
str3
# "\\" for a (single) backslash character
str4 <- 'Show the backslash here \\'
str4
# "\t" for TAB
str5 <- "Tab\tT"
str5
nchar(str5)
cat(str5)
# "\n" for newline
str6 <- "New\nline"
str6
nchar(str6)
cat(str6)


### Missing values (NA)
y <- c(2,4,NA,8)
y
# is.na() function indicates which elements are missing
is.na(y) # -> returns a vector (FALSE FALSE TRUE FALSE)
# na.omit() function returns object with listwise deletion of missing values
y_no_na <- na.omit(y) # --> new object without missing value
y_no_na


################################################################################