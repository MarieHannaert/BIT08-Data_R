################################################################################
### Exercise 3.1
################################################################################
# Load the mice weight data from the previous lecture
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
mice_data <- read.csv(url)


# Look at the structure 
str(mice_data)
# and five-number summary statistics  
summary(mice_data)


# What is the standard deviation of the body weight
sd(mice_data$Bodyweight)


# Stem-and-leaf plot of body weight
stem(mice_data$Bodyweight)


# Make a barplot of the mice diet frequencies
table(mice_data$Diet)
diet_freq <- table(mice_data$Diet)
barplot(diet_freq)

# Make a histogram of the body weight
hist(mice_data$Bodyweight, 
     breaks = 10,
     xlim = c(15,40))


# Make a boxplot of the mice body weight by diet group
boxplot(mice_data$Bodyweight)

boxplot(Bodyweight~Diet, data = mice_data)

boxplot(mice_data$Bodyweight ~ mice_data$Diet)


# Make a Q-Q plot of the mice body weight “show” vs “hf”
qqplot(mice_data$Bodyweight[mice_data$Diet=="chow"], 
       mice_data$Bodyweight[mice_data$Diet=="hf"])





################################################################################
### Exercise 3.2: making a distribution with right skewed / positive skewness
################################################################################
# Simulate a dataset of 10000 random values using rnbinom
N <- 10000
x <- rnbinom(N, mu=10, size=.5)
# mu: mean
# size: shape parameter --> size=0 distribution concentrated at zero

# Default histogram
hist(x)


# Setting x-axis limits with xlim
hist(x, 
     breaks = 20,
     xlim=c(min(x),max(x)))


# Using relative frequency (probability) instead of frequency
# so density can be added as line
hist(x,
     breaks = 20,
     xlim = c(min(x),max(x)), 
     probability = TRUE,
     main='Positive skewed distribution (binom)')

lines(density(x), col="red")


# Determine the skewness and kurtosis
library("moments") # install.packages("moments")
skewness(x)
kurtosis(x)





################################################################################
### Exercise 3.3:
################################################################################
# Load the hospital patients dataset again
# setwd("/media/sf_VMshare/BIT04-R/Rdatasets")
patients <- read.csv(file="hospitalpatients.csv", 
                     header = TRUE)
dim(patients)
colnames(patients)
# [1] "Gender"  "Age"  "Length"  "Weight"  "HospitalSurvival"  "HospitalDays"
str(patients)     # shows for each column the type of date en some values
# Gender   --> categorical, nominal, dichotomous (factors)
# Age      --> continuous/numerical
# Length   --> continuous/numerical
# Weight   --> continuous/numerical
# Survival --> categorical, nominal, dichotomous (should be factor!)
# Days     --> continuous/numerical
summary(patients) # shows for factors the frequencies
patients$HospitalSurvival <- as.factor(patients$HospitalSurvival)
str(patients)


# Make a barplot of gender and survival (frequencies)
table(patients$Gender)
gender_freq <- table(patients$Gender)
# par(mfrow = c(1,1))
barplot(gender_freq) # in next chapter --> how we can improve this plot
table(patients$HospitalSurvival)
survival_freq <- table(patients$HospitalSurvival)
barplot(survival_freq)
# Wrong! barplot(table(patients$Age))

# Stem-and-leaf
stem(patients$Age)


# What is the mean, median and standard deviation of the age
mean(patients$Age)
median(patients$Age)
sd(patients$Age)


# Quartiles and quantiles (20%,40%,60%,80%)
quantile(patients$Age)
quantile(patients$Age, probs = c(0.2,0.4,0.6,0.8))
# quantile(patients$Length)
# quantile(patients$Length, probs = c(0.2,0.4,0.6,0.8))

# Plot a histogram of the patients ages
hist(patients$Age, probability=T)


# Next make a density plot
age_density <- density(patients$Age) 
plot(age_density) 


# Now plot the histogram again with a gray density line
hist(patients$Age, probability=T)
lines(density(patients$Age), 
      col='gray')


# With more bins 
hist(patients$Age, 
     probability=T, 
     breaks = 10,
     xlim = c(0,100))
lines(density(patients$Age), 
      col='gray')
# distribution mode again bimodal (or even multimodal if you consider a third peak)


# Make a boxplot of the patients weights.
boxplot(patients$Weight)
# Any outliers? Yes, one dot above the maximum without outliers.
# How could you know of which patient the weight is an outlier? 
# At this stage one possibility is looking at the boxplot
# and asking for the patient with weight > maximum (without outliers, top whisker)
patients[which(patients$Weight>120),]
# Now you see there are actually two patients weighing 130 kg (=same dot)

## Loop over three features and make histogram and boxplot 
# Uncomment next line to create one figure with all six plots
# par(mfrow = c(3,2), mar = c(6,6,4,4))
features <- c("Length","Weight","HospitalDays")
# features <- colnames(patients[,c(3,4,6)])
for(feat in features) {
  hist(patients[,feat], main = paste0("Histogram ",feat))
  boxplot(patients[,feat], main = paste0("Boxplot ",feat))
}



################################################################################