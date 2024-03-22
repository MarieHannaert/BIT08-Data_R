################################################################################
### Fisher's Exact Test of Independence
################################################################################
# fisher.test performs Fisher's exact test for testing the null of independence 
# of rows and columns in a contingency table with fixed marginals
## Example drug study of cancer patients
# H0: means of two groups are equal, no association between drug and outcome
cured <- c(45,19)
notcured <- c(55,89)
outcome <- data.frame(cured, 
                      notcured, 
                      row.names = c("Drug X","Drug Y"))
fisher.test(outcome)
# p-value = 2.217e-05

## Diet example
# Create data frame with diet data
Men <- c(10,5)
Women <- c(30,60)
diet <- data.frame(Men, Women,
                   row.names = c("Dieting","Non-Dieting"))
# Or as matrix
diet2 <- matrix(c(10,5,30,60), nrow = 2,
                dimnames = list(c("Dieting","Non-Dieting"),c("Men","Women")))
# Null hypothesis: There is no association between gender and dietary habits
# Alternative hypothesis: There is an association between gender and dietary habits (two-sided) 
# Default without the alternative option is two-tailed
# Almost always use a two-tailed test, unless you have a very good reason
fisher.test(diet)
# p-value = 0.02063
fisher.test(diet, alternative="two.sided")
# p-value = 0.02063 
# Alternative hypothesis: There is a positive association between gender and dietary habits (one-sided-greater)
fisher.test(diet, alternative="greater")
# p-value = 0.01588



################################################################################
### One-sample and two-sample Student's t-test
################################################################################
## One-sample t-test
# Correlation of transferrin labeled red and 
# Rab-10 labeled green (McDonald and Dunn, 2013)
# H0: transferrin and Rab-10 not colocalized 
# --> red/green correlation coefficient mean=0
# H1: mean NOT 0
# Correlation coefficients in 5 cells 
corrcoef = c(0.52, 0.20, 0.59, 0.62, 0.60)
calcmean <- mean(corrcoef)
calcmean # mean is 0.506 > µ/mu=0
# one-sample t-test
t.test(corrcoef)
# p-value = 0.002958
# --> 0.003 < 0.01 
# --> highly significant
# --> reject H0 
# --> transferrin and Rab-10 colocalized in cells

## Two-sample t-test
# Height of students class 2017 and 2018 in cm
# measurement var height, nominal var class
students2017 <- c(175,179,167,160,173,179,175,170,157,160)
mean(students2017) # 169.50
students2018 <- c(173,170,173,175,182,178)
mean(students2018) # 175.17
# H0: mean heights in two classes same
# H1: mean heights in two classes NOT same
# two-sample t-test
t.test(students2017,students2018, var.equal = TRUE, 
       paired = FALSE) # FALSE default = independent samples
# p-value = 0.1396 > 0.05
# --> NOT significant --> do not reject H0



################################################################################
### One-way ANOVA
################################################################################
# PlantGrowth data weight vs group (ctrl, trt1, trt2)
plant.df = PlantGrowth
# Inspect data with boxplot
boxplot(weight~group, data = plant.df)

# To know whether treatments (trt1, trt2) 
# are different from ctrl use one-way anova
# Use lm function to fit one-way anova model
plant.model = lm(weight ~ group, data = plant.df)
summary(plant.model)
# Save model fitted to the data in object
# to study goodness of the fit to data

# Make analysis of variance table for model
anova(plant.model)
# Table confirms differences between groups
# 0.01591 < 0.05 (* significant)

# confint calculates confidence intervals on treatment
confint(plant.model)



################################################################################
### Kruskall-Wallis test
################################################################################
setwd("/home/pacoh/Dropbox/howest/BIT07-R/Rdatasets/")
dogs <- read.csv("dogs-Cafazzo2010.csv", 
                      sep = ",", header = TRUE)
# mean rank for males: 11.067
mean(dogs[dogs$Sex=="Male","Rank"])
# mean rank for females: 17.667
mean(dogs[dogs$Sex=="Female","Rank"])
# H0: mean ranks of the groups are the same
# H1: mean ranks of the groups are NOT the same
# Perform Kruskall-Wallis test
kruskal.test(Rank ~ Sex, data = dogs)
# Difference is significant: 0.03179 < 0.05
# reject H0



################################################################################
################################################################################
################################################################################