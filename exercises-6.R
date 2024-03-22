################################################################################
### Exercise 6.1: Fisher's Exact Test of Independence
################################################################################
## C. difficile data
# Make lists of columns
fecal <- c(3,13)
vancomycin <- c(9,4)
# Make data frame of lists (columns) and add row.names
patients <- data.frame(fecal, vancomycin,
                       row.names = c("sick","cured"))
# What are the hypotheses?
# H0: probability of getting cured is same 
# whether you receive fecal transplant or vancomycin
# H1: probability getting cured NOT same
fisher.test(patients)
# p-value = 0.00953 --> highly significant --> reject H0

tpatients <- t(patients)
fisher.test(tpatients)
# same! --> doesn't matter which variable rows and columns



################################################################################
### Exercise 6.2: 
################################################################################
setwd("/home/pacoh/Dropbox/howest/BIT07-R/Rdatasets/")
## Pigs weights after 10 months diet
pigs <- read.csv("pigs-diets-weight.csv", 
                  sep = ",", header = TRUE)
pigs.weight <- c(pigs$Diet1,pigs$Diet2,pigs$Diet3,pigs$Diet4)
pigs.diet <- c(rep("Diet1",5),rep("Diet2",5),rep("Diet3",5),rep("Diet4",5))
pigs.df <- data.frame(pigs.weight,pigs.diet,
                      row.names = seq(1:20))
# View weights per diet in boxplot
boxplot(pigs.weight ~ pigs.diet, data = pigs.df)
# Mean and standard deviation for a diet group
mean.diet3 <- mean(pigs.df[pigs.df$pigs.diet=="Diet3","pigs.weight"])
mean.diet3
sd.diet3 <- sd(pigs.df[pigs.df$pigs.diet=="Diet3","pigs.weight"])
sd.diet3
# lm function to fit one-way anova model
pigs.model = lm(pigs.weight ~ pigs.diet, data = pigs.df)
summary(pigs.model)
# Make anova table for model
anova(pigs.model)
# H0: μ1 = μ2 = μ3 = μ4
# H1: not all μ are equal
# Table confirms differences between groups
# 5.285e-13 < 0.01 (*** highly significant)



################################################################################
################################################################################
################################################################################