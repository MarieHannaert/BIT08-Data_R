################################################################################
### Exact test of goodness-of-fit
################################################################################
## Mendelian inheritance experiment (Conover, 1971)
# Plants of two particular genotypes produce progeny 
# 1/4 "dwarf" and 3/4 "giant"
# Experiment cross results: 243 dwarf and 682 giant plants
# If "giant" is taken as success, 
# H0 is p = 3/4 (alternative != 3/4, p is hypothesized probability of success)
# -> use exact binomial test
binom.test(c(682, 243), p = 3/4)
binom.test(682, 682 + 243, p = 3/4) # same
# -> p-value = 0.3825 -> H0 not rejected
# OR
binom.test(c(243, 682), p = 1/4)
# WRONG: binom.test(c(682, 243), p = 1/4)

## Bark beetle experiment
# Experiments how parasitoid wasps find beetles
# Wasps in Y-shaped tube with different odors in arms:
# odor bark eaten by adult beetles,
# other arm bark eaten by larval beetles
# 10 wasps entered "adult" odor arm, 17 "larval" odor arm
binom.test(c(10,17), p = 0.5)
binom.test(c(17,10), p = 0.5) # same for 0.5
# -> p-value = 0.2478 -> H0 not rejected
# Other experiment infested bark vs mixed infested uninfested
# 36 wasp entered "infested" arm, 7 entered "mixture" arm
binom.test(c(7,36), p = 0.5)
binom.test(c(36,7), p = 0.5) # same for 0.5
# -> p-value = 8.963e-06 -> H0 rejected
# -> not expected 1:1 ratio




################################################################################
### Chi Square test of goodness-of-fit
################################################################################
# European crossbills have tip of upper bill 
# either right or left of the lower bill
# Frequency dependent selection keep 1:1 ratio
# Observation: 1752 right-billed, 1895 left-billed
# H0: p = 1:1, H1: p != 1:1
# correct = FALSE to turn off Yates’ correction
chisq.test(c(1752,1895), correct = FALSE)
# --> X-squared = 5.6071, p-value = 0.01789 < 0,05
# --> reject H0 --> significantly more left-billed
# OR:
chisq.test(c(1752,1895), correct = FALSE, p = c(0.5,0.5))



################################################################################
### G-test of goodness-of-fit
################################################################################
# Tools for Descriptive Statistics incl. GTest
# install.packages("DescTools")
library(DescTools) 
# Log likelihood ratio (G-test) goodness of fit test
# on crossbills observations
GTest(c(1752,1895))
# --> G = 5.6085, p-value = 0.01787
# --> similar to chi square test p-value = 0.01789



################################################################################
### Chi Square test of independence
################################################################################
## Jackson et al. (2013)
#  Collected data on reactions to vaccines in children
nosevere <- c(4758,8840)
severe <- c(30,76)
children <- data.frame(nosevere, severe, 
                       row.names = c("thigh","arm"))
# Chi-square test
chisq.test(children, correct = FALSE)
# correct = FALSE to turn off Yates’ correction
# --> X-squared = 2.07, p-value = 0.1502 > 0.05
# --> H0 NOT rejected 
# --> cannot conclude children given vaccinations 
#     in thigh have fewer reactions than those 
#     given in arm 



################################################################################
### G-test of independence
################################################################################
# chisq.test(children, correct = FALSE)
# --> X-squared = 2.07, p-value = 0.15 > 0.05
library(DescTools)
GTest(children)
# --> G = 2.1405, p-value = 0.1435
# --> H0 NOT rejected



################################################################################
### Cochran-Mantel-Haenszel test
################################################################################
# mantelhaen.test requires 3D contingency table in array form
niacin_array <- array(c(2,11,46,41, # FATS study
                        4,12,67,60, # AFREGS study
                        1, 4,86,76, # ARBITER.2 study
                        1, 6,37,32, # HATS study
                        2, 1,92,93),# CLAS.1 study
                      dim = c(2, 2, 5), # 2x2 for each study
                      dimnames = list(
                        Treatment = c("Niacin", "Placebo"),
                        Revasc = c("Yes", "No"),
                        Study = c("FATS", "AFREGS", "ARBITER.2", 
                                  "HATS", "CLAS.1")))
dim(niacin_array) # 2 2 5
# View and verify the data in the array
niacin_array
# Perform the test
mantelhaen.test(niacin_array)
# p-value = 0.0003568 --> reject H0 --> rate of vascularization NOT same
# --> niacin associated with significant reduction in cardiovascular events



################################################################################
### Tukey-Kramer test
################################################################################
## One-way anova example PlantGrowth 
# weight vs group (ctrl, trt1, trt2)
plant.df = PlantGrowth
boxplot(weight~group, data = plant.df)
# Use lm function to fit one-way anova model
plant.model = lm(weight ~ group, data = plant.df)
summary(plant.model)
# Make analysis of variance table for model
anova(plant.model)
# Table confirms differences between groups
# 0.01591 < 0.05 (* significant)

## Perform Tukey-Kramer post-hoc test
summary(plant.aov <- aov(weight ~ group, data = plant.df))
TukeyHSD(plant.aov, "group")
# --> only trt2-trt1 significant adj. p-value = 0.012

# plant.ctrl <- plant.df[which(plant.df$group=="ctrl"),"weight"]
# plant.trt1 <- plant.df[which(plant.df$group=="trt1"),"weight"]
# plant.trt2 <- plant.df[which(plant.df$group=="trt2"),"weight"]
# t.test(plant.ctrl, plant.trt1, var.equal = TRUE, paired = FALSE)
# t.test(plant.ctrl, plant.trt2, var.equal = TRUE, paired = FALSE)
# t.test(plant.trt2, plant.trt1, var.equal = TRUE, paired = FALSE)



################################################################################
### Bartlett's test
################################################################################
## Perform Bartlett's test on PlantGrowth
bartlett.test(weight~group, PlantGrowth)
# --> Bartlett's K-squared = 2.8786
# --> p-value = 0.2371 > 0.05
# --> cannot reject H0 that variance 
#     is same for all treatment groups
# --> homoscedasticity
# --> no evidence to suggest that variance in plant
#     growth is different for three treatment groups



################################################################################
### Nested anova
################################################################################
# NOT COVERED IN COURSE
# Package nlme # Linear and Nonlinear Mixed Effects Models
# Function lme()



################################################################################
### Two-way anova
################################################################################
setwd("/home/pacoh/Dropbox/howest/BIT04-R/Rdatasets3/")
## Enzyme activity and genotype in amphipods by sex
# Measurement: enzyme activity of MPI
# (mannose-6-phosphate isomerase)
# Nominal 1: MPI genotypes (FF, FS, SS)
# Nominal 2: sex (female, male)
amphipods <- read.csv("two-way-anova-testset.csv", 
                      sep = ",", 
                      #dec = ","
                      header = TRUE)
## Visualization of data with boxplot
boxplot(Activity ~ Genotype*Sex,
        data = amphipods,
        xlab = "Genotype x Sex",
        ylab = "MPI Activity")

## Refresh of one-way anova: 
#  plant.df = PlantGrowth
#  plant.model = lm(weight ~ group, data = plant.df)
#  summary(plant.model)

## Two-way anova
amphipods.model = lm(Activity ~ Genotype*Sex, 
                     data = amphipods)
summary(amphipods.model)
# --> p-value: 0.9128 > 0.05
# --> not significant --> cannot reject H0
# Analysis of variance table for model
anova(amphipods.model)
# --> Pr(>F) significance probability 
#     (associated with the F-statistic)
# --> no effect of Sex, Genotype, 
#     or Sex:Genotype significant



################################################################################
### Paired t-test
################################################################################
setwd("/home/pacoh/Dropbox/howest/BIT04-R/Rdatasets3/")
feathers <- read.csv("bird-feather-dataset.csv", 
                      sep = ",", 
                     header = TRUE)
str(feathers)
## Make birds nominal variable (although not necessary)
feathers$Bird <- as.factor(feathers$Bird)
# OR
feathers$Bird <- as.factor(paste0("Bird",feathers$Bird))
str(feathers)

## Run paired t-test
t.test(feathers$TypicalFeather, 
       feathers$OddFeather, 
       paired = TRUE)
# --> p-value = 0.001017
# --> mean diff. = 0.137125
# --> reject H0
# --> odd feathers significantly less 
#     yellow than the typical feathers 	
#     (higher numbers are more yellow)


## Boxplot of yellowness per feather type
boxplot(feathers$TypicalFeather,
        feathers$OddFeather,
        names = c("Typical","Odd"),
        main = "Yellowness in feathers",
        ylab = "Yellowness")



################################################################################
### Wilcoxon signed-rank test
################################################################################
setwd("/home/pacoh/Dropbox/howest/BIT07-R/Rdatasets/")
poplars <- read.csv("poplar-pollution-dataset.csv", 
                     sep = ",", header = TRUE)
str(poplars)
# two nominal: time of year (Aug/Nov) and poplar clone
# measurement: concentration aluminium
# H0: median diff. between pairs = 0

## Make a figure with histograms and boxplot
par(mfrow = c(2,2))
# Histograms
hist(poplars$August)
hist(poplars$November)
hist(poplars$August.November)
# Boxplot of conc. of Al per month
boxplot(poplars$August, poplars$November,
        names = c("August","November"),
        main = "Aluminium pollution in poplars",
        ylab = "Concentration")

## Run paired t-test
t.test(poplars$August, poplars$November, 
       paired = TRUE)
# --> p-value = 0.03956
# Differences are somewhat skewed
# Wolterson clone much larger 
# difference than other clones
# To be safe better use...
## Wilcoxon signed-rank test
wilcox.test(poplars$August, poplars$November, 
            paired = TRUE)
# --> p-value = 0.03979 --> reject H0 
# --> median change Al in wood significant



################################################################################
################################################################################
################################################################################