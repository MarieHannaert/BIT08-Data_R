################################################################################
### Exercise 7.1: Fruit fly mating experiment
### --> one nominal variable
### --> exact test or Chi-square test or G-test
### --> exact test more accurate for small sample size
################################################################################
# 140 homotypic, 106 heterotypic
# H0: mating randomly (homotypic = heterotypic)
# H1: mating NOT randomly
binom.test(c(140,106), p = 0.5)
# --> p-value = 0.03516 < 0.05 --> reject H0
# --> significantly more homotypic than heterotypic mating



################################################################################
### Exercise 7.2: Measles attack rates vaccinated and unvaccinated children
### --> Two nominal variables: 
### --> Fischer's exact test or Chi-square test or G-test
### --> Fischer's exact test more accurate for small sample size
################################################################################
# Vaccinated: 10 attacked, 90 not attacked
# Unvaccinated: 26 attacked, 74 not attacked
# Test protective value of vaccination at 0.05 significance level
# H0: vaccination and attack rate are independent
# H1: vaccination and attack rate are not independent (related)
attacked <- c(26,10)
notattacked <- c(74,90)
children <- data.frame(attacked, notattacked, 
                       row.names = c("unvaccinated","vaccinated"))
## Fischer's exact test
#  Preferred because small sample size!
fisher.test(children)
# --> p-value = 0.005249
# --> H0 rejected --> vaccination protects 

## Chi-square test
#  NOT preferred because total sample size <1000!
chisq.test(children, correct = FALSE)
# correct = FALSE to turn off Yates correction
# --> X-squared = 8.6721, p-value = 0.003231
# --> H0 rejected --> vaccination protects 

## G-test
#  NOT preferred because total sample size <1000!
library(DescTools)
GTest(children)
# --> G = 8.9294, p-value = 0.002806
# --> H0 rejected --> vaccination protects 



################################################################################
### Exercise 7.3: Mussels allele frequencies
### --> Three nominal variables: location, allele, habitat
### --> Cochran-Mantel-Haenszel test
################################################################################
# 3D contingency table in array form
mussel <- array(c(56,40,69,77,
                  61,57,257,301,
                  73,71,65,79,
                  71,55,48,48),
                dim = c(2, 2, 4), # 2x2 for each location
                dimnames = list(
                    Allele = c("94", "non-94"),
                    Habitat = c("Marine", "Estuarine"),
                    Location = c("Tillamook", "Yaquina", 
                                 "Alsea","Umpqua")))
# View and verify the data in the array
mussel
# Perform the test
mantelhaen.test(mussel)
# --> Mantel-Haenszel X-squared = 5.0497
# --> p-value = 0.02463 < 0.05
# --> reject H0 
# --> proportion Lap94 alleles is NOT same in marine and estuarine habitats



################################################################################
### Exercise 7.4:
### Corn experiment from https://onlinecourses.science.psu.edu/stat500/node/216
### --> One measurement variable: yield
### --> Two nominal variables: manure, fertilizer
### --> Two-way anova
################################################################################
setwd("/home/pacoh/Dropbox/howest/BIT04-R/Rdatasets3/")
corn <- read.csv("corn-dataset.csv", 
                  sep = ",", header = TRUE)
str(corn)

## Visualization of data with boxplot
boxplot(Yield ~ Manure:Fert,
        data = corn, 
        xlab = "manure x fertilizer", 
        ylab = "yield")

## Two-way anova
corn.model = lm(Yield ~ Manure*Fert, 
                data = corn)
summary(corn.model)
# --> p-value: 0.0243 < 0.05 --> significant

# Analysis of variance table for model
anova(corn.model)
# --> effect of manure significant: 0.03539 
# --> effect of fert sifnificant: 0.02885
# --> manure:fert --> 0.27266 
# --> no evidence of significant interaction however



################################################################################
################################################################################
################################################################################