################################################################################
### Exercise 4.1
################################################################################
setwd("/home/pacoh/Dropbox/howest/BIT07-R/Rdatasets/")
urine_data <- read.csv(file="urine.csv", header = TRUE)
colnames(urine_data)
boxplot(calc ~ r, data = urine_data)
# Now set titles and font
# r = indicator of the presence of calcium oxalate crystals
# calc = calcium concentration in millimoles per litre
boxplot(calc ~ r, data = urine_data,
        main = "Boxplot calcium concentration urine dataset", 
        cex = 0.9, pch = 4,
        xlab = "Presence of calcium oxalate crystals", names = c("no","yes"),
        ylab = "Calcium concentration (mM/L)", ylim = c(0,15))
# In the plot you can see where calcium oxalate crystals are present the concentration calcium is higher





################################################################################
### Exercise 4.2
### http://vincentarelbundock.github.io/Rdatasets/datasets.html
################################################################################
## Load the Arabidopsis dataset from the lme4 package
install.packages("lme4") 
# takes a while and you will probably get warnings
# ready when: * DONE (lme4)
library(lme4)
data(Arabidopsis)

## 625 observations of 8 variables
str(Arabidopsis)

## Barplots of factors reg, popu, amd, status
library(RColorBrewer)
display.brewer.all()
# use palette with at least 9 colors
brew.col <- brewer.pal(9,"Paired")
layout(matrix(c(1,2,3,4,4,4), 2, 3, byrow = TRUE))
# Barplot 1: reg (3 levels)
reg.freq <- table(Arabidopsis$reg)
barplot(reg.freq,
        xlab = "reg levels",
        ylab = "frequency",
        ylim = c(0,400),
        col = brew.col,
        main = "Barplot reg")
# Barplot 2: amd (2 levels)
amd.freq <- table(Arabidopsis$amd)
barplot(amd.freq,
        xlab = "amd levels",
        ylab = "frequency",
        ylim = c(0,400),
        col = brew.col, 
        main = "Barplot amd")
# Barplot 3: status (3 levels)
status.freq <- table(Arabidopsis$status)
barplot(status.freq,
        xlab = "status levels",
        ylab = "frequency",
        ylim = c(0,400),
        col = brew.col,
        main = "Barplot status")
# Barplot 4: popu (9 levels)
popu.freq <- table(Arabidopsis$popu)
barplot(popu.freq,
        xlab = "popu levels",
        ylab = "frequency",
        ylim = c(0,200),
        col = brew.col,
        main = "Barplot popu")

## Histograms of numerical values
par(mfrow = c(2,2))
# Histogram 1: gen
hist(Arabidopsis$gen, 
     probability = TRUE)
lines(density(Arabidopsis$gen))
# Histogram 2: rack
hist(Arabidopsis$rack, 
     probability = TRUE)
lines(density(Arabidopsis$rack))
# Histogram 3: nutrient
hist(Arabidopsis$nutrient, 
     probability = TRUE)
lines(density(Arabidopsis$nutrient))
# Histogram 1: total fruits
hist(Arabidopsis$total.fruits, 
     probability = TRUE)
lines(density(Arabidopsis$total.fruits))

## rack and nutrient each have only 
## two numerical values
# Set as factor and calculate frequencies
Arabidopsis$rack <- as.factor(Arabidopsis$rack)
rack.freq <- table(Arabidopsis$rack)
Arabidopsis$nutrient <- as.factor(Arabidopsis$nutrient)
nutrient.freq <- table(Arabidopsis$nutrient)
# Prepare figure 
par(mfrow = c(1,2))
# Barplot 1: rack
bp1 <- barplot(rack.freq,
               col = brew.col,
               ylim = c(0,350),
               main = "Barplot rack")
# Add rack frequency on top of bars
text(x = bp1, y = rack.freq, 
     label = rack.freq, pos = 3)
# Barplot 2: nutrient
bp2 <- barplot(nutrient.freq,
               col = brew.col,
               ylim = c(0,350),
               main = "Barplot nutrient")
# Add nutrient frequency on top of bars
text(x = bp2, y = nutrient.freq, 
     label = nutrient.freq, pos = 3)

## Boxplots
# Prepare figure 
par(mfrow = c(1,2))
# Boxplot 1: reg
boxplot(gen~reg, data = Arabidopsis,
        col = brew.col,
        main = "Boxplot genotype")
# Boxplot 2: total.fruits
boxplot(total.fruits~reg, data = Arabidopsis,
        col = brew.col,
        main = "Boxplot total fruits")

## Scatterplot of genotype vs total fruits
par(mfrow=c(1,1))
# Set region symbols
reg.pch <- Arabidopsis$reg
levels(reg.pch) <- list("0"="NL",
                        "1"="SP",
                        "2"="SW")
reg.pch <- as.numeric(as.character(reg.pch))
# Set region colors
brewer.pal(3,"Set2")
reg.col <- Arabidopsis$reg
levels(reg.col) <- list("#66C2A5"="NL",
                        "#FC8D62"="SP",
                        "#8DA0CB"="SW")
iris.col <- as.character(iris.col)
## Scatterplot with fit line
plot(Arabidopsis$gen, Arabidopsis$total.fruits,
     main = "Arabidopsis genotype vs total fruits", 
     cex.main = 1.5,
     xlab = "Genotype", 
     ylab = "Total fruits",
     pch = reg.pch, cex = 0.7,
     col = as.character(reg.col))
# Genotyp is actually a factor 
# with 24 (numeric-valued) levels





################################################################################
### More datasets to explore
### http://vincentarelbundock.github.io/Rdatasets/datasets.html
################################################################################
# DAAG        # Frogs Data  # 212 rows and 11 columns
# DAAG        # leafshape   # 286 rows and 9 columns
# DAAG        # rice        # 72 rows and 7 columns
# gap         # crohn       # 387 rows and 212 columns
# gap         # hla         # 271 rows and 8 columns
# gap         # nep499      # 499 x 23
# robustbase  # epilepsy    # 236 x 6
# MASS        # Melanoma    # 205 x 7
# MASS        # birthwt     # 189 x 10
################################################################################