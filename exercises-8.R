################################################################################
### Exercise 8.1: Sleep in mammals
### --> general exercise including multiple regression and PCA
################################################################################
## Load dataset and look at structure
setwd("/media/sf_VMshare/BIT04-R/Rdatasets/")
sleep <- read.csv("sleep-in-mammals.csv", 
                  sep = ",", header = TRUE)
str(sleep)
# 11 variables for 62 mammalian species include
#  1 - species
#  2 - body weight (kg)
#  3 - brain weight (g), 
#  4 - slow wave ("nondreaming") sleep or SWS (hrs/day)
#  5 - paradoxical ("dreaming") sleep or PS (hrs/day)
#  6 - total sleep (hrs/day)
#  7 - max lifespan (years), 
#  8 - gestation time (days), 
#  9 - predation index (1=least preyed upon,5=most preyed upon)
# 10 - sleep exposure index (1=least exposed,5=most exposed)
# 11 - overall danger index (1=least danger,5=most danger)
# depend. var. SLEEP: SWS, PS
# (total sleep inc. with independ. vars)
# independ. CONSTITUTIONAL: lifespan, bodyweight, brain weight, gestation time
# independ. ECOLOGICAL: predation, exposure, danger


## Missing values
is.na(sleep)
# List rows of data that have missing values
sleep[!complete.cases(sleep),]
# Number of rows with NA values = 20
nrow(sleep[!complete.cases(sleep),])
# Other solutions how to remove Rows with NA in R Data Frame (6 Examples)
# https://statisticsglobe.com/r-remove-data-frame-rows-with-some-or-all-na


## From all 62 species to 39 used in paper
# 42 species with complete data rows 
nrow(sleep[complete.cases(sleep),])
sleep.complete <- sleep[complete.cases(sleep),]
# In paper 39 species used --> remove:
# Echidna, Lessershort-tailedshrew, Muskshrew
sleep39 <- sleep.complete[which(sleep.complete$Species!="Echidna" 
                        & sleep.complete$Species!="Lessershort-tailedshrew" 
                        & sleep.complete$Species!="Muskshrew"),]
nrow(sleep39)


## Correlation (pearson, kendall, spearman)
# Species names and numeric data for correlation
sleep39.species <- sleep39[,1]
sleep39.num <- sleep39[,2:11]

# Calculate correlation
cor.pearson <- cor(sleep39.num, use = "pairwise", 
    method="pearson")
cor.kendall <- cor(sleep39.num, use = "pairwise", 
    method="kendall")
cor.spearman <- cor(sleep39.num, use = "pairwise", 
    method="spearman")

# View correlation (not same as in paper)
# Spearman most similar, Kendall least similar
cor.pearson  # SWS/PS: 0.52979649 vs 0.582 in paper
cor.kendall  # SWS/PS: 0.4165028 vs 0.582 in paper
cor.spearman # SWS/PS: 0.5917686 vs 0.582 in paper


# Pairwise plots
colnames(sleep39.num)
pairs(data = sleep39.num,
      ~ TotalSleep + BodyWt + BrainWt + LifeSpan + Gestation + Predation + Exposure + Danger)
# OR
library(PerformanceAnalytics)
chart.Correlation(sleep39.num, method = "spearman",
                  histogram = TRUE, pch = 16)


## Multiple regression slow wave ("nondreaming") sleep or SWS
SWSmodel.null = lm(NonDreaming ~ 1,
                data = sleep39.num)
SWSmodel.full = lm(NonDreaming ~ LifeSpan + BodyWt + BrainWt + Gestation + Predation + Exposure + Danger,
                   data = sleep39.num)
# Model selection with step function
step(SWSmodel.null,
     scope = list(upper = SWSmodel.full),
     direction = "both", test="F",
     data = sleep39.num)

# Function will follow procedure to find model with lowest AIC shown at end
SWSmodel.final = lm(NonDreaming ~ Gestation + Danger,
                    data = sleep39.num)
summary(SWSmodel.final)
# For SWS as dependent variable
# --> gestation and danger < 0.01


## Multiple regression paradoxical ("dreaming") sleep or PS
PSmodel.null = lm(Dreaming ~ 1,
                  data = sleep39.num)
PSmodel.full = lm(Dreaming ~ LifeSpan + BodyWt + BrainWt + Gestation + Predation + Exposure + Danger,
                  data = sleep39.num)
# Model selection with step function
step(PSmodel.null,
     scope = list(upper = PSmodel.full),
     direction = "both", test="F",
     data = sleep39.num)
# Function will follow procedure to find model with lowest AIC shown at end
PSmodel.final = lm(Dreaming ~ Danger + Predation + BodyWt + Gestation,
                    data = sleep39.num)
summary(PSmodel.final)
# For PS as dependent variable
# --> danger, predation, bodywt and gestation < 0.01


## REGRESSION EQUATION: Y(PS) = a + b1X1 + b2X2 + b3X3
# --> can be used for prediction
coefficients(PSmodel.final)
a.intercept = coefficients(PSmodel.final)[1]
b1.TotalSleep = coefficients(PSmodel.final)[2]
b2.BodyWt = coefficients(PSmodel.final)[3]
b3.Danger = coefficients(PSmodel.final)[4]

#plot(PSmodel.final, pch = 16, which = 1)

## Principal component analysis (PCA)
# Log transform 
sleep39.log <- log(sleep39.num)
sleep39.log 
sleep39.num[11,] # Dreaming = 0
sleep39.log[11,] # Dreaming = -Inf
# one -Inf value will give error infinite or missing values in 'x'
# when computing PCA with prcomp
# add 0.0001 to all
sleep39.num0001 <- sleep39.num + 0.0001
sleep39.num0001[11,] # Dreaming = 0
sleep39.log <- log(sleep39.num0001)
sleep39.log[11,] # Dreaming = -Inf
# Apply PCA
sleep39.pca <- prcomp(sleep39.log,
                      center = TRUE,
                      scale = TRUE) 
# Have a look at computed PCs 
print(sleep39.pca)
plot(sleep39.pca, type = "l")
# Look at importance of PCs with summary
summary(sleep39.pca)
# 3D plot PCs
library("rgl")
matrixPCA <- cbind(sleep39.pca$x[,1],sleep39.pca$x[,2],sleep39.pca$x[,3])
## Plot PCA
pcaPlot <- plot3d(matrixPCA, 
                  main = "",
                  pch = 1, 
                  type = "s",
                  # 'p' points, 's' spheres, 'l' lines, 'h' line segments
                  radius = 0.1,
                  xlab = "PC1", 
                  ylab = "PC2", 
                  zlab = "PC3")
text3d(x = matrixPCA[,1], 
       y = (matrixPCA[,2]-0.2), 
       z = (matrixPCA[,3]), 
       sleep39$Species,
       cex = 0.6)



################################################################################
################################################################################
################################################################################