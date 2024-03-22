################################################################################
### Correlation and regression
################################################################################
## Iris dataset
data(iris)
# Data petal length and sepal length
iris[,c("Petal.Length","Sepal.Length")]


## Correlation
# Most usual measure is Pearson coefficient:
# covariance of two variables divided by 
# the product of their variance (scaled -1 to 1)
#  1 = perfect positive correlation
# -1 = perfect negative correlation
#  0 = complete randomness
cor(iris[,c("Petal.Length","Sepal.Length")], 
    method = "pearson")
# Test if this coefficient is significant
cor.test(iris$Petal.Length, iris$Sepal.Length, 
         method = "pearson")
# p-value < 2.2e-16


## Linear regression: fit linear model
iris.lm <- lm(iris$Sepal.Length ~ iris$Petal.Length)
iris.lm
summary(iris.lm)
# Scatterplot
plot(iris$Petal.Length, iris$Sepal.Length,
     main = "Petal length versus sepal length", 
     cex.main = 1.5, 
     cex = 0.7,
     xlim = c(0,8),
     ylim = c(0,8),
     xlab = "Petal length", 
     ylab = "Sepal length")
# Add regression line
abline(iris.lm, col = "red")
abline(iris.lm$coefficients[1], # intercept 
       iris.lm$coefficients[2], # slope
       col = "blue",
       lty = 3, 
       lwd = 3)

# Check model assumptions graphically
par(mfrow=c(2,2))
plot(iris.lm)
# --> first columns look at variance homogeneity
# --> normally no pattern in the dots, 
#     just random clouds of points
# --> graph on the top right checks normality assumptions
# --> if data are normally distributed points should 
#     fall more or less in a straight line



################################################################################
### Ancova: Analysis of covariance
################################################################################
setwd("/media/sf_VMshare/BIT04-R/Rdatasets/")
crickets <- read.csv("crickets-dataset.csv", 
                     sep = ",", header = TRUE)
str(crickets)
# Set species colors
crickets$Species <- as.factor(crickets$Species)
crickets.col <- crickets$Species
levels(crickets.col) <- list("black"="Oexcl",
                             "red"="Oniv")
crickets.col <- as.character(crickets.col)
crickets.col
## Plot data
par(mfrow=c(1,1))
plot(crickets$Temp,
     crickets$PulsesPerSec,
     col = crickets.col,
     pch = 16, cex = 0.8,
     xlab = "Temperature",
     ylab = "Pulse")
legend('bottomright',
       legend = levels(crickets$Species),
       col = 1:2, pch = 16, cex = 0.8)
## Mean temp
mean(crickets[,"Temp"]) # 23.76°C
mean(crickets[crickets$Species=="Oexcl","Temp"]) # 22.12°C
mean(crickets[crickets$Species=="Oniv","Temp"])  # 25.76°C
# Average temp 3.6°C higher in O. exclamationis
## Mean pulses
mean(crickets[,"PulsesPerSec"]) # 72.89
mean(crickets[crickets$Species=="Oexcl","PulsesPerSec"]) # 85.59
mean(crickets[crickets$Species=="Oniv","PulsesPerSec"])  # 62.43
# O. exclamationis higher pulse rate
# Higher rate might be at certain temp
# Control for temp with ancova

## Analysis of covariance (ancova)
## Model 1 (with interaction with covariate)
crickets.m1 <- aov(PulsesPerSec ~ Temp + Species + Temp:Species,
                   data = crickets)
# or same
# crickets.m1 <- aov(PulsesPerSec ~ Temp * Species,
#                    data = crickets)
summary(crickets.m1)
# --> both temp and species have significant 
#     effect on pulses per sec
# --> but interaction not significant: p-value 0.2542 
# --> slope across species not different

## Model 2 (without interaction)
crickets.m2 <- aov(PulsesPerSec ~ Temp + Species,
                   data = crickets)
summary(crickets.m2)

## Compare models
anova(crickets.m1,crickets.m2)

## Plot data with both regression lines
par(mfrow=c(1,1))
plot(crickets$Temp, crickets$PulsesPerSec,
     col = crickets$Species, pch = 16, cex = 0.8,
     xlab = "Temperature", ylab = "Pulse")
legend('bottomright', legend = levels(crickets$Species),
       col = 1:2, pch = 16, cex = 0.8)
## Regression lines
# Y = a + bX --> a is Y intercept and b is the slope
# Intercept and B (Temp) values from crickets.m2
Intercept = crickets.m2$coefficients[1]
B = crickets.m2$coefficients[2]
# For Oexcl species
I1 = Intercept + 0 
# For Oniv species
I2 = Intercept + crickets.m2$coefficients[3]
# Add lines to plot
abline(I1, B, col = "black")
abline(I2, B, col = "red")



################################################################################
### Spearman rank correlation
################################################################################
setwd("/media/sf_VMshare/BIT04-R/Rdatasets/")
## Monkeys & nematodes eggs dataset in Rdatasets3
monkeys <- read.csv("monkey-nematode-dataset.csv", 
                    sep = ",", header = TRUE)
library(Hmisc)
rcorr(monkeys$DominanceRank, 
      monkeys$EggsPerGramRank, 
      type = "spearman")
# --> rho = 0.94 
# --> p-value = 0.0048

# Scatterplot
plot(monkeys$DominanceRank, monkeys$EggsPerGramRank,
     main = "Monkey ranks: dominance vs eggs per gram", 
     cex.main = 1.5, pch = 5, cex = 0.9,
     xlab = "Dominance rank", 
     ylab = "Eggs per gram rank")
abline(0, 1, col="gray80", lty = 3)



################################################################################
### Multiple regression
################################################################################
setwd("/media/sf_VMshare/BIT04-R/Rdatasets/")
fish <- read.csv("fish-dataset.csv", 
                 sep = ",", header = TRUE)
str(fish)
# only use numeric data or you will get error
fish.num <- fish[,2:8]
## Correlation matrix
cor(fish.num, use = "pairwise", 
    method="pearson")
## Visualize 
# Pairwise plot
pairs(data = fish.num,
      ~ Longnose + Acerage + DO2 + Maxdepth + NO3 + SO4 + Temp)
# Pairwise correlation chart with histograms
# Result cor.test as stars
# install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(fish.num, 
                  method = "pearson",
                  histogram = TRUE, 
                  pch = 16)

## Multiple regression
model.null = lm(Longnose ~ 1,
                data = fish.num)
model.full = lm(Longnose ~ Acerage + DO2 + Maxdepth + NO3 + SO4 + Temp,
                data = fish.num)
# Model selection with step function
# direction forward --> add to model
# direction backward --> remove from model
# direction both --> both adds and removes
# Uses AIC = Akaike information criterion
# Add test="F" to see p-values
step(model.null,
     scope = list(upper = model.full),
     direction = "both",# test="F",
     data = fish.num)
# Function will follow procedure to find 
# model with the lowest AIC, shown at end.
# Acreage, nitrate, and maximum depth 
# contribute to the multiple regression equation.
model.final = lm(Longnose ~ Acerage + Maxdepth + NO3,
                 data = fish.num)
summary(model.final)

# Plot final model
plot(model.final, pch = 16, which = 1)
# the residuals plot of this last model shows some important points 
# still lying far away from the middle area of the graph

model.final$coefficients # OR coefficients(model.final)
# --> Y = a + b1X1 + b2X2 + b3X3 where b1 to b3 are the coefficients

  

################################################################################
### Simple logistic regression
################################################################################
setwd("/media/sf_VMshare/BIT04-R/Rdatasets/")
amphipod <- read.csv("amphipod-slogreg.csv", 
                      sep = ",", header = TRUE)
# nominal (dependent): Mpi90 and Mpi100 (alleles amphipods)
# measurement: latitude
str(amphipod)
amphipod$Total <- amphipod$Mpi90 + amphipod$Mpi100
amphipod$Percent <- amphipod$Mpi100 / amphipod$Total

## Model fitting with glm()
# family: link function to determine model to use
# Trials matrix: "successes", "failures" columns
# --> here Mpi100, Mpi90 columns
# can also be factor with success/failure levels
Trials <- cbind(amphipod$Mpi100, amphipod$Mpi90)
Trials
amphipod.model <- glm(Trials ~ Latitude,
                      data = amphipod,
                      family = binomial(link=logit))
summary(amphipod.model)

## Pseudo-R-squared
# R does not produce r-squared values for glm
# Function nagelkerke from rcompanion package 
# will calculate McFadden, Cox and Snell, 
# and Nagelkereke pseudo-R-squared for glm
# Cox and Snell is also called the ML
# Nagelkerke is also called Cragg and Uhler
# install.packages("rcompanion")
library(rcompanion)
nagelkerke(amphipod.model)
# --> pseudo-r2 = 0.999970
# --> chi2 = 83.3
# --> p-value = 7.0476e-20

## Overall p-value model
# Testing logistic regression p-value uses Chi-square
# Update produces null model for comparison
anova(amphipod.model,
      update(amphipod.model, ~1),
      test = "Chisq")

## Visualize model
plot(Percent ~ Latitude, data = amphipod,
     xlab = "Latitude", ylab = "Percent Mpi100",
     pch = 19)             
curve(predict(amphipod.model, 
              data.frame(Latitude=x), 
              type="response"),
      lty = 1, lwd = 2, col = "blue", add = TRUE)



################################################################################
################################################################################
################################################################################