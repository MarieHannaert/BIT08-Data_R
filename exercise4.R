#4.1
setwd("C:/Users/11901250/Documents/SF/BIT08-Data_R") #windows
read.csv(file="Rdatasets-v2.1/urine.csv", header = TRUE)
urine_data <- read.csv(file="Rdatasets-v2.1/urine.csv", header = TRUE)
par(mfrow = c(1,2))
boxplot(calc ~ r, data=urine_data)
boxplot(calc ~ r, data=urine_data,
        main = "Boxplot calcium concentration urine dataset", 
        cex = 0.9,
        pch = 4, 
        cex.main = 1.0, 
        xlab = "presens of calcium oxalate crystals", 
        names =c("no", "yes"),
        ylab = "calcium concentration (mM/L)",
        ylim = c(0,15))
  
urine_data$r <- as.factor(urine_data$r)
levels(urine_data$r) <- list("no"="0",
                             "yes"="1")

#4.2
library(lme4)
data("Arabidopsis")
str(Arabidopsis)
summary(Arabidopsis)
layout(matrix(c(1,2,3,4,4,4), 2, 3, byrow = TRUE))
ref.freq <- table(Arabidopsis$reg)
barplot(ref.freq, 
        main = "Barplot ref", xlab = "reg levels", ylab = "frequency", ylim = c(0,400), col=brewer.pal(9,"Paired"))
amd.freq <- table (Arabidopsis$amd)
barplot(amd.freq, 
        main = "Barplot amd", xlab = "amd levels", ylab = "frequency", ylim = c(0,400),col=brewer.pal(9,"Paired"))
status.freq <- table(Arabidopsis$status)
barplot(status.freq, 
        main = "Barplot status", xlab = "status levels", ylab = "frequency", ylim = c(0,400),col=brewer.pal(9,"Paired"))
popu.freq <- table(Arabidopsis$popu)
barplot(popu.freq, 
        main = "Barplot popu", xlab = "popu levels", ylab = "frequency", ylim = c(0,400),col=brewer.pal(9,"Paired"))
par(mfrow=c(2,2), mar = c(5,5,4,2))
hist(Arabidopsis$gen, probability = T, ylab = "density")
lines(density(Arabidopsis$gen), col="black")
hist(Arabidopsis$rack, probability = T, ylab = "density")
lines(density(Arabidopsis$rack), col="black")
hist(Arabidopsis$nutrient, probability = T, ylab = "density")
lines(density(Arabidopsis$nutrient), col="black")
hist(Arabidopsis$total.fruits, probability = T, ylab = "density")
lines(density(Arabidopsis$total.fruits), col="black")

par(mfrow=c(1,2))
rack.freq <- table(as.factor(Arabidopsis$rack))
bp1 <- barplot(rack.freq, 
        main = "Barplot rack", xlab = "rack levels", ylab = "frequency", ylim = c(0,400),col=brewer.pal(9,"Paired"))
text(x= bp1, y = rack.freq, pos=3, labels = rack.freq)
nutrient.freq <- table(as.factor(Arabidopsis$nutrient))
bp2 <- barplot(nutrient.freq, 
        main = "Barplot nutrient", xlab = "nutrient levels", ylab = "frequency", ylim = c(0,400),col=brewer.pal(9,"Paired"))
text(x = bp2, y = nutrient.freq, pos =3, labels = nutrient.freq)

brew.col <- brewer.pal(9, "Paired")
par(mfrow = c(1,2))
boxplot(gen~reg, data = Arabidopsis,
        col = brew.col,  
        main= "boxplot genotype")
boxplot(total.fruits~reg, data = Arabidopsis, 
        col = brew.col,
        outline= FALSE,
        main ="Boxplot total fruits")

par(mfrowc(1,2))
reg.col <- Arabidopsis$reg
levels(reg.col) <- list("")
plot(Arabidopsis$gen, Arabidopsis$total.fruits, 
     main= "Arabidopsis genotype vs total fruits",
     cex.main=1.5, 
     xlab= "Genotype",
     ylab= "Total fruits",
     ylim = c(0,400),
     col=brew.col,
     )
