################################################################################
### Exercise 9.1: ggplot2 exercise
################################################################################
## Load the hla dataset from the gap package (Genetic analysis package)
install.packages("gap")
library(gap)


# Dataset contains HLA markers DRB, DQA, DQB and 
# phenotypes of 271 Schizophrenia patients (y=1) and controls (y=0)
# genotypes for 3 HLA loci have prefixes name (e.g., "DQB") and 
# a suffix for each of two alleles (".a1" and ".a2").
data(hla)
# Structure: 271 obs. 8 variables
str(hla)
# The first five rows (patients)
hla[1:5,]
## Make boxplots for each of the markers x alleles
library(ggplot2)
library(RColorBrewer)
library(ggthemes) # install.packages("ggthemes")
library(gridExtra) # install.packages("gridExtra")
markers.alleles <- colnames(hla[,3:8])
markers.alleles
darkcols <- brewer.pal(3,"Dark2") # minimal value for n is 3
hla2col <- darkcols[1:2]
################################################################################
## Version 1
ggplot(hla, aes(x = id, y = DQR.a1)) +
  geom_boxplot()
################################################################################

## Version 2
bxp2 <- ggplot(hla, aes(x = id, y = DQR.a2)) +
  geom_boxplot(colour = hla2col) + 
  scale_x_discrete(name = "Patient group") +
  scale_y_continuous(name = "Alleles",
                     breaks = seq(0, 26, 2),
                     limits=c(0, 26)) +
  ggtitle("Boxplot DQR.a2") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(2, 2, 2, 2, "cm")) 
bxp2

################################################################################

# Version 3
boxplot.DQR.a1 <- ggplot(hla, aes(x = id, y = DQR.a1)) +
                    geom_boxplot(colour = hla2col) +
                    scale_x_discrete(name = "Patient group") +
                    scale_y_continuous(name = "Alleles",
                                       breaks = seq(0, 26, 2),
                                       limits=c(0, 26)) +
                    ggtitle("Boxplot DQR.a1") +
                    theme_economist()+
                    theme(plot.title = element_text(hjust = 0.5),
                          plot.margin = margin(2, 2, 2, 2, "cm"))
boxplot.DQR.a2 <- ggplot(hla, aes(x = id, y = DQR.a2)) +
                    geom_boxplot(colour = hla2col) +
                    scale_x_discrete(name = "Patient group") +
                    scale_y_continuous(name = "Alleles",
                                       breaks = seq(0, 26, 2),
                                       limits=c(0, 26)) +
                    ggtitle("Boxplot DQR.a2") +
                    theme_economist() + 
                    theme(plot.title = element_text(hjust = 0.5),
                          plot.margin = margin(2, 2, 2, 2, "cm"))
grid.arrange(boxplot.DQR.a1, boxplot.DQR.a2, ncol = 2)



################################################################################
################################################################################
################################################################################