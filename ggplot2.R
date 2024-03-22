################################################################################
### ggplot2
################################################################################
## Install ggplot2 package
install.packages("ggplot2")
# Load ggplot2 package
library(ggplot2)
# List of available geometric objects
help.search("geom_", package = "ggplot2")


## SCATTERPLOT with ggplot2
# Use the iris dataset
data(iris)
# Base graph
plot(iris$Sepal.Length, iris$Sepal.Width)
# Begin plot with ggplot() function 
# to create coordinate system
# Next add layer of points to plot: geom_point()
ggplot(data = iris) + 
  geom_point(mapping = aes(x = Sepal.Length, 
                           y = Sepal.Width))
# OR same plot with
ggplot(data = iris,
       aes(x = Sepal.Length, 
           y = Sepal.Width)) + 
  geom_point()
# With simple linear regression line
# Basic plot with lm line
iris.lm <- lm(iris$Sepal.Width ~ iris$Sepal.Length)
summary(iris.lm)
# iris.intercept = iris.lm$coefficients[1]
# iris.slope = iris.lm$coefficients[2]
plot(iris$Sepal.Length, iris$Sepal.Width)
abline(iris.lm, col = "red")
# Now in ggplot2
plot1<- ggplot(data = iris,
               aes(x = Sepal.Length, 
               y = Sepal.Width)) + 
          geom_point()
# Without confidence interval
plot1 + geom_point(aes(color = Species)) + 
  geom_smooth(method = "lm", se = FALSE)


## http://vincentarelbundock.github.io/Rdatasets/datasets.html
## Load the CrohnD dataset 
#  from the robustbase package
install.packages("robustbase") 
library(robustbase)
# Data set issued from a study 
# of the adverse events of a drug on 
# 117 patients affected by Crohn's disease 
# (a chronic inflammatory disease of the intestines)
data(CrohnD)
# Structure: 117 obs. 9 variables
str(CrohnD)
# The first five rows (patients)
CrohnD[1:5,]


## BARPLOT with ggplot2
# country.freq <- table(CrohnD$country)
# barplot(country.freq, ylim = c(0,100))
bp1 <- ggplot(CrohnD, aes(CrohnD$country))
# geom_bar uses stat="count" which 
# makes the height of the bar proportion 
# to the number of cases in each group
bp1 + geom_bar()
# Make it a stacked barplot incl. gender
bp2 <- ggplot(CrohnD, aes(CrohnD$country))
bp2 + geom_bar(aes(fill = CrohnD$sex))

# Make it a grouped barplot incl. gender
bp3 <- ggplot(CrohnD, aes(CrohnD$country))
bp3 + geom_bar(aes(fill = CrohnD$sex),
               position = "dodge")
# Custom title and labels of axes
# and change theme (white background)
bp4 <- ggplot(CrohnD, aes(CrohnD$country))
bp4 + geom_bar(aes(fill = CrohnD$sex),
               position = "dodge") +
  xlab("Country") + 
  ylab("Frequency by gender") +
  ggtitle("Countries and gender") +
  theme_bw()

# Change limits y-axis 
# and change legend
bp5 <- ggplot(CrohnD, aes(CrohnD$country))
bp5 + geom_bar(aes(fill = CrohnD$sex),
               position = "dodge") +
  xlab("Country") + 
  ylab("Frequency by gender") +
  ylim(0,70) +
  ggtitle("Countries and gender") +
# to remove the legend
  # guides(fill = FALSE) + 
# to remove the legend title
  # guides(fill = guide_legend(title = NULL)) + 
# to set legend title
  # guides(fill = guide_legend(title = "Gender")) + 
# set legend title and labels
  scale_fill_discrete(
    name = "Gender",
    breaks = c("M", "F"),
    labels = c("Male", "Female")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
# last line to center plot title


## HISTOGRAM with ggplot2
# histogram1
ggplot(CrohnD, aes(CrohnD$BMI)) + 
  geom_histogram()
# histogram2
ggplot(CrohnD, aes(CrohnD$BMI)) + 
  geom_histogram(aes(y =..density..),
                 binwidth = 0.8,
                 alpha = I(.25),
                 col = I("gray")) +
  xlab("BMI") + 
  xlim(10,50) +
  geom_density(col = "blue") +
  theme_bw()


## BOXPLOT with ggplot2
# A simple ggplot boxplot
bxp1 <- ggplot(CrohnD, aes(x = treat, y = nrAdvE)) +
  geom_boxplot()
bxp1
# Boxplot with custom axis labels and ticks,
# centered title, coloring, margins set
library(RColorBrewer)
bxp2 <- ggplot(CrohnD, aes(x = treat, y = nrAdvE)) +
  geom_boxplot(fill = brewer.pal(3,"Set2"), 
               colour = brewer.pal(3,"Set2"), 
               alpha = 0.7) + 
  scale_x_discrete(name = "Treatment") +
  scale_y_continuous(name = "Number of adverse events",
                     breaks = seq(0, 14, 2),
                     limits=c(0, 14)) +
  ggtitle("Boxplot of number of adverse events per treatment") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1, 1, 2, 2, "cm")) 
bxp2


## FACETING - SMALL MULTIPLES
# Generate data for 12 samples,
# 100 values (total 1200 rows)
Measurement <- rep(1:100, each = 12)
Measurement[1:36]
Samples <- factor(rep(paste0("Sample",1:12),100),
                  levels = paste0("Sample",1:12))
# factor and levels set for correct order
Samples[1:36]
Values <- rnorm(1200)
Values[1:36]
df <- data.frame(Measurement, Samples, Values)
df[1:24,]
# Plot the data as lines
p1 <- ggplot(df, aes(Measurement, Values)) + 
  geom_line(aes(color = Samples))
p1
# Now with faceting
p2 <- ggplot(df, aes(Measurement, Values)) + 
  geom_line(aes(color = Samples)) + 
  facet_wrap(~Samples, ncol = 4)
p2
# Same but with data points 
p3 <- ggplot(df, aes(Measurement, Values)) + 
  geom_point(aes(color = Samples),
             size = 0.7) + 
  facet_wrap(~Samples, ncol = 4)
p3


## ggrepel: Repulsive Text and Label Geoms for 'ggplot2'
install.packages("ggrepel")
library(ggrepel)
# Scatterplot CrohnD height vs weight
s1 <- ggplot(data = CrohnD,
               aes(x = height, 
                   y = weight)) 
s1 + geom_point(aes(color = treat))
# With labels
s2 <- ggplot(data = CrohnD,
             aes(x = height, 
                 y = weight)) + 
      geom_point(aes(color = treat)) +
      geom_text(aes(height, weight, 
                    label = ID))
s2
# Repel text labels and use faceting
s3 <- ggplot(data = CrohnD,
             aes(x = height, 
                 y = weight)) + 
  geom_point(aes(color = treat)) +
  geom_text_repel(aes(height, weight, 
                      label = ID)) + 
  facet_wrap(~treat, ncol = 1)
s3
# With fancy labels
s4 <- ggplot(data = CrohnD,
             aes(x = height, 
                 y = weight)) + 
  geom_point(aes(color = treat)) +
  geom_label_repel(
    aes(height, weight, fill = factor(treat), label = ID),
    fontface = 'bold', color = 'white', size = 3,
    box.padding = 0.35, point.padding = 0.5,
    segment.color = 'grey50'
  ) +
  facet_wrap(~treat, ncol = 1)
s4



################################################################################
################################################################################
################################################################################