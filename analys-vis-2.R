################################################################################
### Working directory, packages and datasets
################################################################################
# Working directory
getwd()
setwd("/media/sf_VMshare/BIT04-R")
# Packages

# Iris dataset
data(iris)



################################################################################
### Graphical parameters and layout
################################################################################
## View the current settings
par()
# Now you can see all the parameters e.g. 
# $cex [1] 1 # $cex.axis [1] 1 # $cex.lab [1] 1 # $cex.main [1] 1.2 # $cex.sub [1] 1
## Make a histogram of petal length of the iris dataset with default par
hist(iris$Petal.Length)
## Save the default/original settings
opar <- par()
## Now set the main title with cex 1.5 and plot the histogram again
hist(iris$Petal.Length, 
     main = "Histogram of the petal length", 
     cex.main = 1.5, 
     cex.lab = 1.2)
hist(iris$Petal.Width) 
## The last settings were not used because 
## they were specified in the plot function, not in the par()
## Now specify them in the par function
par(cex.main = 1.5, cex.lab = 1.2)
hist(iris$Petal.Width)
## Restore original settings
par(opar)
hist(iris$Petal.Width)



################################################################################
### Plotting lines
################################################################################
d <- density(iris$Petal.Width)
hist(iris$Petal.Width, prob = TRUE,
     main = "Histogram of iris petal width with bimodal distribution", 
     cex.main = 1.4, cex.lab = 1.1)
lines(d, lty = 2, lwd = 3)


# Similarly for boxplot
boxplot(iris$Petal.Width, 
        data = iris,
        main = "Boxplot of\niris petal width", 
        cex.main = 1.6,
        lty = 1, lwd = 4)



################################################################################
### Plotting symbols
################################################################################
plot(iris$Sepal.Width)

plot(iris$Sepal.Width, 
     pch = 22, 
     cex = 0.7)

as.numeric(iris$Species)

plot(iris$Sepal.Width, 
     main = "Sepal width - symbol per Species",
     pch = as.numeric(iris$Species), 
     cex = 0.6)



################################################################################
### Axes
################################################################################
d <- density(iris$Petal.Length)
hist(iris$Petal.Length, prob = TRUE,
     main = "Histogram of iris petal length")
lines(d, lty = 3, lwd = 2)
# Now with the axes adjusted
hist(iris$Petal.Length, 
     prob = TRUE,
     main = "Histogram of iris petal length", 
     cex.main = 1.5,
     xlab = "petal length", 
     ylab = "density", 
     cex.lab = 1.2,
     xlim = c(0,8), 
     ylim = c(0,0.6))
lines(d, lty = 3, lwd = 2)



################################################################################
### Font, text and labels
################################################################################
# Make a subset of the iris dataset, 5 of each species
iris_subset <- iris[c(1:5,51:55,101:105),]

# Change the levels to the symbol numbers
levels(iris_subset$Species) <- list("1"="setosa","4"="versicolor","2"="virginica")
# as.numeric uses index 1,2,3
as.numeric(iris_subset$Species)
# Make numeric of character values and save in species.pch to use as symbols
as.character(iris_subset$Species)
species.pch <- as.numeric(as.character(iris_subset$Species))

# Plot the petal length of the 15 iris flowers 
# and set axes, titles and font
plot(iris_subset$Petal.Length, 
     main = "Petal length of 15 iris flowers", 
     pch = species.pch, 
     cex = 1.1, 
     family = "serif",
     xlab = "iris flower", 
     ylab = "petal length", 
     ylim = c(0,8))
# Add labels to each data point
text(iris_subset$Petal.Length, 
     pos = 3, 
     cex = 0.8, 
     family = "serif", 
     font = 2)



################################################################################
### Reference line
################################################################################
# Add horizontal and vertical reference lines to previous plot
abline(h = seq(1,8,2), 
       lty = 2, 
       col = "gray")
abline(v = seq(1,15,1),
       lty = 2, 
       col = "gray")

# Add additional horizontal reference lines at y = 1.5, 4.4, 5.7  
abline(h = c(1.5,4.4,5.7), 
       lty = 3, 
       lwd = 2, 
       col = "blue")



################################################################################
### Legend 
################################################################################
# Add legend to previous plot
legend("topleft", 
       title = "Species", 
       cex = 0.8,
       legend = c("setosa","versicolor","virginica"), 
       pch = c(1,4,2))

legend("bottomright", 
       title = "Species", 
       cex = 0.8,
       legend = c("setosa","versicolor","virginica"), 
       pch = c(1,4,2))



################################################################################
### Combining plots
################################################################################
par(mfrow = c(2,2), mar = c(6,6,4,4))
hist(iris$Sepal.Length, main = "Histogram of sepal length")
hist(iris$Sepal.Width, main = "Histogram of sepal width")
hist(iris$Petal.Length, main = "Histogram of petal length")
hist(iris$Petal.Width, main = "Histogram of petal width")

# Now repeat in a loop, set x-axis labels and ylim from 0 to 35
figures <- colnames(iris[,1:4]) # exclude column 5 (Species)
for(figure in figures) {
  hist(iris[,figure], 
       main = paste0("Histogram of ",figure),
       xlab = figure, 
       ylim = c(0,40))
}



################################################################################
### Layout
################################################################################
# Two figures (1,2) in row 1 and one in row 2 (3)
layout(matrix(c(1,2,3,3), 
              2, 
              2, 
              byrow = TRUE))

# Plot 1: barplot of iris Species frequencies
species.freq <- table(iris$Species)
barplot(species.freq, 
        main = "Barplot Species frequencies")

# Plot 2: boxplot of Petal.Length
boxplot(Petal.Length ~ Species, data = iris, 
        main = "Boxplot Petal.Length")

# Plot 3: histogram of Petal.Length
hist(iris$Petal.Length, 
     main = "Histogram Petal.Length")


## Repeat but make the barplot 1/3 and boxplot 2/3 in width
## and row 1: 2/5 and row 2: 3/5 (histogram)
# Two figures (1,2) in row 1 and one in row 2 (3)
layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE),
       widths = c(1,2), heights = c(2,3))
# Plot 1: barplot of iris Species frequencies
species.freq <- table(iris$Species)
barplot(species.freq, 
        main = "Barplot Species frequencies")
# Plot 2: boxplot of Petal.Length
boxplot(Petal.Length ~ Species, data = iris, 
        main = "Boxplot Petal.Length")
# Plot 3: histogram of Petal.Length
hist(iris$Petal.Length, 
     main = "Histogram Petal.Length")



################################################################################
### Advanced layout control
################################################################################
# Start a new plot
plot.new() 
# Set the margins
par(mar = c(3, 2, 2, 1) + 0.1) 
# Figure 1: Q-Q plot of versicolor vs virginica
par(fig = c(0,0.8,0,0.8), new=TRUE)
qqplot(iris$Sepal.Length[iris$Species=="versicolor"], 
       iris$Sepal.Length[iris$Species=="virginica"])
# Figure 2: boxplot iris$Species=="versicolor"
par(fig=c(0,0.8,0.8,1), new=TRUE)
boxplot(iris$Sepal.Length[iris$Species=="versicolor"], 
        horizontal=TRUE, axes=FALSE)
# Figure 3: boxplot iris$Species=="virginica"
par(fig=c(0.8,1,0,0.8),new=TRUE)
boxplot(iris$Sepal.Length[iris$Species=="virginica"], 
        axes=FALSE)
# Add a general title for the figure
mtext("Combined figure with fine control", 
      side=3, outer=TRUE, line=-3) 

# The position of the boxplots is not so good, let's do better
plot.new()
# A bit more margin on the left
par(mar = c(6, 6, 4, 2) + 0.1) 
par(fig=c(0,0.8,0,0.8), new=TRUE)
qqplot(iris$Sepal.Length[iris$Species=="versicolor"], 
       iris$Sepal.Length[iris$Species=="virginica"])
# For the top boxplot choose 0.55 rather than 0.8 
# so that the top figure will be pulled closer to the Q-Q plot
par(fig=c(0,0.8,0.55,1), new=TRUE)
boxplot(iris$Sepal.Length[iris$Species=="versicolor"], 
        horizontal=TRUE, axes=FALSE)
# For the right boxplot choose from 0.65 to 1 
# to pull it also closer to the Q-Q plot
par(fig=c(0.65,1,0,0.8),new=TRUE)
boxplot(iris$Sepal.Length[iris$Species=="virginica"], 
        axes=FALSE)
# Increase the size of the title (cex) a bit
mtext("Combined figure with fine control", 
      side=3, outer=TRUE, line=-3, cex = 1.4) 



################################################################################
### COLORS
################################################################################
## The 657 color names in R
colors()
par(mfrow = c(1,2))
# Use 4 color names from colors()
barplot(seq(1:4), col=c("aliceblue","chocolate","deepskyblue","gold"))
# Use 4 RGB values
barplot(seq(1:4), col=c("#900000","#009000","#000090","#909090"))

## grDevices # Graphics Devices and Support for Colours and Fonts
rgb(0,1,0)
rgb(0, 102, 0, maxColorValue = 255) 
hsv(h = 0.5, s = 0.5, v = 0.5) 
# col2rgb # color to RGB (red/green/blue) conversion
col2rgb(c("aliceblue","chocolate","deepskyblue","gold"))
# rgb2hsv # RGB to HSV conversion
rgb2hsv(240,248,255) 
# grDevices color palettes
par(mfrow = c(1,2))
heat.colors(4, alpha=1)
barplot(seq(1:4), col=heat.colors(4, alpha=1))
terrain.colors(4, alpha = 1)
barplot(seq(1:4), col=terrain.colors(4, alpha = 1))
# colorRamp color interpolation
par(mfrow = c(1,2))
cr.pal <- colorRampPalette(c("blue", "red"))( 4 )
barplot(seq(1:4), col=cr.pal)
cr.pal2 <- colorRampPalette(c("red", "black"))( 10 )
barplot(seq(1:10), col=cr.pal2)

## colorRamps
library(colorRamps) # install.packages("colorRamps")
par(mfrow = c(3,1))
# make a color map of 10 colors
# that runs from blue -> cyan -> yellow -> red
blue2red(8)
barplot(seq(1:8), col=blue2red(8))
# color map from blue -> magenta -> yellow -> green
blue2green(8)
barplot(seq(1:8), col=blue2green(8))
# color map from green -> cyan -> magenta -> red
green2red(8)
barplot(seq(1:8), col=green2red(8))
# other color maps
barplot(seq(1:8), col=blue2yellow(8))
barplot(seq(1:8), col=cyan2yellow(8))
barplot(seq(1:8), col=magenta2green(8))

## colorspace # Color space manupulation
library(colorspace) # install.packages("colorspace")
hex2RGB( rgb(0, 102, 0, maxColorValue = 255) ) 
hex2RGB( hsv(h = 0.5, s = 0.5, v = 0.5) )
mixcolor(0.5, RGB(1, 0, 0), RGB(0, 1, 0))
par(mfrow = c(1,2))
rb_col <- rainbow(8)
barplot(seq(1:8), col=rb_col)
rb_col2 <- rainbow_hcl(8)
barplot(seq(1:8), col=rb_col2)

## The RColorBrewer package
library(RColorBrewer) # install.packages("RColorBrewer")
par(mfrow=c(1,1))
display.brewer.all()
# SEQUENTIAL palettes names: 
  # Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd 
  # Purples RdPu Reds YlGn YlGnBu YlOrBr YlOrRd
# DIVERGING palettes names:
  # BrBG PiYG PRGn PuOr RdBu RdGy RdYlBu RdYlGn Spectral
# QUALITATIVE
  # Accent Dark2 Paired Pastel1	Pastel2	Set1 Set2 Set3
# Check colorbrewer2.org

# Have a look at 3 different palettes
par(mfrow=c(3,2))
# Show the BrBg colors
display.brewer.pal(4,"Set1")
# Return 6 colors from BrBG
brewer.pal(6,"BrBG") 
barplot(seq(1:6), col=brewer.pal(6,"BrBG"))
# Show the Set1 colors
display.brewer.pal(6,"Set1")
# Return four colors from Set1
brewer.pal(4,"Set1") 
barplot(seq(1:4), col=brewer.pal(4,"Set1"))
# Show the Paired colors
display.brewer.pal(10,"Paired")
# Return four colors from Set1
brewer.pal(10,"Paired") 
barplot(seq(1:10), col=brewer.pal(10,"Paired"))

# Boxplot of Petal.Length with colors
par(mfrow = c(1,1))
threecol <- brewer.pal(3,"Set2")
boxplot(Petal.Length ~ Species, 
        data = iris, 
        main = "Boxplot petal length", 
        pch = 19, 
        cex = 1.1,
        col = threecol, outcol = threecol,
        xlab = "Iris species", 
        ylab = "Petal length", 
        ylim = c(0,8),
        names = c("","",""))
mtext("setosa", side = 1, line = 1, 
      at = 1, las = 1, col = threecol[1])
mtext("versicolor", side = 1, line = 1, 
      at = 2, las = 1, col = threecol[2])
mtext("virginica", side = 1, line = 1,
      at = 3, las = 1, col = threecol[3])

# Histogram Petal.Length with colors
par(mfrow = c(1,1), mar = c(5,5,2,2))
d <- density(iris$Petal.Length)
hist(iris$Petal.Length, prob = TRUE,
     main = "Histogram of iris petal length", 
     xlab = "petal length", 
     ylab = "density", 
     xlim = c(0,8), 
     ylim = c(0,0.5),
     cex.main = 1.5, 
     cex.lab = 1.2,
     col = "skyblue", 
     col.lab = "indianred")
lines(d, lty = 3, lwd = 3, col = "indianred")



################################################################################
### Scatterplot
################################################################################
# Set plot layout
layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
# Set symbols
iris.pch <- iris$Species
levels(iris.pch) <- list("0"="setosa",
                         "1"="versicolor",
                         "2"="virginica")
iris.pch <- as.numeric(as.character(iris.pch))
# Set species colors
brewer.pal(3,"Set2")
iris.col <- iris$Species
levels(iris.col) <- list("#66C2A5"="setosa",
                         "#FC8D62"="versicolor",
                         "#8DA0CB"="virginica")
iris.col <- as.character(iris.col)
# First plot boxplots
# Boxplot of Petal.Length with colors
boxplot(Petal.Length ~ Species, data = iris, 
        main = "Boxplot petal length", 
        pch = 19, cex = 1.1,
        col = unique(iris.col),
        ylab = "Petal length", ylim = c(0,8),
        names = c("","",""))
mtext("setosa", side = 1, line = 1, 
      at = 1, las = 1, col = "#66C2A5")
mtext("versicolor", side = 1, line = 1, 
      at = 2, las = 1, col = "#FC8D62")
mtext("virginica", side = 1, line = 1, 
      at = 3, las = 1, col = "#8DA0CB")
# Boxplot of Sepal.Length with colors
boxplot(Sepal.Length ~ Species, data = iris, 
        main = "Boxplot sepal length", 
        pch = 19, 
        cex = 1.1,
        col = unique(iris.col),
        ylab = "Sepal length", 
        ylim = c(0,8),
        names = c("","",""))
mtext("setosa", side = 1, line = 1, 
      at = 1, las = 1, col = "#66C2A5")
mtext("versicolor", side = 1, line = 1, 
      at = 2, las = 1, col = "#FC8D62")
mtext("virginica", side = 1, line = 1, 
      at = 3, las = 1, col = "#8DA0CB")
# Next plot scatterplot
plot(iris$Petal.Length, 
     iris$Sepal.Length,
     main = "Petal length versus sepal length", 
     cex.main = 1.5,
     xlab = "Petal length", 
     ylab = "Sepal length",
     pch = iris.pch, 
     cex = 0.7,
     col = as.character(iris.col))
legend("topleft", cex = 0.7,
       legend = c("setosa","versicolor","virginica"), 
       pch = c(0,1,2), 
       col = unique(iris.col), 
       text.col = unique(iris.col))

## Scatterplot with fit line
par(mfrow = c(1,1), mar = c(5,5,4,2))
plot(iris$Petal.Length, iris$Sepal.Length,
     main = "Petal length versus sepal length", 
     cex.main = 1.5,
     xlab = "Petal length", 
     ylab = "Sepal length",
     pch = iris.pch, cex = 0.7,
     col = as.character(iris.col))
## Add fit lines
# Regression line (y~x)
abline(lm(iris$Sepal.Length ~ iris$Petal.Length), 
       col="red")
# Lowess line (x,y)
lines(lowess(iris$Petal.Length, iris$Sepal.Length), 
      col="blue", lty = 2, lwd = 1) 



## 3D scatterplot
library(scatterplot3d)
par(mfrow = c(1,1))
scatterplot3d(iris$Petal.Length, 
              iris$Sepal.Length, 
              iris$Petal.Width, 
              main = "3D scatterplot",
              color = as.character(iris.col))



################################################################################
## Spinning 3D scatterplot
# rgl package: 3D Visualization Using OpenGL
# Can be difficult to install!
## sudo dnf install mesa-libGL-devel mesa-libGLU-devel
################################################################################
## sudo dnf install openssl-devel libcurl-devel fribidi-devel 
## sudo dnf install freetype-devel libpng-devel libtiff-devel libjpeg-turbo-devel
## install.packages("devtools")
## library(devtools)
## Check versions on https://cran.r-project.org/src/contrib/Archive/rgl/
## install_version("rgl", version = "0.103.5", repos = "http://cran.us.r-project.org")
## 0.103.5 from 2020-11-23 | 0.105.13 from 2021-02-15
## Choose 2 for CRAN only
################################################################################
## install.packages("rgl")
library(rgl) 
plot3d(iris$Petal.Length, 
       iris$Sepal.Length, 
       iris$Petal.Width, 
       col = as.character(iris.col), 
       size = 8)



################################################################################
### Parallel coordinates
################################################################################
# parcoord() from the MASS package
library(MASS)
par(mfrow = c(1,1), mar = c(4,4,4,8), 
    xpd = TRUE)
# if xpd=TRUE, plotting is clipped to figure region
parcoord(iris[,1:4], 
         col = iris.col, 
         var.label = TRUE)
legend(x = 4 + 0.1, y = 0.6, cex = 0.7,
       legend = c("setosa","versicolor","virginica"), 
       pch = c(0,1,2), 
       col = unique(iris.col), 
       text.col = unique(iris.col))
## Other packages:
# parallel() from the lattice package
# cdparcoord package for categorical 
#  and discrete parallel coordinates



################################################################################
### Scatterplot matrix with pairs
################################################################################
## Basic scatterplot matrix
pairs(iris[1:4], 
      cex = 0.7,
      pch = iris.pch, 
      # Extra margin with oma() for legend
      # oma = c(3,3,3,15) 
      col = iris.col)
#par(xpd = TRUE)
#legend("bottomright", 
#       fill = unique(iris.col), 
#       legend = c( levels(iris$Species)))


################################################################################
### Bubble chart
################################################################################
## Scatterplot with a third "bubble" dimension
symbols(iris$Sepal.Length, 
        iris$Sepal.Width, 
        circles = iris$Petal.Length, 
        inches = 0.15,
        fg = iris.col, 
        bg = iris.col)
legend(
  "topright", 
  legend = unique(iris$Species), 
  pch = 21,
  bty = "n",
  col = unique(iris.col),
  pt.bg = unique(iris.col),         
  pt.cex = c(0.5,0.5,0.5)
)     


################################################################################
### Stacked and grouped barplot for multiple categorical variables
################################################################################
## Create HairGender testdata
hair <- c("black","brown","blond","black","black",
          "brown","brown","brown","blond","blond")
gender <- c("male","male","male","male","male",
            "female","female","female","female","female")
hairgender <- data.frame(hair,gender)
counts <- table(hairgender$hair, hairgender$gender)
## Stacked barplot
par(mfrow=c(1, 1), mar=c(5, 5, 4, 8))
barplot(counts, 
        main = "Hair and gender",
        xlab = "Gender", 
        col = c("black","yellow","red"),
        legend.text = TRUE,
        args.legend = list(x = "topright", 
                           bty = "n", 
                           inset=c(-0.3, 0))) 
## Grouped barplot
par(mfrow=c(1, 1), mar=c(5, 5, 4, 8))
barplot(counts, 
        main = "Hair and gender",
        xlab = "Gender", 
        col = c("black","yellow","red"),
        legend.text = TRUE,
        args.legend = list(x = "topright", 
                           bty = "n", 
                           inset=c(-0.3, -0.2)),
        beside = TRUE)



################################################################################
### Heatmap
################################################################################
# Create color palette from red to green
cr.pal <- colorRampPalette(c("red","yellow","green"))( 100 )
# Put iris subset in matrix as input for heatmap
iris_matrix <- data.matrix(iris_subset)
# Plot heatmap
iris_heatmap <- heatmap(iris_matrix, Colv=NA, #Rowv=NA, 
                        col = cr.pal, scale="column", 
                        margins = c(10,2))

 

################################################################################
### Run chart for measure and count data
################################################################################
# Load package qicharts2
library(qicharts2)
# Lock random number generator
set.seed(123)       
# Random values Poisson distribution 
# n = 20 (values), lambda = 16 (means)
mval <- rpois(20, 16)
# Run chart of mval
qic(mval, show.grid = TRUE, 
    title = "Run chart with 20 rpois values")              



################################################################################