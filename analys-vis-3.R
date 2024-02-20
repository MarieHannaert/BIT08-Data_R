################################################################################
### MultiDimensional Scaling (MDS)
################################################################################
## Dataset to use
data(iris)
# Set symbols
iris.pch <- iris$Species
levels(iris.pch) <- list("0"="setosa",
                         "1"="versicolor",
                         "2"="virginica")
iris.pch <- as.numeric(as.character(iris.pch))
# Set species colors
library(RColorBrewer)
brewer.pal(3,"Set2")
iris.col <- iris$Species
levels(iris.col) <- list("#66C2A5"="setosa",
                         "#FC8D62"="versicolor",
                         "#8DA0CB"="virginica")
iris.col <- as.character(iris.col)

## Classical MDS
# N rows (objects) x p columns (variables)
# each row identified by a unique row name
# default: Euclidean distances between the rows
d <- dist(iris[,1:4]) 
# k is the number of dimensions
fit <- cmdscale(d, eig = TRUE, k = 2)
# View results
fit
# Plot
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, 
     xlab = "Coordinate 1", 
     ylab = "Coordinate 2",
     main = "MDS using Euclidean distance",
     col = iris.col,
     pch = 1, # type="n",
     cex = 0.7)
#text(x, y, labels = row.names(iris), cex=.7) 

## Using Manhattan distance
dM <- dist(iris[,1:4], method = "manhattan") 
# k is the number of dimensions
fitM <- cmdscale(dM, eig = TRUE, k = 2)
# View results
fitM
# Plot
xM <- fitM$points[,1]
yM <- fitM$points[,2]
plot(xM, yM, 
     xlab = "Coordinate 1", 
     ylab = "Coordinate 2",
     main = "MDS using Manhattan distance",
     col = iris.col,
     pch = 1, # type="n",
     cex = 0.7)



################################################################################
### Principal Component Analysis (PCA)
################################################################################
## Log transform 
iris.log <- log(iris[, 1:4])
iris.species <- iris[, 5]
## Apply PCA 
# scale=TRUE advisable but default FALSE
iris.pca <- prcomp(iris.log,
                   center = TRUE,
                   scale. = TRUE) 

## Have a look at the computed PCs 
print(iris.pca)

## Plot variances (y-axis) associated with PCs (x-axis)
## Useful to decide how many PCs to keep for further analysis
plot(iris.pca, type = "l")
# First two PCs explain most of the variability in the data

## Look at importance of PCs with summary
summary(iris.pca)

## Plot PCs
plot(iris.pca$x[,1:2], 
     col = iris.col)

## 3D PCA plot
library("rgl") # for 3D real-time rendering system
pcaData <- iris[, 1:4]
## Compute PCs
pca<-prcomp(scale(pcaData))
head(pca) # usefull to check axis
matrixPCA <- cbind(pca$x[,1],pca$x[,2],pca$x[,3])
## Plot PCA
pcaPlot <- plot3d(matrixPCA, 
                main = "",
                pch = 1, 
                type = "s",
# 'p' points, 's' spheres, 'l' lines, 'h' line segments
                radius = 0.15,
                xlab = "pc1", 
                ylab = "pc2", 
                zlab = "pc3",
                col = iris.col)
## Save 3D
# dirToSave<-paste(savePath, "results/",sep="")
# writeWebGL(dir = dirToSave, filename = file.path(dirToSave, "pagename.html"),
#            template = system.file(file.path("WebGL", "template.html"), package = "rgl"),
#            prefix = "",
#            snapshot = TRUE, font = "Arial", width = 1200, height = 800)



################################################################################
### t-distributed Stochastic Neighbor Embedding (t-SNE)
################################################################################
library(xlsx)
library(Rtsne)
library(RColorBrewer)
## count data of 2000 genes of 2 cell types  (c and i) of 8 patients
setwd("/home/pacoh/Dropbox/howest/BIT07-R/Rdatasets/")
countData <- read.csv(file="countData2000.csv", header = TRUE)
colnames(countData)
countData[1:2,1:6]
rownames(countData) <- countData$X
countData$X <- NULL
dim(countData) # 2000 rows (genes) x 15 columns (samples)
## PCA
# Cannot scale because of zero expression genes
which(apply(countData, 1, var)==0)
countData["AA06",]
countDataNo0 <- countData[apply(countData, 1, var)!= 0,]
dim(countDataNo0) # 1899 15
# log will result in infinite values in countData.log
countData.pca <- prcomp(t(countDataNo0),
                        center = TRUE,
                        scale = TRUE) 
plot(countData.pca, type = "l")
summary(countData.pca)
plot(countData.pca$x[,1:2])
# Add labels to each data point
text(x = countData.pca$x[,1], y = countData.pca$x[,2], 
     labels = colnames(countData), pos = 3, 
     cex = 0.8, family = "serif", font = 2)

## t-SNE
set.seed(1)
tsne.out <- Rtsne(t(countData), 
                  dims = 2, initial_dims = 15, 
                  perplexity = 4)
plot(tsne.out$Y,
     pch = 21,
     main = "tSNE perplexity=5",
     cex = 0.6,
     xlim = c(-200,200), ylim = c(-100,100))
# Add labels to each data point
text(x = tsne.out$Y[,1], y = tsne.out$Y[,2], 
     labels = colnames(countData), pos = 3, 
     cex = 0.8, family = "serif", font = 2)
# The "c" and "i" samples are grouped together



################################################################################
################################################################################
################################################################################