################################################################################
### Exercise 5: Dimensionality reduction
################################################################################
setwd("/media/sf_VMshare/BIT04-R/Rdatasets/")

################################################################################
### Prepare GTExdata from Taskesen (2016) 219,6 MB
################################################################################
GTExdata <- read.csv("Taskesen2016-GTExdata.csv", 
                     sep = ",", 
                     header = FALSE)
dim(GTExdata) # 1641 16142
GTExdata[1:4,1:6]
SampleLabels <- read.csv("Taskesen2016-SampleLabels.csv", 
                         sep = ",", 
                         header = TRUE)
dim(SampleLabels) # 1641 3
str(SampleLabels) # Tissue.type: 25 levels

# Change char to factor
class(SampleLabels$Tissue.type)
SampleLabels$Tissue.type <- as.factor(SampleLabels$Tissue.type)
class(SampleLabels$Tissue.type)
levels(SampleLabels$Tissue.type)
#  [1] "Adipose Tissue" "Adrenal Gland"  "Blood"          "Blood Vessel"   "Brain"          "Breast"        
#  [7] "Colon"          "Esophagus"      "Fallopian Tube" "Heart"          "Kidney"         "Liver"         
# [13] "Lung"           "Muscle"         "Nerve"          "Ovary"          "Pancreas"       "Pituitary"     
# [19] "Prostate"       "Skin"           "Stomach"        "Testis"         "Thyroid"        "Uterus"        
# [25] "Vagina" 






## SUBSET three clusters to use for dimensionality reduction
# cluster 5 (Muscle), cluster 6 (Artery| Blood Vessel), cluster 7 (Heart)
SampleLabels[SampleLabels$Tissue.type=="Muscle",]
SampleLabels[SampleLabels$Tissue.type=="Blood Vessel",]
SampleLabels[SampleLabels$Tissue.type=="Heart",]



SampleLabels[which(SampleLabels$Tissue.type=="Muscle"
                 | SampleLabels$Tissue.type=="Blood Vessel"
                 | SampleLabels$Tissue.type=="Heart"),]
# Subset 391 of 1641 samples
dim(SampleLabels[which(SampleLabels$Tissue.type=="Muscle"
                   | SampleLabels$Tissue.type=="Blood Vessel"
                   | SampleLabels$Tissue.type=="Heart"),])

subset <- SampleLabels[which(SampleLabels$Tissue.type=="Muscle"
                                           | SampleLabels$Tissue.type=="Blood Vessel"
                                           | SampleLabels$Tissue.type=="Heart"),]
subset_ids <- row.names(subset)
GTExsubset <- GTExdata[subset_ids,]
dim(GTExsubset) # 391 16142

## Set symbols
subset.pch <- subset$Tissue.type
levels(subset.pch) <- list("0"="Muscle",
                           "1"="Blood Vessel",
                           "2"="Heart")
subset.pch <- as.numeric(as.character(subset.pch))

## Set colors
library(RColorBrewer)
brewer.pal(3,"Set1")
subset.col <- subset$Tissue.type
levels(subset.col) <- list("#E41A1C"="Muscle",
                           "#377EB8"="Blood Vessel",
                           "#4DAF4A"="Heart")
subset.col <- as.character(subset.col)



################################################################################
## MDS GTExdata subset
################################################################################
d <- dist(GTExsubset) 
# k is the number of dimensions
fit <- cmdscale(d, eig = TRUE, k = 2)
# Plot
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, 
     xlab = "Coordinate 1", 
     ylab = "Coordinate 2",
     main = "MDS GTExdata subset",
     col = subset.col,
     pch = subset.pch, # type="n",
     cex = 0.7)



################################################################################
## tSNE
################################################################################
library(Rtsne)
set.seed(1)
tsne.out <- Rtsne(GTExsubset, 
                  #dims = 2, initial_dims = 50, 
                  perplexity = 10)
plot(tsne.out$Y,
     pch = subset.pch,
     col = subset.col,
     main = "tSNE subset GTExdata (perplexity=10)",
     #xlim = c(-200,200), 
     #ylim = c(-100,100),
     cex = 0.6)
# With perplexity = 30
tsne.out <- Rtsne(GTExsubset, 
                  #dims = 2, initial_dims = 50, 
                  perplexity = 30)
plot(tsne.out$Y,
     pch = subset.pch,
     col = subset.col,
     main = "tSNE subset GTExdata (perplexity=30)",
     xlim = c(-30,30), 
     ylim = c(-30,30),
     cex = 0.6)
# To add a legend
legend("topleft", 
       title = "Tissue type",
       #title.col = "black",
       cex = 0.8,
       legend = c("Muscle","Blood Vessel","Heart"),
       #text.col = unique(subset.col),
       pch = unique(subset.pch),
       col = unique(subset.col))



################################################################################
################################################################################
################################################################################