setwd("C:/Users/11901250/Documents/SF/BIT08-Data_R") #windows
#read.csv(file="Rdatasets-v2.1/Taskesen2016-GTExdata.csv", header = FALSE)
data_HT <- read.csv(file="Rdatasets-v2.1/Taskesen2016-GTExdata.csv", header = FALSE)
#read.csv(file="Rdatasets-v2.1/Taskesen2016-SampleLabels.csv", header = TRUE)
label_HT <- read.csv(file="Rdatasets-v2.1/Taskesen2016-SampleLabels.csv", header = TRUE)

#summary(data_HT)
#summary(label_HT)

str(data_HT)
str(label_HT)

subset_label <- label_HT[which(label_HT$Tissue.type=='Muscle'|label_HT$Tissue.type=='Blood Vessel'|label_HT$Tissue.type=='Heart'),]
row_numbers_label <- rownames(subset_label)

subset_data_HT <- data_HT[row_numbers_label,]
as.factor(subset_label[row_numbers_label,"Tissue.type"])

subset_samples.col <- as.factor(label_HT[row_numbers_label,"Tissue.type"])
levels(subset_samples.col) <- list("#1B9E77" = "Heart", 
                                   "#D95F02" = "Blood Vessel", 
                                   "#7570B3" = "Muscle")

HT.pch <- subset_data_HT
levels(HT.pch) <- list("0"="Heart",
                         "1"="Blood Vessel",
                         "2"="Muscle")
HT.pch <- as.numeric(as.character(HT.pch))
d <- dist(subset_data_HT) 
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
     col = subset_samples.col,
     pch = 1, # type="n",
     cex = 0.7)
