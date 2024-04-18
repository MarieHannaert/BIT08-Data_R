pkg <- installed.packages()[, "Package"]
if(!('ggplot2' %in% pkg)) { install.packages("ggplot2") }
library(ggplot2)
if(!('gridExtra' %in% pkg)) { install.packages("gridExtra") }
library(gridExtra)
if(!('RColorBrewer' %in% pkg)) { install.packages("RColorBrewer") }
library(RColorBrewer)

setwd("C:/Users/11901250/Documents/SF/BIT08-Data_R/Exam/") #windows
sapiens_data <- read.csv(file="Homo_sapiens_CDS-GC-GC3.csv", header = FALSE)
str(sapiens_data)
head(sapiens_data,5)
sapiens_data$V1 <- as.factor(sapiens_data$V1)

p1 <- ggplot(sapiens_data, aes(x=sapiens_data$V3, color=sapiens_data$V1)) +
  geom_density()+labs(x = "GC3%") + theme(legend.position="none")+
  scale_color_brewer(palette = "Paired")+
  geom_vline(xintercept=c(25, 75), linetype=2, color = "grey")+geom_vline(xintercept=50, linetype=2)
p2 <- ggplot(sapiens_data, aes(x=sapiens_data$V1, y=sapiens_data$V3, fill=sapiens_data$V1)) + 
  geom_violin(trim=FALSE)+ labs(x = "", y = "%")+ guides(fill=guide_legend(title="Codon content"))
gridExtra::grid.arrange(p1,p2, ncol=2)


#How many transcripts (rows) have a GC3 percentage = 100.
length(which(sapiens_data$V1=="Human  GC3"&sapiens_data$V3==100))

#How many transcripts (rows) have a GC3 percentage = 0
length(which(sapiens_data$V1=="Human  GC3"&sapiens_data$V3==0))

#Filter data excluding GC3 percentages equal to 100 and to 0, keeping only "Human  GC3" rows and save in a new dataframe 
subset_df <- subset(sapiens_data, V1 == "Human  GC3" | V3!=100 | V3!=0)
#Filter 5 transcripts with lowest GC3 (0% and 100% not included). Filter 5 transcripts with highest GC3 (0% and 100% not included).
tail1 <- tail(subset_df,5)
head1 <- head(subset_df,5)
#Merge rows together in new dataframe (exemplified in the view below).
merge(tail1,head1)
