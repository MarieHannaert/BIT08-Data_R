nrow(ChickWeight)
ncol(ChickWeight)
data("ChickWeight")
dim(ChickWeight)
str(ChickWeight)
library(xlsx)

write.xlsx(x = ChickWeight, 
           file = "ChickWeight.xlsx",
           sheetName = "chicken_weight",
           row.names = FALSE)
ChickWeight$Day <- paste0("day ",ChickWeight$Time)
sub("d", "D", ChickWeight)

grepl("day 0", ChickWeight$Day)
class(ChickWeight$Day)
ChickWeight$Day <- as.factor(ChickWeight$Day)
class(ChickWeight$Day)

class(ChickWeight$Chick)
unique(sort(ChickWeight$Chick, decreasing = TRUE))
#Chick2 <- order(sort(ChickWeight$Chick))
Chick2 <- as.numeric(ChickWeight$Chick)

ChickWeight$Chick2 <- Chick2


sort(ChickWeight$Chick2, decreasing=TRUE)
ChickWeight[ChickWeight$Diet=="1", ]

ChickWeight1 <- ChickWeight[which(ChickWeight$Diet=="1")]
ChickWeight1subset <- subset(ChickWeight, ChickWeight$Diet=="1")

as.unique(ChickWeight$Diet)
subsets <- c("1", "2", "3", "4")
listofdfs <- list() #creation of an emptylist
for(subset in subsets){
  dataframe <- paste0("ChickWeight",subset)
  print(paste0("Dataframe = ", dataframe))
  listofdfs[[dataframe]] <- ChickWeight[which(ChickWeight$Diet==subset),]
}
