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
