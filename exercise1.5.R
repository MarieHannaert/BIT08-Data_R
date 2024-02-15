nrow(ChickWeight)
ncol(ChickWeight)
str(ChickWeight)
library(xlsx)
write.xlsx(x = ChickWeight, 
           file = "ChickWeight.xlsx",
           sheetName = "chicken_weight",
           row.names = FALSE)

