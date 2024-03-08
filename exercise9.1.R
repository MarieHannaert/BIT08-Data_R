install.packages("gap")
library(gap)
data("hla")
str(hla)
hla[1:5,]

library(ggplot2)
library(gridExtra)
grid.arrange 
bxp1 <- ggplot(hla, aes(x = id, y = DQR.a1)) +
  geom_boxplot()
bxp1
bxp2 <- ggplot(hla, aes(x = id, y = DQR.a2)) +
  geom_boxplot()
bxp2

