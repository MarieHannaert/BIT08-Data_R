pkg <- installed.packages()[, "Package"]
if(!('Stat2Data' %in% pkg)) { install.packages("Stat2Data") }
library(Stat2Data)
if(!('RColorBrewer' %in% pkg)) { install.packages("RColorBrewer") }
library(RColorBrewer)
data("BirdNest")
str(BirdNest)
Nest.freq <- table(BirdNest[,5])
Nest.freq.sort <- sort(Nest.freq, decreasing = TRUE)
bird.col <- brewer.pal(7,"Dark2")
bird.col <- c("#A6761D","#E6AB02","#66A61E","#E7298A","#7570B3","#D95F02","#1B9E77"  )
nestlab.col <- Nest.freq
levels(nestlab.col) <- list("#1B9E77"="cup",
                            "#D95F02" = "cavity",
                            "#7570B3"= "saucer",
                            "#E7298A"="crevice",
                            "#66A61E"="pherical",
                            "#E6AB02"="burrow",
                            "#A6761D"="pendant"
                      )
nestlab.col <- as.character(nestlab.col)
barp <- barplot(sort(Nest.freq), 
        horiz = TRUE, 
        main = "Counts of nesttypes",
        xlab = "Total number of nests",
        ylab = "Type of nest",
        xlim = c(0,60) ,
        las=1,
        col = bird.col,
        names = c("","","","","","",""),
        
      )
abline(v= c(5,15,25,35,45,55), lty=4, col="lightgray")
abline(v= c(10,20,30,40,50,60), lty=2, col="darkgray")
mtext("pendant", side = 2, line = 1, 
      at = 1, las = 1, col = "#A6761D")
mtext("burrow", side = 2, line = 1, 
      at = 2, las = 1, col = "#E6AB02")
mtext("pherical", side = 2, line = 1, 
      at = 3, las = 1, col = "#66A61E")
mtext("crevice", side = 2, line = 1, 
      at = 4, las = 1, col = "#E7298A")
mtext("saucer", side = 2, line = 1, 
      at = 5, las = 1, col = "#7570B3")
mtext("cavity", side = 2, line = 1, 
      at = 6, las = 1, col = "#D95F02")
mtext("cup", side = 2, line = 1, 
      at = 7, las = 1, col = "#1B9E77")

