setwd("C:/Users/11901250/Documents/SF/BIT08-Data_R") #windows
read.csv(file="algae.csv", header = TRUE)
algae_data <- read.csv(file="algae.csv", header = TRUE)
str(algae_data)
summary(algae_data)
algae_data$season <- as.factor(algae_data$season)
algae_data$size <- as.factor(algae_data$size)
algae_data$speed <- as.factor(algae_data$speed)

season.freq <- table(algae_data$season)
size.freq <- table(algae_data$size)
speed.freq <- table(algae_data$speed)

par(mfrow = c(1,3))
barplot(season.freq)
barplot(size.freq)
barplot(speed.freq)

par(mfrow = c(1,3))
cat_algaes <- colnames(algae_data[,2:4])
for(cat_algae in cat_algaes) {
  cat_algae.freq <- table(algae_data[,cat_algae])
  barplot(cat_algae.freq,main = paste0("Histogram of ",cat_algae),
          xlab = cat_algae, 
           )
}



par(mfrow = c(2,))
d <- density(algae_data[,"mxPH"])
hist(algae_data$mxPH)
lines(d, lty = 3, lwd = 3, col = "indianred")
hist(algae_data$mnO2)
hist (algae_data$Cl)
hist(algae_data$NO3)
hist(algae_data$NH4)
hist(algae_data$oPO4)
hist(algae_data$PO4)
hist(algae_data$Chla)

par(mfrow = c(2,4))
algaes <- colnames(algae_data[,5:12])
for(algae in algaes) {
  hist(algae_data[,algae], 
       main = paste0("Histogram of ",algae),
       xlab = algae, 
       ylim = c(0,40))
}


library(ggplot2)
#install.packages("ggESDA")
library(ggESDA)
library(RColorBrewer)

par(mfrow = c(1,2))
boxplot(mxPH ~ season, data = algae_data, col= c("#FB8072","#8DD3C7","#FFED6F","#80B1D3"))
boxplot(mnO2 ~ season, data = algae_data, col= c("#FB8072","#8DD3C7","#FFED6F","#80B1D3"))

require(gridExtra)
plot1<-ggplot(algae_data, aes(x=season, y=mxPH, color=season)) +
  geom_boxplot()+scale_color_manual(values=c("#FB8072","#8DD3C7","#FFED6F","#80B1D3"))

plot2<-ggplot(algae_data, aes(x=season, y=mnO2, color=season)) +
  geom_boxplot()+scale_color_manual(values=c("#FB8072","#8DD3C7","#FFED6F","#80B1D3"))
grid.arrange(plot1, plot2, ncol=2)
# Load package qicharts2
library(qicharts2)
# Run chart of mval
qic(algae_data$mxPH , show.grid = TRUE, 
    title = "mxPH")
qic(algae_data$mnO2 , show.grid = TRUE, 
    title = "mnO2")



pairs(algae_data[, c("a1", "a2", "a3", "a4", "a5", "a6","a7","speed", "season")])


# Three first columns of the data frame
types_algae <- algae_data[, 13:19]

# Plot the three columns at once
matplot(types_algae, type = "l", lty = 1,
        ylab = "data")# Y-axis limits

m_a1 <- mean(algae_data$a1)
m_a2 <- mean(algae_data$a2)
m_a3 <- mean(algae_data$a3)
m_a4 <- mean(algae_data$a4)
m_a5 <- mean(algae_data$a5)
m_a6 <- mean(algae_data$a6)
m_a7 <- mean(algae_data$a7)

m_atypes <- mean(algae_data[, 13:19])


par(mfrow = c(2,4))
geom_boxplot(types_algae)
#http://www.sthda.com/english/wiki/ggplot2-box-plot-quick-start-guide-r-software-and-data-visualization
par(mfrow = c(1,1))
boxplot2(types_algae)

library(reshape2)
library(ggplot2)

newDF_boxplot <- algae_data[, c(2,4,13:19)]


dat.m = melt(newDF_boxplot, id.var=c("speed","season"))
dat.m$season = factor(dat.m$season, levels=c("winter", "spring","summer","autumn"))

# If you want the two levels of event plotted side by side
ggplot(dat.m, aes(season, speed, colour=speed)) +
  facet_grid(. ~ variable) +
  geom_boxplot(width=0.7)

library(plotly)

# Create 3D scatterplot with plotly
plot_ly(data = algae_data, x = ~season, y = ~a1, z = ~speed, type = "scatter3d", mode = "markers") %>%
  layout(scene = list(xaxis = list(title = "Season"),
                      yaxis = list(title = "a1"),
                      zaxis = list(title = "Speed")),
         title = "3D Scatterplot of a1 by Season and Speed")











library(plotly)
library(RColorBrewer)

# Define the number of unique seasons
num_seasons <- length(unique(algae_data$season))

# Get colors from the "Accent" palette
colors <- brewer.pal(num_seasons, "Set1")

# Create 3D scatterplot with plotly
plot_ly(data = algae_data, x = ~season, y = ~a1, z = ~speed, 
        color = ~season, 
        colors = colors,
        type = "scatter3d", mode = "markers") %>%
  layout(scene = list(xaxis = list(title = "Season"),
                      yaxis = list(title = "a1"),
                      zaxis = list(title = "Speed")),
         title = "3D Scatterplot of a1 by Season and Speed")




library(plotly)
library(RColorBrewer)

# Define the number of unique seasons
num_seasons <- length(unique(algae_data$season))

# Get colors from the "Set1" palette
colors <- brewer.pal(num_seasons, "Set1")

# Create 3D scatterplot with plotly
plot_ly(data = algae_data, x = ~mxPH, y = ~a1, z = ~mnO2, 
        color = ~season, 
        colors = colors,
        type = "scatter3d", mode = "markers") %>%
  layout(scene = list(xaxis = list(title = "mxPH"),
                      yaxis = list(title = "a1"),
                      zaxis = list(title = "mnO2")),
         title = "3D Scatterplot of a1 by mxPH and mnO2 colored by Season")



library(ggplot2)
library(gridExtra)

# Define a function to create scatterplots for each group
create_scatterplot <- function(group) {
  ggplot(algae_data, aes(x = season, y = .data[[group]], color = season)) +
    geom_point() +
    labs(title = paste("Scatterplot of", group, "by Season"),
         x = "Season", y = group) +
    theme_minimal()
}

# Create scatterplots for each group a1-a7
plots <- lapply(paste0("a", 1:7), create_scatterplot)

# Arrange all scatterplots on the same page
grid.arrange(grobs = plots, ncol = 2)

library(ggplot2)
library(gridExtra)

library(ggplot2)
library(gridExtra)
library(RColorBrewer)

# Define the number of unique groups
num_groups <- 7

# Define custom color palette with the "Accent" palette from RColorBrewer
group_colors <- brewer.pal(num_groups, "Accent")

# Define a function to create scatterplots for each group
create_scatterplot <- function(group) {
  ggplot(algae_data, aes(x = mxPH, y = .data[[group]], color = group)) +
    geom_point() +
    scale_color_manual(values = group_colors) +  # Assign unique color for each group
    labs(title = paste("Scatterplot of", group, "by mxPH"),
         x = "mxPH", y = group) +
    theme_minimal()
}

# Create scatterplots for each group a1-a7
plots <- lapply(paste0("a", 1:7), create_scatterplot)

# Arrange all scatterplots on the same page
grid.arrange(grobs = plots, ncol = 2)

library(seaborn)

sns.kdeplot(x=df.x, y=df.y, cmap="Blues", shade=True)

df.plot(kind='hexbin', x='x', y='y', gridsize = 13)


algaes_all_spec <- algae_data[,13:19]
for (a in algaes_all_spec){
  hc <- algae_data %>% 
    hchart('scatter', hcaes(x = a, y = mnO2, group = season)) %>%
    hc_colors(c("#FB8072","#8DD3C7","#FFED6F","#80B1D3"))
  hc
}
par(mfrow = c(2,4))
algaes_all_spec <- algae_data[,13:19]
for (a in algaes_all_spec){
  hc <- algae_data %>% 
    hchart('scatter', hcaes(x = a, y = mnO2, group = season)) %>%
    hc_colors(c("#FB8072","#8DD3C7","#FFED6F","#80B1D3"))
  hc
}
