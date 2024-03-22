set.seed(1)

# Create dataset of 100 random values generated from a normal distribution
data <- rnorm(1000)
hist(data, breaks = 20)
# Perform Shapiro-Wilk test for normality
shapiro.test(data)
# Shapiro-Wilk normality test
# data:  data
# p-value = 0.9876

# Create dataset of 100 random values generated from a normal distribution
data2 <- rgamma(1000, shape=5)
hist(data2, breaks = 20)
# Perform Shapiro-Wilk test for normality
shapiro.test(data2)
# Shapiro-Wilk normality test
# data:  data2
# p-value = 6.188e-16

# Checking skewness
library(moments)
skewness(data)
skewness(data2)
