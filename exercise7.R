#exersice 7.1 
#one nominal variable 
#exact, chi, G
#exact more accurate for small sampple sizes
#140 homotypic, 106 heterotypic 
#H0 mating randomly (homotypic=heterotypic)
#H1 mating not randomly 

binom.test(c(140,106),p = 0.05)

#7.2
attacked <- c(26,10)
notattacked <- c(74,90)
children_measles <- data.frame(attacked, notattacked, 
                       row.names = c("unvaccinated","vaccinated"))

chisq.test(children_measles, correct = FALSE)
#better option to use the fisher test

#7.3
mussel_array <- array(c(56,40,69,77, # Tillamook
                        61,57,257,301, # Yaquina
                        73, 71,65,79, # Alsea
                        71, 55,48,48 #Umpqua
                        ),
                      dim = c(2, 2, 4), # 2x2 for each study
                      dimnames = list(
                        Allel = c("94", "non-94"),
                        Revasc = c("Marine", "Estuarine"),
                        Study = c("Tillamook", "Yaquina","Alsea","Umpqua")))
dim(mussel_array) # 2 2 5
# View and verify the data in the array
mussel_array
# Perform the test
mantelhaen.test(mussel_array)

#7.4
setwd("C:/Users/11901250/Documents/SF/BIT08-Data_R/Rdatasets-v2.1/") #windows
read.csv(file="corn-dataset.csv.csv", header = TRUE)
corn <- read.csv(file="corn-dataset.csv", header = TRUE)
str(corn)

boxplot(Yield~Manure*Fert, data = corn)
corn.model = lm(Yield ~ Manure*Fert, 
                     data = corn)
summary(corn.model)
# --> p-value: 0.9128 > 0.05
# --> not significant --> cannot reject H0
# Analysis of variance table for model
anova(corn.model)
#er is een significant verschil tussen de verschillende boxplots
#maar als je kijkt naar beide hoog zie je geen significant verschil

#-->>> the commands with interpretation needs also to be in the file. 
##
#Reject the H0 
#For manure yes 
#For fert yes 
#For Manure*Fert NO 
#There is no effect of using both of them, there is no interaction between the two factors 

#Why two-way anova: observations are not paired 
