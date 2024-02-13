name <- c("Marie", "Tim", "Elias", "Anke", "Gitte", "Paco")
age <- c(24,23,22,21,20,19)
df_patient <- data.frame(name,age)
df_patient
summary(df_patient)
df_patient[1:1,1:2]
df_patient[c(2,4),1:2]
df_patient[,"age"]
class(df_patient$name)
df_patient<- as.factor(df_patient$name)
df_patient$name
df_patient<- as.character(df_patient$name)
df_patient$name
gender <- c("female","male","male", "female","female", "male")
gender <- factor(gender)
df_patient <- data.frame(name, age, gender)
df_patient
class(df_patient$gender)
