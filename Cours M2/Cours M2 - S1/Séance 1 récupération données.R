install.packages(c("questionr"), lib = 'C:/Users/ADMiN/Desktop/')

library(questionr)
data("hdv2003")

# Si vous ne pouvez pas installer le package, utiliser pour l'instant ce petit script pour récupérer les données:
hdv2003 = read.csv2("https://raw.githubusercontent.com/abdelghani-maddi/Cours_R_Sorbonne_U/main/Jeux%20de%20donn%C3%A9es/hdv2003.csv", sep = ",")[,-1]

for (i in c(3:4, 6:7, 9:19)) {
  hdv2003[i] <- data.frame(as.factor(hdv2003[,i]))}

describe(hdv2003)
look_for(hdv2003) 
