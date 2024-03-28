# Examen 2024
# Cours de R et Rstudio
# M Maddi

# Sujet 9 : 
# Customer Satisfaction Survey Dataset : Cet ensemble de données contient des réponses 
# à des enquêtes de satisfaction client, telles que la satisfaction à l'égard du produit, 
# du service client, etc.
# Description des données : https://search.r-project.org/CRAN/refmans/bayesm/html/customerSat.html 

# Package
install.packages("bayesm")
library(bayesm)

# Données
data(customerSat)

# Question :
# L'objectif est de prédire la satisfaction globale du client / 
# ou caractériser les profils des cliens selon les questions.

# S'appuyer sur les cours (statistiques desc, regression, analyse des correspondances, etc.)