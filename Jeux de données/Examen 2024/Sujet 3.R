# Examen 2024
# Cours de R et Rstudio
# M Maddi

# Sujet 3
# Description du jeu de données :

# Cet ensemble de données donne les résultats du 1er tour de l'élection présidentielle en
# France en 2002 pour chaque département (94 départements en lignes) et tous les candidats.
# 
# Le tableau contient les colonnes suivantes :
#   le nombre d'inscrits ; le nombre d'abstentions (abstentions) ; 
#   le nombre d'électeurs (votants); le nombre de votes exprimés (exprimes) 
#   et, le nombre de votes pour chaque candidat.

# Package
library(ade4)

# Données
data(presid2002)
data <- presid2002$tour1

# Question : analyser et decrire les données avec les outils vus en cours (statistiques descriptives, analyse factorielle et classification)
