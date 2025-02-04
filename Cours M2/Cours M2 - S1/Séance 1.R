### Dossier 1 : Importation des données, lecture et exploration avec R

## Objectifs pédagogiques :
# Apprendre à importer et explorer un jeu de données avec R.
# Découvrir les différentes structures de données dans R (data frames, matrices, listes, etc.).
# Comprendre comment effectuer des premières analyses descriptives (statistiques simples, tableaux de fréquences, résumés).
# S'initier à l'inspection des variables et des structures des tables.
# Jeu de données : hdv2003 (package questionr)
# Contexte : Il s'agit d'un jeu de données issu d'une enquête sur les conditions de vie et les pratiques des ménages en 2003 en France. Il contient des informations sur l'âge, le sexe, le statut matrimonial, l'activité professionnelle, la satisfaction au travail, et bien d'autres variables pertinentes.
# Plan du cours :

#   1. Introduction à l’environement de travail RStudio (10 min) ----
# Présentation de R et RStudio : fenêtres principales (script, console, environnement, plot).
# Explication des avantages de R pour l’analyse de données d’enquêtes.
# Préparer l’environnement de travail : installation des packages nécessaires pour la séance.
# 

# install.packages("questionr")
library(questionr)
library(questionr)
library(openxlsx) # pour lire et enregistrer des fichier excel
library(tidyverse)
library(gtsummary)
library(labelled)

# 2. Importation des données (20 min) ----
# Concepts abordés :
#   
#   Importer des fichiers CSV, Excel, et utiliser des jeux de données intégrés à R.
# Découverte du package questionr et du jeu de données hdv2003.
# Exemple d'importation :

data(hdv2003)
write.xlsx(hdv2003, "hdv2003.xlsx")


# Les données sont intégrées dans le package et accessibles directement via data().
# Présentation des différentes fonctions pour importer d’autres formats de données :
# Fichiers CSV : read.csv()
# Fichiers Excel : readxl::read_excel()
# 
# Travail pratique :
# Importation de leurs propres données (fichier CSV) ou d’un fichier fourni.


# 3. Exploration des données : Structure et premières observations (30 min) ----
# Concepts abordés :
#   
# Les structures de données dans R : data frames, listes, matrices.
# Vérifier la structure des données et les premières observations.
# Fonctions clés :
#   


str(hdv2003)       # Structure du jeu de données
dim(hdv2003)       # Dimensions (nombre de lignes et colonnes)
colnames(hdv2003)  # Noms des colonnes (variables)
head(hdv2003)      # Aperçu des premières lignes
summary(hdv2003)   # Résumé statistique des variables

# Exercice pratique :
# Vérifier la structure du jeu de données et d'identifier :
# Le nombre total d’observations (lignes).
# Le nombre de variables (colonnes).
# Les types de variables (numériques, catégorielles, etc.).

# Inspection générale des données ----
# summary
summary(hdv2003$age)
summary(hdv2003$qualif)
summary(hdv2003)

# str
str(hdv2003)

# glimpse
library(dplyr)
glimpse(hdv2003)


# look_for
library(labelled)
look_for(hdv2003)

look_for(hdv2003, "trav")

# describe
describe(hdv2003)
describe(hdv2003, "age", "trav")
describe(hdv2003$sexe)

# codebook
codebook::label_browser_static(hdv2003)


# Étiquettes de variables ----
var_label(hdv2003$occup) = "Occupation actuelle"
var_label(hdv2003$age) = "Âge de la personne"

# 4. Exploration des variables : Aperçu et nettoyage (30 min) -----
# 
# Exploration de chaque variable, focus sur les variables catégorielles et numériques.
# Détection et gestion des valeurs manquantes (NA).
# Exemple pratique :
# Explorer les variables age (âge) et sexe (genre).

table(hdv2003$sexe)      # Fréquence des valeurs dans la variable 'sexe'
hist(hdv2003$age)        # Histogramme pour visualiser la distribution des âges

# Gérer les valeurs manquantes :

sum(is.na(hdv2003$age))  # Nombre de valeurs manquantes pour 'age'
hdv2003_clean <- na.omit(hdv2003)  # Supprimer les lignes avec des NA

# Exercice pratique :
# Identifier les variables qui ont des valeurs manquantes et de proposer une stratégie de nettoyage (suppression, imputation).

# 5. Analyses descriptives : Statistiques simples et résumés (30 min) ----

# Statistiques descriptives (moyenne, médiane, écart-type, etc.) pour les variables numériques.
# Fréquences et proportions pour les variables catégorielles.
# Visualisation des premières relations entre variables.

# Exemple pratique :
# Calculer des statistiques descriptives pour la variable age :

mean(hdv2003$age, na.rm = TRUE)    # Moyenne de l'âge
median(hdv2003$age, na.rm = TRUE)  # Médiane
sd(hdv2003$age, na.rm = TRUE)      # Ecart-type

# Tableau de fréquences pour sexe et profession :
table(hdv2003$sexe) 
table(hdv2003$qualif)
table(hdv2003$sexe, hdv2003$qualif)


# Visualisation : Histogrammes et diagrammes en barres.
barplot(table(hdv2003$sexe))

# Exercice pratique :
# Demander aux étudiants de choisir deux variables, de calculer leurs statistiques descriptives et de visualiser les résultats.
# Comparer les distributions des âges selon le sexe avec des boxplots :

boxplot(age ~ sexe, data = hdv2003)

# 6. Conclusion et récapitulatif (10 min) ----

# Résumer les étapes clés abordées lors de cette première séance :
# 
# Importation des données.
# Exploration de la structure du jeu de données.
# Première exploration des variables (qualitatives et quantitatives).
# Analyses descriptives de base et visualisation.


# Travail à la maison : -----
# # Jeu de données supplémentaire à analyser individuellement : 
# # Jeu de données : starwars (package dplyr)
# # Description : Caractéristiques des personnages de Star Wars (taille, poids, espèce, affiliation).
# 
# Tâches : Importer le fichier, explorer la structure, et faire des premières analyses descriptives (calcul de moyenne, médiane, histogramme).
#          Identifier des variables manquantes et proposer une méthode pour les gérer.
# 
# Matériel de soutien :
# Fichier RMarkdown contenant tout le code vu en séance.



# 