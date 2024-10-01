########################################
# Dossier 1 
# Cours Maddi
########################################

# Nettoyer l'espace de travail ----
# Utilisez cette ligne pour effacer les objets dans votre environnement si nécessaire.
# rm(list=ls())

# Lancement des packages ----
# Nous chargeons les packages nécessaires pour manipuler, analyser et présenter les données.
library(questionr)   # Pour l'accès aux jeux de données d'enquêtes
library(openxlsx)    # Pour la gestion des fichiers Excel
library(tidyverse)   # Pour la manipulation de données et les visualisations
library(gtsummary)   # Pour résumer les résultats statistiques
library(labelled)    # Pour la gestion des labels dans les données

# Chargement des données ----
# Nous utilisons le jeu de données 'hdv2003' provenant du package questionr.
data(hdv2003)

# Travail sur les données ----
# Nous allons filtrer et analyser les jeunes de 25 ans ou moins, et étudier la répartition par sexe et occupation.

## Étape 1 : Filtrer les jeunes de 25 ans ou moins
# Cette étape consiste à isoler les individus âgés de 25 ans ou moins à partir des données hdv2003.
hdv_jeunes <- hdv2003 %>%
  filter(age <= 25)

## Étape 2 : Sélectionner les colonnes 'id', 'sexe' et 'occup', puis enlever les doublons
# Nous ne conservons que les variables pertinentes : identifiant (id), sexe et occupation.
# Nous supprimons également les doublons pour éviter les répétitions inutiles.
hdv_jeunes <- hdv_jeunes %>%
  select(id, sexe, occup) %>%
  distinct()

## Étape 3 : Compter le nombre d'observations par sexe et occupation
# Nous allons regrouper les données par sexe et occupation, puis compter le nombre d'individus dans chaque catégorie.
hdv_jeunes_counts <- hdv_jeunes %>%
  group_by(sexe, occup) %>%
  summarise(nbr = n())

## Étape 4 : Calculer la proportion de chaque groupe
# Ici, nous calculons la proportion de chaque groupe par rapport au total des jeunes de 25 ans ou moins.
total_nbr <- sum(hdv_jeunes_counts$nbr)

hdv_jeunes_counts <- hdv_jeunes_counts %>%
  mutate(part = (nbr / total_nbr) * 100)

# Résultat : Nous avons maintenant la répartition des jeunes par sexe et occupation, ainsi que leur proportion.

# Ou bien de façon plus compacte :
# Cette approche compacte combine toutes les étapes en une seule opération fluide.
hdv_jeunes_counts <- hdv2003 %>%
  filter(age <= 25) %>%
  select(id, sexe, occup) %>%
  distinct() %>%
  group_by(sexe, occup) %>%
  summarise(nbr = n()) %>%
  ungroup() %>%
  mutate(part = nbr / sum(nbr) * 100)  # Proportion des groupes calculée sur l'ensemble

# Représentation graphique ----
# Nous visualisons maintenant les résultats avec un graphique en barres.
ggplot(hdv_jeunes_counts) +
  aes(x = occup, y = part, fill = sexe) +
  geom_col(position = "dodge2") +  # Barres côte à côte pour comparer les sexes
  scale_fill_hue(direction = 1) +  # Palette de couleurs pour différencier les groupes
  theme_minimal()                  # Thème minimaliste pour une meilleure lisibilité
