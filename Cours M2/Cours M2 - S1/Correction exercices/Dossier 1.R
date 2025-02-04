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
# Nous allons analyser la répartition des occupations selon les tranches d'âge.

# Créer les tranches d'âge ----
# Nous découpons la variable 'age' en tranches en utilisant la fonction 'cut'.
# Les tranches sont définies pour les âges : 18-30, 30-40, 40-50, 50-64 et 64-97.
hdv2003$age_rec <- cut(hdv2003$age,
                       include.lowest = TRUE,  # Inclure la limite inférieure dans la première tranche
                       right = FALSE,          # La borne supérieure est exclue de la tranche
                       dig.lab = 4,            # Nombre de chiffres à utiliser pour les labels
                       breaks = c(18, 30, 40, 50, 64, 97)  # Bornes des tranches d'âge
)

# Calcul des proportions d'occupation par tranche d'âge ----
# Nous sélectionnons les colonnes d'intérêt, supprimons les doublons, et comptons les individus dans chaque groupe d'âge et d'occupation.
hdv_age_counts <- hdv2003 %>%
  select(id, age_rec, occup) %>%    # Sélection des colonnes 'id', 'age_rec' (tranches d'âge) et 'occup' (occupation)
  distinct() %>%                    # Retrait des doublons pour éviter les répétitions
  group_by(age_rec, occup) %>%       # Regroupement par tranche d'âge et occupation
  summarise(nbr = n()) %>%           # Comptage du nombre d'individus dans chaque groupe
  group_by(age_rec) %>%              # Regroupement par tranche d'âge pour calculer les proportions
  mutate(part = nbr/sum(nbr) * 100)  # Calcul de la proportion (%) d'occupation par tranche d'âge


# Représentation graphique ----
# Nous visualisons la répartition des occupations par tranche d'âge à l'aide d'un graphique en barres.
ggplot(hdv_age_counts) +
  aes(x = age_rec, y = part, fill = occup) +  # Tranche d'âge en x, proportion en y, occupation comme couleur
  geom_col(position = "dodge2") +             # Barres côte à côte pour mieux comparer les occupations
  scale_fill_hue(direction = 1) +             # Palette de couleurs pour différencier les occupations
  theme_minimal() +                           # Utilisation d'un thème minimaliste pour améliorer la lisibilité
  labs(
    title = "Répartition des occupations selon les tranches d'âge",  # Titre du graphique
    x = "Tranche d'âge",                                             # Label de l'axe des x
    y = "Proportion (%)"                                             # Label de l'axe des y
  )

# # Réordonner les occupations par proportion dans chaque tranche d'âge ----
# hdv_age_counts <- hdv_age_counts %>%
#   group_by(age_rec) %>%
#   mutate(occup = fct_reorder(occup, part, .desc = TRUE))  # Reordonne par proportion décroissante


# Utilisation de fct_reorder pour trier les occupations dans l'ordre décroissant des proportions.
ggplot(hdv_age_counts) +
  aes(x = (age_rec), y = part, fill = occup) +  # fct_reorder pour ordonner décroissant
  geom_col(position = "dodge2") + 
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  labs(
    title = "Répartition des occupations selon les tranches d'âge",
    x = "Occupation",
    y = "Proportion (%)"
  )


########################################
########################################
########################################

# Dossier 1 
# Cours Maddi - Cas d'étude 2

########################################
########################################
########################################
########################################


# Nettoyer l'espace de travail ----
# Utilisez cette ligne pour effacer les objets dans votre environnement si nécessaire.
# rm(list=ls())

# Lancement des packages ----
# Nous chargeons les packages nécessaires pour manipuler, analyser et présenter les données.
library(questionr)   # Pour accéder aux jeux de données d'enquêtes
library(openxlsx)    # Pour manipuler les fichiers Excel
library(tidyverse)   # Pour la manipulation des données et visualisation
library(gtsummary)   # Pour générer des résumés statistiques
library(labelled)    # Pour la gestion des labels dans les données

# Chargement des données ----
# Nous utilisons le jeu de données 'hdv2003' disponible dans le package questionr.
data(hdv2003)

# Travail sur les données ----
# Nous allons étudier la répartition des qualifications professionnelles selon le niveau d'éducation.

# Étape 1 : Filtrage des données ----
# Nous conservons uniquement les individus ayant renseigné leur niveau d'éducation ('nivetud') et leur qualification professionnelle ('qualif').
hdv_educ <- hdv2003 %>%
  filter(!is.na(nivetud) & !is.na(qualif))  # Exclusion des individus avec des valeurs manquantes

# Étape 2 : Création des catégories de niveau d'études ----
# Nous regroupons les niveaux d'études en trois grandes catégories : 'Primaire', 'Secondaire', et 'Supérieur'.
hdv_educ$cat_nivetud <- hdv_educ$nivetud %>%
  fct_recode(
    "Primaire" = "N'a jamais fait d'etudes",
    "Primaire" = "A arrete ses etudes, avant la derniere annee d'etudes primaires",
    "Primaire" = "Derniere annee d'etudes primaires",
    "Secondaire" = "1er cycle",
    "Secondaire" = "2eme cycle",
    "Supérieur" = "Enseignement technique ou professionnel court",
    "Supérieur" = "Enseignement technique ou professionnel long",
    "Supérieur" = "Enseignement superieur y compris technique superieur"
  )

# Étape 3 : Comptage des individus par catégo rie de niveau d'études et profession ----
# Nous comptons le nombre d'individus dans chaque combinaison de niveau d'études et profession.
hdv_educ_counts <- hdv_educ %>%
  select(id, cat_nivetud, qualif) %>%  # Sélection des colonnes 'id', 'cat_nivetud' (catégorie de niveau d'études) et 'qualif' (profession)
  distinct() %>%                       # Retrait des doublons
  group_by(cat_nivetud, qualif) %>%     # Regroupement par niveau d'études et profession
  summarise(nbr = n()) %>%              # Comptage des individus dans chaque groupe
  mutate(part = (nbr / sum(nbr)) * 100)  # Calcul de la proportion (%) d'individus dans chaque groupe

# Représentation graphique ----
# Nous visualisons la répartition des professions par niveau d'éducation à l'aide d'un graphique en barres.
ggplot(hdv_educ_counts) +
  aes(x = cat_nivetud, y = part, fill = qualif) +  # Niveau d'éducation en x, proportion en y, profession comme couleur
  geom_col(position = "dodge2") +                 # Barres côte à côte pour comparer les professions
  scale_fill_hue(direction = 1) +                 # Palette de couleurs pour différencier les professions
  theme_minimal() +                               # Utilisation d'un thème minimaliste
  labs(
    title = "Répartition des statuts professionnels par niveau d'études",  # Titre du graphique
    x = "Niveau d'études",                                                 # Label de l'axe des x
    y = "Proportion (%)"                                                   # Label de l'axe des y
  )
