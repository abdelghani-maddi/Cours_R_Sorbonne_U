# Séance 1 : Introduction à la régression logistique
# Objectif : Apprendre à réaliser une analyse de régression logistique avec R
# Analyse : La pratique du sport en fonction de différentes variables socio-démographiques.

## Chargement des données et des packages ----
# Nous chargeons ici les bibliothèques nécessaires à l'analyse.
# 'tidyverse' pour la manipulation des données,
# 'labelled' pour gérer les étiquettes des variables,
# 'questionr' pour des statistiques descriptives,
# 'gtsummary' pour des tableaux récapitulatifs,
# 'GGally' pour des visualisations bivariées, 
# 'effects' et 'ggeffects' pour les effets marginaux et les graphiques associés.

library(tidyverse)
library(labelled)
library(questionr)
library(gtsummary)
library(GGally)
library(effects)
library(ggeffects)

# Configurations des thèmes pour les tableaux et graphiques
theme_gtsummary_language("fr", decimal.mark = ",", big.mark = " ")
theme_gtsummary_mean_sd()

# Chargement du jeu de données 'hdv2003' qui contient des informations sur la pratique du sport
data(hdv2003)

## Étiquettes de variables ----
# Nous ajoutons des étiquettes aux variables pour mieux comprendre leur signification.
# Ces étiquettes seront affichées dans les résultats pour rendre les analyses plus lisibles.

hdv2003 <- hdv2003 %>%
  set_variable_labels(
    sport = "Pratique du sport ?",   # Variable binaire : Oui ou Non à la pratique du sport
    age = "Âge",                     # Âge des répondants
    sexe = "Sexe",                   # Sexe des répondants
    nivetud = "Niveau d'études",     # Niveau d'éducation des répondants
    relig = "Rapport à la religion", # Relation à la religion
    heures.tv = "Heures quotidiennes de TV"  # Nombre d'heures passées devant la télévision par jour
  )

## Recodages -------

# Nous recodons les tranches d'âge pour les rendre plus compréhensibles.
# Utilisation de la fonction 'cut()' pour créer des groupes d'âge.
# Ensuite, nous réétiquetons les catégories pour des noms plus simples.

hdv2003$groupe_age <- hdv2003$age %>% 
  cut(
    include.lowest = TRUE,    # Inclure la valeur de la borne inférieure
    right = FALSE,            # Exclure la borne supérieure
    dig.lab = 4,              # Nombre de décimales dans les étiquettes
    breaks = c(18, 25, 45, 60, 97) # Définir les bornes des groupes d'âge
  ) %>% 
  fct_recode(
    "18-24" = "[18,25)",
    "25-44" = "[25,45)",
    "45-59" = "[45,60)",
    "60 et +" = "[60,97]"
  )
# Étiqueter la variable 'groupe_age' pour la rendre plus lisible
var_label(hdv2003$groupe_age) <- "Groupe d'âges"

# Recodage de la variable 'sexe' pour assurer que "Femme" est la référence dans le modèle.
hdv2003$sexe <- hdv2003$sexe %>%
  fct_relevel("Femme")

# Recodage du niveau d'études pour rendre les catégories plus intuitives.
hdv2003$etudes <- hdv2003$nivetud %>% 
  fct_recode(
    "Primaire" = "N'a jamais fait d'etudes",
    "Primaire" = "A arrete ses etudes, avant la derniere annee d'etudes primaires",
    "Primaire" = "Derniere annee d'etudes primaires",
    "Secondaire" = "1er cycle",
    "Secondaire" = "2eme cycle",
    "Technique/Professionnel" = "Enseignement technique ou professionnel court",
    "Technique/Professionnel" = "Enseignement technique ou professionnel long",
    "Supérieur" = "Enseignement superieur y compris technique superieur"
  ) %>%
  fct_explicit_na("Manquant") # Traitement des valeurs manquantes
var_label(hdv2003$etudes) <- "Niveau d'études"

## Analyse univariée --------
# Nous commençons par une analyse univariée des variables.
# Nous allons résumer les variables catégorielles avec des pourcentages et le nombre de répondants.

hdv2003 %>%
  select(sport, groupe_age, sexe, etudes, relig, heures.tv) %>%
  tbl_summary(
    statistic = all_categorical() ~ "{p}% [{n}]",  # Afficher le pourcentage et le nombre de cas
    digits = all_categorical() ~ c(1, 0)            # Arrondir les pourcentages à 1 décimale
  )

## Analyse bivariée --------
# Ensuite, nous réalisons une analyse bivariée en fonction de la variable 'sport'.
# Nous examinons comment les autres variables sont distribuées selon la pratique du sport (Oui/Non).

hdv2003 %>%
  select(sport, groupe_age, sexe, etudes, relig, heures.tv) %>%
  tbl_summary(
    by = "sport",               # Analyser par groupe 'sport'
    percent = "row",            # Afficher les pourcentages par ligne (pour chaque groupe)
    statistic = all_categorical() ~ "{p}% [{n}]", # Pour chaque variable catégorielle
    digits = all_categorical() ~ c(1, 0)           # Arrondir les pourcentages à 1 décimale
  ) %>%
  add_overall(last = TRUE) %>%   # Ajouter une ligne pour l'ensemble des données
  add_p()                       # Ajouter le test statistique pour chaque variable

# Visualisation bivariée : relations entre 'sport' et les autres variables.
hdv2003 %>%
  ggbivariate(
    outcome = "sport",              # Résultat : pratique du sport
    explanatory = c("groupe_age", "sexe", "etudes", "relig", "heures.tv")  # Variables explicatives
  )

## Calcul de la régression logistique -----
# Nous passons à l'analyse de régression logistique.
# La régression logistique permet d'analyser la relation entre une variable dépendante binaire ('sport') et plusieurs variables indépendantes.

mod <- glm(
  sport ~ groupe_age + sexe + etudes + relig + heures.tv,  # Modèle avec les variables explicatives
  family = binomial(),         # La famille 'binomial' est utilisée pour les modèles logistiques
  data = hdv2003
)

# Résumé du modèle : estimation des coefficients, des intervalles de confiance, et des p-valeurs.
mod %>% 
  tbl_regression(
    intercept = TRUE,                  # Inclure l'ordonnée à l'origine (intercept)
    exponentiate = TRUE,               # Exponentier les coefficients pour obtenir les OR (Odds Ratios)
    add_estimate_to_reference_row = TRUE  # Ajouter une ligne de référence avec les estimations
  )

# Graphique des coefficients : Visualisation des odds ratios avec leur intervalle de confiance.
ggcoef_model(mod, exponentiate = TRUE)

## Effets marginaux ------
# L'effet marginal permet d'examiner l'impact de chaque variable sur la probabilité de l'événement d'intérêt.

# Calcul des effets marginaux avec la fonction 'allEffects()'.
mod %>% allEffects() %>% plot()  # Visualisation des effets marginaux pour chaque variable

# Alternative avec 'ggeffects' pour obtenir une visualisation différente des effets marginaux.
ggeffect(mod) %>% 
  plot() %>%
  cowplot::plot_grid(plotlist = .)  # Utilisation de 'cowplot' pour organiser les graphiques
