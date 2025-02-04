#' ---
#' title: "Analyse de la pratique du sport (hdv2003)"
#' ---

#' # Mise en place

## Chargement des données et des packages ----
# On charge les bibliothèques nécessaires pour l'analyse. Ces packages permettent de manipuler les données, d'effectuer des analyses statistiques et de produire des graphiques.

library(tidyverse)  # Manipulation des données (dplyr, ggplot2, tidyr...)
library(labelled)   # Pour gérer les étiquettes des variables
library(questionr)  # Pour des fonctions pratiques en analyse d'enquête
library(gtsummary)  # Pour résumer les résultats de manière élégante
library(GGally)     # Pour des graphiques de corrélations et autres visualisations
library(effects)    # Pour calculer et visualiser les effets marginaux
library(ggeffects)  # Pour visualiser les effets marginaux des modèles

# Paramètres de mise en forme des tables dans gtsummary
theme_gtsummary_language("fr", decimal.mark = ",", big.mark = " ")
theme_gtsummary_mean_sd()  # Affichage des moyennes et écarts-types

# On charge le jeu de données 'hdv2003' qui contient des informations sur les pratiques sportives
data(hdv2003)

## Étiquettes de variables ------

# Ici, on attribue des étiquettes plus compréhensibles aux variables.
# Par exemple, la variable 'sport' devient "Pratique du sport ?" dans les résultats.
hdv2003 <- hdv2003 %>%
  set_variable_labels(
    sport = "Pratique du sport ?",
    age = "Âge",
    sexe = "Sexe",
    nivetud = "Niveau d'études",
    relig = "Rapport à la religion",
    heures.tv = "Heures quotidiennes de TV"
  )

## Recodages -------

# On crée une nouvelle variable 'groupe_age' pour classifier les individus selon leur tranche d'âge.
# Cela permet de simplifier l'analyse en regroupant les âges en catégories.
hdv2003$groupe_age <- hdv2003$age %>% 
  cut(
    include.lowest = TRUE,  # Inclut la borne inférieure
    right = FALSE,          # Intervalle gauche inclus, droit exclu
    dig.lab = 4,            # Format d'affichage des étiquettes
    breaks = c(18, 25, 45, 60, 97)  # Les intervalles d'âge
  ) %>% 
  fct_recode(
    "18-24" = "[18,25)",
    "25-44" = "[25,45)",
    "45-59" = "[45,60)",
    "60 et +" = "[60,97]"
  )
var_label(hdv2003$groupe_age) <- "Groupe d'âges"  # On attribue une étiquette à cette variable

# Recodage de la variable 'sexe' pour que "Femme" soit la catégorie de référence
hdv2003$sexe <- hdv2003$sexe %>%
  fct_relevel("Femme")

# Recodage de la variable 'etudes' pour simplifier les niveaux de l'éducation
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
  fct_explicit_na("Manquant")  # Pour gérer les valeurs manquantes
var_label(hdv2003$etudes) <- "Niveau d'études"

#' # Analyse uni et bivariée

## Analyse univariée --------

# On effectue une analyse univariée des variables catégorielles
# tbl_summary résume les variables en termes de fréquences et pourcentages
hdv2003 %>%
  select(sport, groupe_age, sexe, etudes, relig, heures.tv) %>%
  tbl_summary(
    statistic = all_categorical() ~ "{p}% [{n}]",  # Affiche le pourcentage et le nombre
    digits = all_categorical() ~ c(1, 0)  # Contrôle le nombre de décimales affichées
  )

## Analyse bivariée --------

# Ici, on analyse la relation entre les variables catégorielles et la variable cible 'sport'.
# Cela permet de vérifier comment chaque variable se distribue en fonction de la pratique du sport.
hdv2003 %>%
  select(sport, groupe_age, sexe, etudes, relig, heures.tv) %>%
  tbl_summary(
    by = "sport",  # Sépare les résultats par la variable cible
    percent = "row",  # Montre les pourcentages par ligne
    statistic = all_categorical() ~ "{p}% [{n}]",
    digits = all_categorical() ~ c(1, 0)
  ) %>%
  add_overall(last = TRUE) %>%  # Ajoute une ligne pour le total général
  add_p()  # Ajoute une valeur p pour tester l'hypothèse d'indépendance

#' # Régression logistique

## Calcul de la régression logistique -----
# On effectue une régression logistique pour prédire la pratique du sport en fonction des variables explicatives.
# La fonction glm() permet de spécifier un modèle linéaire généralisé, ici avec une distribution binomiale (pour la variable binaire 'sport').
mod <- glm(
  sport ~ groupe_age + sexe + etudes + relig + heures.tv,  # Variables explicatives
  family = binomial(),  # Spécification d'un modèle de régression logistique
  data = hdv2003
)

# Résumé du modèle avec gtsummary : on obtient des estimations des coefficients (log-odds), leur intervalle de confiance et la p-value.
mod %>% 
  tbl_regression(
    intercept = TRUE,  # Affiche l'ordonnée à l'origine
    exponentiate = TRUE,  # Exponentiation des coefficients pour obtenir les cotes (odds ratios)
    add_estimate_to_reference_row = TRUE  # Ajouter une ligne de référence
  )

# Visualisation des coefficients avec ggcoef_model
ggcoef_model(mod, exponentiate = TRUE)

## Effets marginaux ------
# Les effets marginaux représentent l'impact moyen d'une variation dans une variable sur la probabilité de l'événement (ici, pratiquer un sport).
mod %>% allEffects() %>% plot()

# Visualisation des effets marginaux avec ggeffects
ggeffect(mod) %>% 
  plot() %>%
  cowplot::plot_grid(plotlist = .)

## Effet global de chaque variable ------
# Analyse de l'effet global de chaque variable pour comprendre leur influence sur la probabilité de pratiquer un sport.
mod %>% 
  tbl_regression(
    intercept = TRUE,
    exponentiate = TRUE,
    add_estimate_to_reference_row = TRUE
  ) %>%
  add_global_p(keep = TRUE)  # Ajoute un test global pour chaque variable

## Sélection pas à pas descendante ------
# Sélection des variables par la méthode pas à pas descendante (stepwise) pour simplifier le modèle.
mod2 <- step(mod)

# Visualisation des résultats du modèle simplifié
mod2 %>% ggcoef_model()

# Comparaison entre le modèle complet et le modèle simplifié
ggcoef_compare(
  list("modèle complet" = mod, "modèle simplifié" = mod2),
  exponentiate = TRUE,
  type = "f"
)

# Résumé des deux modèles (complet et simplifié) dans une table pour comparaison
t1 <- mod %>% 
  tbl_regression(exponentiate = TRUE, add_estimate_to_reference_row = TRUE) %>% 
  add_global_p()
t2 <- mod2 %>% 
  tbl_regression(exponentiate = TRUE, add_estimate_to_reference_row = TRUE) %>% 
  add_global_p()

tbl_merge(
  tbls = list(t1, t2),
  tab_spanner = c("**Modèle complet**", "**Modèle simplifié**")
)

## Interaction sexe et âge -----
# On teste s'il y a une interaction entre le sexe et le groupe d'âge, pour savoir si l'effet de l'âge varie en fonction du sexe.
mod3 <- glm(
  sport ~ sexe * groupe_age + etudes + heures.tv, 
  family = binomial, data = hdv2003
)

# Résumé du modèle avec l'interaction sexe * âge
mod3 %>% tbl_regression() %>% add_global_p()

# Visualisation des coefficients du modèle avec interaction
ggcoef_model(mod3)

# Visualisation des effets marginaux pour l'interaction sexe * âge
plot(allEffects(mod3))

# Effet des variables sexe et groupe_age avec ggeffects
mod3 %>%
  ggeffect(c("groupe_age", "sexe")) %>%
  plot()

# Régression avec interaction dans une autre formulation
mod4 <- glm(
  sport ~ sexe:groupe_age + etudes + heures.tv, 
  family = binomial, data = hdv2003
)
ggcoef_model(mod4)
mod4 %>%
  ggeffect(c("groupe_age", "sexe")) %>%
  plot()

# Test d'autres combinaisons d'interactions
mod5 <- glm(
  sport ~ groupe_age + sexe:groupe_age + etudes + heures.tv, 
  family = binomial, data = hdv2003
)
ggcoef_model(mod5)
mod5 %>%
  ggeffect(c("groupe_age", "sexe")) %>%
  plot()

## Interaction sexe et niveau d'étude -----
# On teste maintenant l'effet d'une interaction entre le sexe et le niveau d'études.
mod6 <- glm(
  sport ~ groupe_age + sexe * etudes + heures.tv, 
  family = binomial, data = hdv2003
)
mod6 %>%
  ggeffect(c("etudes", "sexe")) %>%
  plot()

# Anova pour tester l'importance de cette interaction
car::Anova(mod6)

## Y a-t-il un risque de multicolinéarité ?
# On vérifie s'il existe un risque de multicolinéarité entre les variables explicatives à l'aide du VIF (Variance Inflation Factor).
car::vif(mod)

# Si nécessaire, la fonction add_vif() sera disponible dans une version future de gtsummary.
