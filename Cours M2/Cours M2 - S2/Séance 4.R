# Charger les packages nécessaires
library(survey)
library(tidyverse)
library(arules)
library(gtsummary)
library(questionr)
library(ggeffects)  # Pour visualiser les effets marginaux des modèles
library(GGally)     # Pour des graphiques de corrélations et autres visualisations
library(effects)    # Pour calculer et visualiser les effets marginaux

# Description des données https://search.r-project.org/CRAN/refmans/arules/html/Income.html
# Charger le jeu de données 'income' du package 'survey'
data(IncomeESL)

# =============================================================================
# SCRIPT COMPLET : Analyse des déterminants du revenu avec IncomeESL
# =============================================================================

# 2. Chargement et exploration des données IncomeESL
data("IncomeESL", package = "arules")
str(IncomeESL)       # Structure du jeu de données
summary(IncomeESL)   # Résumé global

# Affichage de la distribution de la variable "income"
# La variable income est un facteur ordonné avec 9 niveaux :
# [0,10) < [10,15) < [15,20) < [20,25) < [25,30) < [30,40) < [40,50) < [50,75) < 75+
table(IncomeESL$income)
# Exemple de sortie :
# [0,10)   : 1745 (19.4%)
# [10,15)  :  775 (8.6%)
# [15,20)  :  667 (7.4%)
# [20,25)  :  813 (9.0%)
# [25,30)  :  722 (8.0%)
# [30,40)  : 1110 (12.3%)
# [40,50)  :  969 (10.8%)
# [50,75)  : 1308 (14.5%)
# 75+      :  884 (9.8%)
# Total    : 8993

# 3. Création d'une variable binaire "high_income"
# Nous définissons ici "haut revenu" comme étant les revenus dans les niveaux [50,75) ou 75+
IncomeESL <- IncomeESL %>%
  mutate(high_income = ifelse(income %in% c("[50,75)", "75+"), 1, 0)) %>%
  # Convertir en facteur avec libellés explicites
  mutate(high_income = factor(high_income, levels = c(0, 1), labels = c("Low", "High")))

# Vérifier la répartition de la nouvelle variable cible
table(IncomeESL$high_income)
# On s'attend à environ 24 % de "High" (14.5 + 9.8) et 76 % de "Low"

## Réordonnancement de IncomeESL$sex
IncomeESL$sex <- IncomeESL$sex %>%
  fct_relevel(
    "female", "male"
  )

## Recodage de IncomeESL$age
IncomeESL$age <- IncomeESL$age %>%
  fct_recode(
    "14 à 17 ans" = "14-17",
    "18 à 24 ans" = "18-24",
    "25 à 34 ans" = "25-34",
    "35 à 44 ans" = "35-44",
    "45 à 54 ans" = "45-54",
    "55 à 64 ans" = "55-64"
  )

IncomeESL$age <- as.character(IncomeESL$age)
IncomeESL$age <- as.factor(IncomeESL$age)


IncomeESL$education <- as.character(IncomeESL$education)
IncomeESL$education <- as.factor(IncomeESL$education)


# 4. Analyse descriptive et visualisation
# Exemple : Répartition du revenu élevé par sexe et par niveau d'éducation
# (Les variables suivantes font partie de IncomeESL : sex, marital.status, age, education, occupation, etc.)

# Distribution par sexe
ggplot(IncomeESL, aes(x = sex, fill = high_income)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion de hauts revenus par sexe",
       x = "Sexe",
       y = "Proportion",
       fill = "Revenu")

# Distribution par niveau d'éducation (les niveaux étant ordonnés)
ggplot(IncomeESL, aes(x = education, fill = high_income)) +
  geom_bar(position = "fill") +
  coord_flip() +
  labs(title = "Proportion de hauts revenus par niveau d'éducation",
       x = "Niveau d'éducation",
       y = "Proportion",
       fill = "Revenu")

# 5. Modélisation : Régression logistique
# Nous allons prédire la probabilité d'avoir un revenu élevé (high_income)
# en fonction de quelques variables explicatives. Ici, nous utilisons sex, age, education, occupation et marital.status.

mod <- glm(high_income ~ sex + age + education + occupation + `marital status`+
             `dual incomes`
           ,           data = IncomeESL,
           family = binomial())

# Affichage du résumé du modèle
summary(mod)
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



## Interaction sexe et âge -----
# On teste s'il y a une interaction entre le sexe et le groupe d'âge, pour savoir si l'effet de l'âge varie en fonction du sexe.
mod3 <- glm(high_income ~ sex + age + education + occupation + `marital status`+
              `dual incomes`
            ,
           data = IncomeESL,
           family = binomial())


# Résumé du modèle avec l'interaction sexe * âge
# mod3 %>% tbl_regression() %>% add_global_p()

# Visualisation des coefficients du modèle avec interaction
# ggcoef_model(mod3)


# Effet des variables sexe et groupe_age avec ggeffects
mod3 %>%
  ggeffect(c("sex", "age")) %>%
  plot()

# =============================================================================
# FIN DU SCRIPT
# =============================================================================
