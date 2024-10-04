# Chargement des packages nécessaires
library(gtsummary)
library(tidyverse)
library(gt)
library(officer)
library(flextable)

# Utilisation du jeu de données "trial" fourni par gtsummary
data(trial)

# ----------- EXERCICE 1 : Résumé statistique simple ----------- #
# Objectif : Créer un tableau résumant les variables quantitatives et qualitatives

# Création du tableau récapitulatif avec gtsummary
summary_table <- tbl_summary(trial)

# Affichage du tableau récapitulatif dans la console
summary_table

# Modification du tableau pour afficher la médiane à la place de la moyenne pour les variables numériques
summary_table_median <- tbl_summary(
  trial,
  statistic = list(all_continuous() ~ "{median} ({p25}, {p75})")
)

# Affichage du tableau avec la médiane
summary_table_median

# Ajout d'une stratification selon la variable 'trt' (groupes de traitement)
summary_table_by_group <- tbl_summary(trial, by = trt)

# Affichage du tableau stratifié
summary_table_by_group


# ----------- EXERCICE 2 : Analyse bivariée ----------- #
# Objectif : Comparer les distributions d'une variable quantitative et d'une variable qualitative

# Filtrage des données pour retirer les valeurs manquantes de la variable 'response'
trial_filtered <- trial %>% filter(!is.na(response))

# Création du tableau récapitulatif avec stratification par la variable 'trt'
bivariate_summary <- tbl_summary(trial_filtered, by = trt)

# Ajout des tests statistiques pour comparer les groupes
bivariate_summary_with_p <- bivariate_summary %>%
  add_p()

# Affichage du tableau avec les tests statistiques
bivariate_summary_with_p

# Personnalisation des tests statistiques pour certaines variables
bivariate_summary_custom_test <- tbl_summary(trial_filtered, by = trt) %>%
  add_p(test = list(age ~ "t.test", response ~ "chisq.test"))

# Affichage du tableau avec des tests personnalisés
bivariate_summary_custom_test


# ----------- EXERCICE 3 : Résumé de modèle de régression ----------- #
# Objectif : Créer un tableau résumant les résultats d'un modèle de régression logistique

# Ajustement d'un modèle de régression logistique pour prédire 'response' en fonction d'autres variables
mod <- glm(response ~ age + trt + grade + stage, data = trial, family = binomial)

# Création du tableau récapitulatif des coefficients du modèle
regression_table <- tbl_regression(mod)

# Affichage du tableau récapitulatif
regression_table

# Transformation des coefficients en odds ratios et ajout des intervalles de confiance à 95%
regression_table_exp <- tbl_regression(mod, exponentiate = TRUE)

# Affichage du tableau avec les odds ratios
regression_table_exp

# Ajout de labels personnalisés aux variables
regression_table_labels <- regression_table_exp %>%
  modify_header(label ~ "**Variable**")

# Affichage du tableau avec des labels personnalisés
regression_table_labels

# Ajout des symboles de significativité pour les p-values
regression_table_significance <- regression_table_exp %>%
  modify_column_hide(columns = p.value) %>%
  add_significance_stars()

# Affichage du tableau avec les p-values sous forme de symboles
regression_table_significance


# ----------- EXERCICE 4 : Combinaison de plusieurs tableaux ----------- #
# Objectif : Combiner les résultats de plusieurs analyses dans un seul tableau

# Création de deux tableaux : un récapitulatif descriptif et un tableau de régression
tbl_summary_age <- tbl_summary(trial, by = trt)
mod <- glm(response ~ age + trt + grade, data = trial, family = binomial)
tbl_reg <- tbl_regression(mod)

# Fusion des deux tableaux dans un seul tableau combiné
tbl_combined <- tbl_merge(
  tbls = list(tbl_summary_age, tbl_reg),
  tab_spanner = c("**Résumé statistique**", "**Modèle logistique**")
)

# Affichage du tableau combiné
tbl_combined

# Personnalisation de l'apparence avec un en-tête unique pour les résultats combinés
tbl_combined_custom_header <- tbl_combined %>%
  modify_spanning_header(everything() ~ "**Résultats combinés**")

# Affichage du tableau combiné avec personnalisation
tbl_combined_custom_header


# ----------- EXERCICE 5 : Exportation des résultats ----------- #
# Objectif : Exporter les tableaux dans différents formats (HTML, LaTeX, Word)

# Exportation du tableau récapitulatif au format HTML
tbl_summary(trial) %>%
  as_gt() %>%
  gt::gtsave("summary_table.html")

# Exportation du tableau au format LaTeX
tbl_summary(trial) %>%
  as_gt() %>%
  gt::gtsave("summary_table.tex")

# Exportation du tableau au format Word
tbl_summary(trial) %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "summary_table.docx")

# Ajustement de la largeur des colonnes dans le document Word exporté
tbl_summary(trial) %>%
  as_flex_table() %>%
  set_table_properties(width = 1.5, layout = "autofit") %>%
  flextable::save_as_docx(path = "summary_table_adjusted.docx")

