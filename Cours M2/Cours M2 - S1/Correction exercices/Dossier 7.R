# Packages utilisés
library(tidyverse)
library(questionr)
library(gtsummary)
library(labelled)
# Pour analyses pondérées
library(survey)
library(learningtower)

# Données
data("student_subset_2022")
# dico ici : https://cran.r-project.org/web/packages/learningtower/learningtower.pdf

# Réglages gtsummary (fr, formats)
theme_gtsummary_language("fr")
theme_gtsummary_compact()  # tableaux compacts


glimpse(student_subset_2022)
describe(student_subset_2022)

# Comptage pays
n_pays <- n_distinct(student_subset_2022$country)

# Variables 100% manquantes
colMeans(is.na(student_subset_2022)) * 100


# 1) Retirer variables vides
df <- student_subset_2022 %>%
  select(-desk, -dishwasher, -wealth)

# 2) Score moyen
df <- df %>%
  mutate(mean_score = rowMeans(across(c(math, read, science)), na.rm = TRUE))

# 3) Catégorie ESCs
df <- df %>%
  mutate(escs_cat = case_when(
    escs < -0.5 ~ "faible",
    escs <  0.5 ~ "moyen",
    escs >= 0.5 ~ "élevé",
    TRUE        ~ NA_character_
  )) %>%
  mutate(escs_cat = factor(escs_cat, levels = c("faible","moyen","élevé")))

# 4) Effectifs par catégorie
table(df$escs_cat, useNA = "ifany")
