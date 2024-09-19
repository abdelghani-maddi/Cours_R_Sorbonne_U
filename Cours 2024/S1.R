########################################
# Séance 1 R et Rstudio
# Cours Maddi
# https://larmarange.github.io/guide-R/analyses/statistique-bivariee.html
########################################

# Nettoyer espace de travail ----
# rm(list=ls())

# Lancement des packages ----
library(questionr)
library(openxlsx) # pour lire et enregistrer des fichier excel
library(tidyverse)
library(gtsummary)
library(labelled)


# Chargement des données ----
data(hdv2003)
write.xlsx(hdv2003, "hdv2003.xlsx")

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
codebook::label_browser_static(femmes)


# Étiquettes de variables ----
var_label(hdv2003$occup) = "Occupation actuelle"
var_label(hdv2003$age) = "Âge de la personne"

# Factors ----
class(hdv2003$qualif)
levels(hdv2003$qualif)
hdv2003$qualif |>
  fct_relevel("Cadre", "Autre", "Technicien", "Employe") |>
  questionr::freq()


# Représentations graphiques (usage de ESQUISSE) ----
library(questionr)
data(hdv2003)

library(ggplot2)

ggplot(hdv2003) +
 aes(x = age, y = nivetud) +
 geom_boxplot(fill = "#112446") +
 theme_minimal()


library(dplyr)

hdv2003 %>%
 filter(!is.na(nivetud)) %>%
 ggplot() +
 aes(x = nivetud, fill = sexe) +
 geom_bar(position = "dodge") +
 scale_fill_hue(direction = 1) +
 theme_minimal()
