# Charger les packages nécessaires


library(tidyverse)  # Manipulation des données (dplyr, ggplot2, tidyr...)
library(labelled)   # Pour gérer les étiquettes des variables
library(questionr)  # Pour des fonctions pratiques en analyse d'enquête
library(gtsummary)  # Pour résumer les résultats de manière élégante
library(GGally)     # Pour des graphiques de corrélations et autres visualisations
library(effects)    # Pour calculer et visualiser les effets marginaux
library(ggeffects)  # Pour visualiser les effets marginaux des modèles


# Charger les données
# Supposons que les données sont stockées dans un dataframe nommé 'femmes'

data(fecondite)

# 1️⃣ **Préparation des données**
# Recoder les valeurs spéciales en NA pour `nb_enf_ideal`
femmes <- femmes %>%
  mutate(nb_enf_ideal = na_if(nb_enf_ideal, 96),  # "Ne sait pas" -> NA
         nb_enf_ideal = na_if(nb_enf_ideal, 99))  # "Manquant" -> NA


# 🔹 **Recodage des variables avec libellés**
femmes <- femmes %>%
  mutate(
    milieu = factor(milieu, levels = c(1, 2), labels = c("Urbain", "Rural")),
    region = factor(region, levels = c(1, 2, 3, 4), labels = c("Nord", "Est", "Sud", "Ouest")),
    educ = factor(educ, levels = c(0, 1, 2, 3), labels = c("Aucun", "Primaire", "Secondaire", "Supérieur")),
    travail = factor(travail, levels = c(0, 1, 9), labels = c("Non", "Oui", "Manquant")),
    matri = factor(matri, levels = c(0, 1, 2, 3, 4, 5), 
                   labels = c("Célibataire", "Mariée", "Concubinage", "Veuve", "Divorcée", "Séparée")),
    religion = factor(religion, levels = c(1, 2, 3, 4, 5), 
                      labels = c("Musulmane", "Chrétienne", "Protestante", "Sans religion", "Autre"))
  )

# Création de la variable binaire (0-3 enfants = 0, plus de 3 enfants = 1)
femmes <- femmes %>%
  mutate(plus_de_3_enfants = ifelse(nb_enf_ideal > 4, 1, 0))

# Vérifier la distribution
table(femmes$plus_de_3_enfants, useNA = "ifany")

# 2️⃣ **Statistiques descriptives et graphiques**
# Résumé statistique
summary(femmes$nb_enf_ideal)

# Histogramme du nombre idéal d'enfants
ggplot(femmes, aes(x = nb_enf_ideal)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "white") +
  labs(title = "Distribution du nombre idéal d'enfants",
       x = "Nombre idéal d'enfants",
       y = "Effectif") +
  theme_minimal()

# Comparaison selon le milieu de résidence
ggplot(femmes, aes(x = as.factor(milieu), fill = as.factor(plus_de_3_enfants))) +
  geom_bar(position = "fill") +
  labs(title = "Proportion de personnes voulant plus de 3 enfants selon le milieu",
       x = "Milieu",
       y = "Proportion",
       fill = "Plus de 3 enfants") +
  theme_minimal()

# 3️⃣ **Régression logistique**
# Création du modèle avec quelques variables explicatives
modele_logit <- glm(plus_de_3_enfants ~ age + milieu + region + educ + travail + matri + religion, 
                    data = femmes, family = binomial)

# Résumé du modèle
summary(modele_logit)

# Présentation sous forme de tableau interprétable
tbl_regression(modele_logit, exponentiate = TRUE)  # Exp(B) pour interpréter les OR

