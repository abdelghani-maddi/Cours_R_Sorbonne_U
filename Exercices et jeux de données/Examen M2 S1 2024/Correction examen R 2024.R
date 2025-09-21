# Script de correction pour l'analyse des données "happy"

# Chargement des packages nécessaires
library(tidyverse)
library(questionr)
library(FactoMineR)
library(factoextra)
library(ade4)
library(gtsummary)
library(rsconnect)

# Chargement des données
# Les données sont disponibles via le package "productplots" (à installer si nécessaire)
data("happy")

# Vérification initiale
glimpse(happy)

# Partie 1 : Exploration et préparation des données

# Aperçu des premières lignes
head(happy)

# Dimensions du jeu de données
dim(happy)

# Structure des données
str(happy)

# Types de variables
look_for(happy)

# Valeurs manquantes par variable
happy %>% summarise(across(everything(), ~sum(is.na(.))))

# Création d'une variable "age_group" (il y a 1000 et 1 façons de le faire :))
happy <- happy %>% 
  mutate(age_group = case_when(
    age >= 18 & age <= 29 ~ "Jeunes adultes",
    age >= 30 & age <= 49 ~ "Adultes",
    age >= 50 & age <= 64 ~ "Seniors",
    age >= 65 ~ "Âgés"
  ))

# Création d'une variable "happiness_score"
happy <- happy %>% 
  mutate(happiness_score = case_when(
    happy == "very happy" ~ 3,
    happy == "pretty happy" ~ 2,
    happy == "not too happy" ~ 1
  ))

# Partie 2 : Analyse descriptive

# Statistiques descriptives pour l'âge
summary(happy$age)

# Répartition par niveau d'éducation (plusieurs façon également pour le faire)
freq(happy$degree)

# Répartition par groupe d'âge
freq(happy$age_group)

# Croisement entre bonheur et sexe : deux façons :
# un peu moche
table_happy_sex <- happy %>% 
  count(happy, sex) %>% 
  group_by(sex) %>% 
  mutate(prop = n / sum(n))
table_happy_sex

# mieux
happy %>%
  filter(!is.na(happy)) %>%
  tbl_summary(
    include = c(happy, sex),
    by = sex,
    percent = "row"
  ) %>%
  add_p() %>%
  bold_labels()

# Visualisation du croisement
happy %>%
  filter(!is.na(happy)) %>% 
  ggplot(aes(x = sex, fill = happy)) +
  geom_bar(position = "fill") +
  labs(title = "Répartition du bonheur selon le sexe", 
       x = "Sexe", y = "Proportion") +
  scale_fill_brewer(palette = "Set3")

# Partie 3 : Comparaisons et interprétations

# Répartition du bonheur selon le statut matrimonial
happy %>%
  filter(!is.na(happy) & !is.na(marital)) %>%  
  ggplot(aes(x = marital, fill = happy)) +
  geom_bar(position = "fill") +
  coord_flip() +
  labs(title = "Bonheur par statut matrimonial", 
       x = "Statut matrimonial", y = "Proportion") +
  scale_fill_brewer(palette = "Set2")

# Répartition du bonheur selon la santé
happy %>%
  filter(!is.na(happy) & !is.na(health)) %>%  
  ggplot(aes(x = health, fill = happy)) +
  geom_bar(position = "fill") +
  labs(title = "Bonheur par état de santé", 
       x = "Santé", y = "Proportion") +
  scale_fill_brewer(palette = "Set1")

# Partie 4 : Analyse des correspondances multiples (ACM)

# Préparation des données pour l'ACM
happy_acm <- happy %>% 
  select(happy, marital, degree, health, finrela) %>% 
  drop_na()

# Regroupement des modalités de finrela
happy_acm <- happy_acm %>% 
  mutate(financial_status = case_when(
    finrela %in% c("far below average", "below average") ~ "low",
    finrela == "average" ~ "average",
    finrela %in% c("above average", "far above average") ~ "high"
  ))

# Réalisation de l'ACM
acm_res <- MCA(happy_acm, quali.sup = 5, graph = FALSE)

# Résultats des dimensions principales
fviz_screeplot(acm_res, addlabels = TRUE, ylim = c(0,13))

# Visualisation des modalités
fviz_mca_var(acm_res, repel = TRUE, 
             ggtheme = theme_minimal(),
             title = "Représentation des modalités")

# Visualisation des individus
fviz_mca_ind(acm_res, geom = "point", 
             ggtheme = theme_minimal(),
             title = "Représentation des individus")

# Interprétation des résultats
print(acm_res$var$contrib)

# Partie 5 : Synthèse et recommandations
# Tableau de synthèse 
# (c'est encore mieux de rajouter des labels pour les noms de variables, 
# mais pas eu le temps :))
happy %>% 
  drop_na() %>%
  tbl_summary(by = happy, 
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = all_continuous() ~ 2,
              percent = "row") %>%
  bold_labels() %>%
  add_overall() %>%
  add_p()

