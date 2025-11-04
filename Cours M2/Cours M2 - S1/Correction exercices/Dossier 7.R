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
## Cutting student_subset_2022$escs into student_subset_2022$escs_cat
df$escs_cat <- cut(student_subset_2022$escs,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(-1.23345, -0.18225, 0.706, 7.0749),
  labels = c("faible", "moyen", "élevé")
)


# 4) Effectifs par catégorie
table(df$escs_cat, useNA = "ifany")



tbl_genre_np <- df %>%
  select(mean_score, math, read, science, escs, mother_educ, father_educ, internet, computer, gender) %>%
  tbl_summary(by = gender,
              type = list(
                all_continuous() ~ "continuous2",
                all_categorical() ~ "categorical"
              ),
              statistic = list(
                all_continuous() ~ "{mean} ({sd})",
                all_categorical() ~ "{n} ({p}%)"
              ),
              missing = "ifany") %>%
  add_p() %>%
  separate_p_footnotes() %>%
  bold_labels() %>%
  modify_caption("**Tableau A — Caractéristiques par genre (non pondéré)**")
tbl_genre_np

# Effet du milieu social (ESCs)

tbl_escs_np <- df %>%
  select(mean_score, math, read, science, gender, internet, computer, book, car, escs_cat) %>%
  tbl_summary(by = escs_cat,
              type = list(
                all_continuous() ~ "continuous2",
                all_categorical() ~ "categorical"
              ),
              statistic = list(
                all_continuous() ~ "{mean} ({sd})",
                all_categorical() ~ "{n} ({p}%)"
              ),
              missing = "ifany") %>%
  add_p() %>%
  bold_labels() %>%
  modify_caption("**Tableau C — Scores et conditions matérielles selon ESCs (non pondéré)**")
tbl_escs_np


# Comparaisons internationales
pays_sel <- c("FRA","DEU","ESP")  # France, Allemagne, Espagne
subset_countries <- df %>% filter(country %in% pays_sel)
subset_countries$country <- as.character(subset_countries$country)

# Non pondéré
tbl_pays_np <- subset_countries %>%
  select(mean_score, escs, gender, country) %>%
  tbl_summary(by = country,
              type = list(
                all_continuous() ~ "continuous2",
                gender ~ "categorical"
              ),
              statistic = list(
                all_continuous() ~ "{mean} ({sd})",
                all_categorical() ~ "{n} ({p}%)"
              ),
              missing = "no") %>%
  add_p() %>%
  bold_labels() %>%
  modify_caption("**Tableau E — Comparaison entre pays**")
tbl_pays_np



# Genre × Pays : écarts comparés

# Moyennes par (pays, genre)
resume_pg <- subset_countries %>%
  group_by(country, gender) %>%
  summarise(
    n = n(),
    mean_math    = mean(math, na.rm = TRUE),
    mean_read    = mean(read, na.rm = TRUE),
    mean_science = mean(science, na.rm = TRUE),
    .groups = "drop"
  )
resume_pg

# Visualisation : écart filles-garçons par pays sur mean_score
subset_countries %>%
  group_by(country, gender) %>%
  summarise(mean_score = mean(mean_score, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = country, y = mean_score, fill = gender)) +
  geom_col(position = position_dodge(width = 0.6)) +
  labs(x = "Pays", y = "Score moyen", title = "Score moyen par genre et pays") +
  theme_minimal()

# Tableau récapitulatif par genre
tab_final <- df %>%
  select(mean_score, escs, mother_educ, internet, country, gender) %>%
  tbl_summary(by = gender,
              type = list(
                mean_score ~ "continuous2",
                escs ~ "continuous2",
                mother_educ ~ "categorical",
                internet ~ "categorical",
                country ~ "categorical"
              ),
              statistic = list(
                all_continuous() ~ "{mean} ({sd})",
                all_categorical() ~ "{n} ({p}%)"
              ),
              missing = "ifany") %>%
  add_p() %>%
  modify_caption("**Tableau G — Caractéristiques des élèves selon le genre (non pondéré)**") %>%
  bold_labels()
tab_final


## Figures rapides

# Distribution du score moyen
ggplot(df, aes(x = mean_score)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution du score moyen", x = "Score moyen", y = "Effectif") +
  theme_minimal()

# Boxplot par catégorie ESCs
df %>%
  filter(!is.na(escs_cat)) %>%
  ggplot(aes(x = escs_cat, y = mean_score)) +
  geom_boxplot() +
  labs(title = "Score moyen par catégorie ESCs", x = "ESCs", y = "Score moyen") +
  theme_minimal()

# Barplot pays (top 10 par effectif)
top_pays <- df %>% count(country, sort = TRUE) %>% slice_head(n = 10) %>% pull(country)
df %>% filter(country %in% top_pays) %>%
  group_by(country) %>%
  summarise(m = mean(mean_score, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(country, m), y = m)) +
  geom_col() +
  coord_flip() +
  labs(title = "Score moyen par pays (Top 10 effectifs)", x = "Pays", y = "Score moyen") +
  theme_minimal()

