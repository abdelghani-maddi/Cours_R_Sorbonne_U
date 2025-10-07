# Cours : Préparation et description des données avec R

---
  
  ## Objectifs pédagogiques
  
#   À l'issue du cours, les étudiant·e·s sauront :
# 
# * Explorer systématiquement un jeu de données sociologique (vérifier la structure, le dictionnaire, les valeurs manquantes).
# * Nettoyer et préparer les variables (recode, factorisation, gestion des valeurs aberrantes et manquantes).
# * Produire des tableaux et graphiques descriptifs (effectifs, proportions, moyennes, écarts‑type), y compris **pondérés**.

---

## Jeu de données étudié `happy` — dictionnaire rapide

# Le jeu est déjà chargé en mémoire sous l'objet `happy`.

# Variables (extrait de `look_for(happy)`):
#   
# 1. `id` (dbl) : identifiant unique
# 2. `happy` (fct) : variable de bien‑être subjectif — niveaux : `not too happy`, `pretty happy`, `very happy` (4717 manquantes)
# 3. `year` (dbl) : année de l'enquête
# 4. `age` (dbl) : âge (184 manquantes)
# 5. `sex` (fct) : `male` / `female`
# 6. `marital` (fct) : statut matrimonial (`married`, `never married`, `divorced`, `widowed`, `separated`) (14 manquantes)
# 7. `degree` (fct) : niveau d'études (`lt high school`, `high school`, `junior college`, `bachelor`, `graduate`) (164 manquantes)
# 8. `finrela` (fct) : situation financière relative (`far below average`, `below average`, `average`, `above average`, `far above average`) (4876 manquantes)
# 9. `health` (fct) : état de santé (`poor`, `fair`, `good`, `excellent`) (12 529 manquantes)
# 10. `wtssall` (dbl) : poids d'échantillonnage (pondération)

---

## Chargement des packages ----
library(tidyverse)
library(questionr)
library(gtsummary)

# Chargement des données ----
data(happy)

# Exploration rapide ----
glimpse(happy)
look_for(happy)
describe(happy)
str(happy)


# Préparation des données ----

# Ajouter une modalité pour les NA de la variable happy
happy$happy <- happy$happy %>%
  fct_na_value_to_level("No_answer")

# Ajouter une modalité pour les NA de la variable degree
happy$degree <- happy$degree %>%
  fct_na_value_to_level("No_answer")

# Recodage de degree en équivalent français
happy <- happy %>%
  mutate(
    degree = fct_recode(degree,
                        "<= collège"         = "lt high school",
                        "Bac"                = "high school",
                        "Bac+2"              = "junior college",
                        "Licence (Bac+3)"    = "bachelor",
                        "Master/Doctorat"    = "graduate"
    )
  )


# Réordonner finrela du plus bas au plus haut
happy <- happy %>%
  mutate(finrela = fct_relevel(finrela, c("far below average", "below average", "average", "above average", "far above average")))

# Détecter âges aberrants
happy <- happy %>%
  mutate(happy2 = case_when(
    happy == "very happy" ~ "happy",
    happy == "pretty happy" ~ "happy",
    happy == "not too happy" ~ "unhappy",
    TRUE ~ NA_character_
  )) %>%
  mutate(happy2 = factor(happy2, levels = c("unhappy", "happy")))


# Créer des regroupements simplifiés
happy <- happy %>%
  mutate(
    finrela2 = fct_collapse(
      finrela,
      "Bas" = c("far below average", "below average"),
      "Moyen" = "average",
      "Haut" = c("above average", "far above average")
    ),
    health2 = fct_collapse(
      health,
      "Mauvaise santé" = c("poor", "fair"),
      "Bonne santé" = c("good", "excellent")
    )
  )



# Créer une variable combinée santé × argent
happy <- happy %>%
  mutate(sante_argent = interaction(health2, finrela2, sep = " × ", drop = TRUE))


happy <- happy %>%
  filter(!is.na(health2) & !is.na(finrela2)) %>%
  mutate(sante_argent = paste0(health2, " x ", finrela2))

# Stats desc -----

# Tableau croisé pondéré


######
# calcul non-pondéré
ordre <- happy %>%
  filter(!is.na(happy2), !is.na(sante_argent)) %>%     # 1️⃣ on garde seulement les lignes complètes
  group_by(sante_argent) %>%                           # 2️⃣ on regroupe par combinaison Santé × Argent
  summarise(p_happy = mean(happy2 == "happy")) %>%     # 3️⃣ on calcule la part de "happy" dans chaque groupe
  arrange(desc(p_happy)) %>%                           # 4️⃣ on classe du plus heureux au moins heureux
  pull(sante_argent)                                   # 5️⃣ on extrait juste la liste des modalités

# appliquer l'ordre
happy <- happy %>%
  mutate(sante_argent = factor(sante_argent, levels = ordre))

# tableau (garde l'ordre des niveaux tel quel)
happy %>%
  filter(!is.na(happy2), !is.na(sante_argent)) %>%
  select(sante_argent, happy2) %>%
  tbl_summary(
    by = happy2,
    label = list(sante_argent ~ "Santé × Argent"),
    percent = "row",
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    missing = "no"
  ) %>%
  modify_spanning_header(all_stat_cols() ~ "**Bonheur subjectif**") %>%
  modify_header(label ~ "**Ordre décroissant de 'happy'**") %>%
  bold_labels() %>%
  add_overall(last = T) %>%
  add_p()
  

happy %>%
  tbl_summary(
    sort = happy ~ 'frequency'
  )
