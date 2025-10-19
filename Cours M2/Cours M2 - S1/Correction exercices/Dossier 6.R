# Packages nécessaires
library(tidyverse)
library(gtsummary)
library(labelled)
library(mitools)

# Charger le dataset
data(pisamaths)


### Variables individuelles / élèves
# 
# SCHOOLID : Identifiant de l’école (facteur).
# 
# CNT : Pays de l’élève (facteur).
# 
# STRATUM : Strate d’échantillonnage de l’école (facteur).
# 
# OECD : Indique si le pays fait partie de l’OCDE (facteur).
# 
# STIDSTD : Identifiant unique de l’élève (facteur).
# 
# ST04Q01 : Genre de l’élève (Male / Female).
# 
# ST14Q02 : L’élève a-t-il des livres à la maison ? (Yes / No / NA)
# 
# ST18Q02 : Accès à un ordinateur à la maison ? (Yes / No / NA)


### Scores et performances


# MATHEFF : Score en mathématiques standardisé (z-score).
# 
# OPENPS : Score latent sur un facteur d’« ouverture à l’apprentissage » ou similaire (dépend de la documentation PISA).
# 
# PV1MATH … PV5MATH : Cinq valeurs de plausible values pour le score de mathématiques (méthode PISA pour mesurer l’incertitude).

 
### Poids et pondérations
# 
# W_FSTUWT : Poids de l’élève pour analyses statistiques (pondération pour population nationale).
# 
# W_FSCHWT : Poids de l’école pour analyses statistiques.
# 
# condwt : Poids conditionnel combiné élève + école pour analyses (souvent utilisé pour des moyennes et régressions).
# 
# Variables contextuelles / école
# 
# SC35Q02 : Score ou pourcentage relatif à une pratique scolaire (par exemple % élèves ayant accès à un matériel spécifique).
# 
# PCGIRLS : Proportion de filles dans l’école.
# 
# PROPMA5A : Proportion d’élèves dans un certain niveau de performance (niveau A, le plus élevé).
# 
# ABGMATH : Type de regroupement des élèves par niveau de mathématiques (One of these forms of ability grouping …)
# 
# SMRATIO : Ratio élèves/professeurs (taille moyenne des classes).


pisamaths_clean <- pisamaths %>%
  mutate(
    # Variables élèves
    sexe = factor(ST04Q01, levels = c("Male", "Female"), labels = c("Garçon", "Fille")),
    livres_maison = factor(ST14Q02, levels = c("Yes", "No"), labels = c("Oui", "Non")),
    ordinateur_maison = factor(ST18Q02, levels = c("Yes", "No"), labels = c("Oui", "Non")),
    
    # Score moyen des 5 plausible values
    math_mean = rowMeans(select(., PV1MATH:PV5MATH), na.rm = TRUE),
    
    # Variable sociologique : accès aux ressources à la maison
    ressources_maison = case_when(
      livres_maison == "Oui" & ordinateur_maison == "Oui" ~ "Livres + Ordinateur",
      livres_maison == "Oui" & ordinateur_maison == "Non" ~ "Livres seulement",
      livres_maison == "Non" & ordinateur_maison == "Oui" ~ "Ordinateur seulement",
      livres_maison == "Non" & ordinateur_maison == "Non" ~ "Aucune ressource",
      TRUE ~ NA_character_
    ),
    ressources_maison = factor(ressources_maison)
  ) %>%
  set_variable_labels(
    sexe = "Sexe de l'élève",
    livres_maison = "Livres à la maison",
    ordinateur_maison = "Ordinateur à la maison",
    math_mean = "Score moyen en mathématiques",
    ressources_maison = "Accès aux ressources à la maison"
  )


##

pisamaths_clean %>%
  select(sexe, livres_maison, ordinateur_maison, ressources_maison, math_mean) %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} / {N} ({p}%)"
    ),
    missing = "ifany"
  ) %>%
  bold_labels()


pisamaths_clean %>%
  select(ressources_maison, math_mean, sexe) %>%
  tbl_summary(
    by = ressources_maison,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} / {N} ({p}%)"
    ),
    missing = "ifany"
  ) %>%
  add_p() %>%
  bold_labels()


pisamaths_clean %>%
  select(sexe, math_mean, W_FSTUWT) %>%
  tbl_summary(
    by = sexe,
    statistic = all_continuous() ~ "{mean} ({sd})",
    missing = "ifany"
  )
  

