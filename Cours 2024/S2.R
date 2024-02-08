########################################
# Séance 2 R et Rstudio
# Cours Maddi
# https://larmarange.github.io/guide-R/analyses/statistique-bivariee.html
########################################

# Nettoyer espace de travail ----
# rm(list=ls())

# Lancement des packages ----
library(questionr)
library(tidyverse)
library(gtsummary)
library(labelled)


# Chargement des données -----
data(hdv2003, package = "questionr")
d <- hdv2003

d |> look_for("sport") # ou bien %>% à la place de |>
d |> look_for("sexe")


# Préparation des données ----

library(tidyverse)
d <- d |> 
  mutate(sexe = sexe |> fct_relevel("Femme"))


# Préparation des données : faire comme suit (ou utiliser les Adins pour plus de simplicité, comme vu en cours)

data(hdv2003, package = "questionr")
d <-
  hdv2003 |> 
  mutate(
    sexe = sexe |> fct_relevel("Femme"),
    groupe_ages = age |>
      cut(
        c(18, 25, 45, 65, 99),
        right = FALSE,
        include.lowest = TRUE,
        labels = c("18-24 ans", "25-44 ans",
                   "45-64 ans", "65 ans et plus")
      ),
    etudes = nivetud |> 
      fct_recode(
        "Primaire" = "N'a jamais fait d'etudes",
        "Primaire" = "A arrete ses etudes, avant la derniere annee d'etudes primaires",
        "Primaire" = "Derniere annee d'etudes primaires",
        "Secondaire" = "1er cycle",
        "Secondaire" = "2eme cycle",
        "Technique / Professionnel" = "Enseignement technique ou professionnel court",
        "Technique / Professionnel" = "Enseignement technique ou professionnel long",
        "Supérieur" = "Enseignement superieur y compris technique superieur"
      ) |> 
      fct_na_value_to_level("Non documenté")  
  ) |> 
  set_variable_labels(
    sport = "Pratique un sport ?",
    sexe = "Sexe",
    groupe_ages = "Groupe d'âges",
    etudes = "Niveau d'études",
    relig = "Rapport à la religion",
    heures.tv = "Heures de télévision / jour"
  )

# Statistiques descriptives ----

library(gtsummary)
theme_gtsummary_language("fr", decimal.mark = ",", big.mark = " ") # Changer le thème pour avoir les résultats en français

d |> 
  tbl_summary(
    by = sport,
    include = c(sexe, groupe_ages, etudes, relig, heures.tv)
  ) |>
  add_overall(last = TRUE) |> 
  add_p() |> 
  bold_labels() |> 
  modify_spanning_header(
    update = all_stat_cols() ~ "**Pratique un sport ?**"
  )


