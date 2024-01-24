########################################
# https://larmarange.github.io/guide-R/analyses/statistique-bivariee.html
########################################

# Lancement des packages ----
# library(questionr)
# library(openxlsx)
# install.packages("codebook")
# install.packages("gtsummary")
# library(tidyverse)
# library(gtsummary)
# library(labelled)


# Chargement des données ----
# data(hdv2003)
# write.xlsx(hdv2003, "hdv2003.xlsx")

# Inspection générale des données ----
# summary
# str
# glimpse
# look_for
# describe
# codebook

# Étiquettes de variables ----
# var_label(hdv2003$occup) = "Occupation actuelle"
# var_label(hdv2003$age) = "Âge de la personne"
# labelled::set_variable_labels()

# Factors ----
# class(hdv2003$qualif)
# levels(hdv2003$qualif)
# hdv2003$qualif |> 
#   fct_relevel("Cadre", "Autre", "Technicien", "Employe") |> 
#   questionr::freq()
# Usage du adin pour ordonner les variables


# Modifier les modalités : usage du adin ----
# Descritiser une variable : usage du adin ----

# Tableaux et tris à plat ----
# hdv2003 %>% 
#   tbl_summary(
#     include = c(age, occup, heures.tv),
#     label = list(age ~ "Âge médian")
#   )

# hdv2003 |>
#   tbl_summary(
#     include = c(age, heures.tv),
#     statistic = 
#       all_continuous() ~ "Moy. : {mean} [min-max : {min} - {max}]"
#   )


# Tableau croisé avec gtsummary ----
# library(gtsummary)
# theme_gtsummary_language("fr", decimal.mark = ',')
# data("trial")

# library(gtsummary)
# trial |> 
#   tbl_summary(
#     include = c(stage, trt),
#     by = grade,
#     statistic = ~ "{p}% ({n}/{N})",
#     percent = "row"
#   ) |> 
#   add_overall(last = TRUE)


# Croiser deux variables : simple pour 2 var
# trial |> 
#   tbl_cross(
#     row = stage,
#     col = grade,
#     percent = "row"
#   )


# Test du Chi² et dérivés
# trial |> 
#   tbl_summary(
#     include = stage,
#     by = grade
#   ) |> 
#   add_p()

# Comparaison de deux proportions
# trial |> 
#   tbl_summary(
#     by = trt,
#     include = response
#   ) |> 
#   add_difference()

# Représentations graphiques
