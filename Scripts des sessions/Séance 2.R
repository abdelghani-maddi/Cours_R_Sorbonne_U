# Facteurs et forcats : https://larmarange.github.io/guide-R/manipulation/facteurs.html 
# les facteurs sont utilisés pour représenter des variables catégorielles

x <- c("nord", "sud", "sud", "est", "est", "est")
x |> 
  factor()

x |> 
  factor(levels = c("nord", "est", "sud", "ouest"))


x |> 
  factor(levels = c("nord", "sud"))

x |> 
  readr::parse_factor(levels = c("nord", "sud"))

f <- factor(x)
levels(f)

c("supérieur", "primaire", "secondaire", "primaire", "supérieur") |> 
  ordered(levels = c("primaire", "secondaire", "supérieur"))

class(f)
typeof(f)
as.integer(f)
as.character(f)

# Changer l’ordre des modalités
library(tidyverse)
data("hdv2003", package = "questionr")

hdv2003$qualif |> 
  questionr::freq()

hdv2003$qualif |> 
  fct_rev() |> 
  questionr::freq()

hdv2003$qualif |> 
  fct_relevel("Cadre", "Autre", "Technicien", "Employe") |> 
  questionr::freq()

hdv2003$qualif |> 
  fct_infreq() |> 
  questionr::freq()

hdv2003$qualif |> 
  fct_infreq() |> 
  fct_rev() |> 
  questionr::freq()

v <- c("c", "a", "d", "b", "a", "c")
factor(v)

fct_inorder(v)

hdv2003$qualif_tri_age <-
  hdv2003$qualif |> 
  fct_reorder(hdv2003$age, .fun = mean)
hdv2003 |> 
  dplyr::group_by(qualif_tri_age) |> 
  dplyr::summarise(age_moyen = mean(age))

# Modifier les modalités
hdv2003$sexe |> 
  questionr::freq()

hdv2003$sexe <- 
  hdv2003$sexe |> 
  fct_recode(f = "Femme", m = "Homme")
hdv2003$sexe |> 
  questionr::freq()

hdv2003$nivetud |> 
  questionr::freq()

hdv2003$instruction <- 
  hdv2003$nivetud |> 
  fct_recode(
    "primaire" = "N'a jamais fait d'etudes",
    "primaire" = "A arrete ses etudes, avant la derniere annee d'etudes primaires",
    "primaire" = "Derniere annee d'etudes primaires",
    "secondaire" = "1er cycle",
    "secondaire" = "2eme cycle",
    "technique/professionnel" = "Enseignement technique ou professionnel court",
    "technique/professionnel" = "Enseignement technique ou professionnel long",
    "supérieur" = "Enseignement superieur y compris technique superieur"
  )
hdv2003$instruction |> 
  questionr::freq()

hdv2003$instruction <- 
  hdv2003$nivetud |> 
  fct_collapse(
    "primaire" = c(
      "N'a jamais fait d'etudes",
      "A arrete ses etudes, avant la derniere annee d'etudes primaires",
      "Derniere annee d'etudes primaires"
    ),
    "secondaire" = c(
      "1er cycle",
      "2eme cycle"
    ),
    "technique/professionnel" = c(
      "Enseignement technique ou professionnel court",
      "Enseignement technique ou professionnel long"
    ),
    "supérieur" = "Enseignement superieur y compris technique superieur"
  )

hdv2003$instruction <-
  hdv2003$instruction |> 
  fct_explicit_na(na_level = "(manquant)")
hdv2003$instruction |> 
  questionr::freq()

hdv2003$qualif |> 
  questionr::freq()

hdv2003$qualif |> 
  fct_other(keep = c("Technicien", "Cadre", "Employe")) |> 
  questionr::freq()

hdv2003$qualif |> 
  fct_lump_n(n = 4, other_level = "Autres") |> 
  questionr::freq()

hdv2003$qualif |> 
  fct_lump_min(min = 200, other_level = "Autres") |> 
  questionr::freq()

v <- factor(
  c("a", "a", "b", "a"),
  levels = c("a", "b", "c")
)
questionr::freq(v)

v
v |> fct_drop()

v |> fct_expand("d", "e")

# Découper une variable numérique en classes

hdv2003 <-
  hdv2003 |> 
  mutate(groupe_ages = cut(age, 5))
hdv2003$groupe_ages |> questionr::freq()

hdv2003 <-
  hdv2003 |> 
  mutate(groupe_ages = cut(age, c(18, 20, 40, 60, 80, 97)))
hdv2003$groupe_ages |> questionr::freq()

hdv2003 <-
  hdv2003 |> 
  mutate(groupe_ages = cut(
    age, 
    c(18, 20, 40, 60, 80, 97),
    include.lowest = TRUE
  ))
hdv2003$groupe_ages |> questionr::freq()

hdv2003 <-
  hdv2003 |> 
  mutate(groupe_ages = cut(
    age, 
    c(18, 20, 40, 60, 80, 97),
    include.lowest = TRUE,
    right = FALSE
  ))
hdv2003$groupe_ages |> questionr::freq()

##########################################################







