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

# Analyse descriptive du fichier trial

library(tidyverse)
library(labelled)
library(questionr)
library(gtsummary)
library(GGally)

theme_gtsummary_language("fr", decimal.mark = ",", big.mark = " ")

# Import des données -----

trial <- readxl::read_excel("donnees/trial.xlsx")

var_label(trial$age) <- "Âge en années"
var_label(trial$trt) <- "Traitement"

save(trial, file = "donnees/trial.RData")

# Statistique univariée -----

# une variable quantitative

mean(trial$age, na.rm = TRUE)
median(trial$age, na.rm = TRUE)
quantile(trial$age, na.rm = TRUE, probs = c(.1, .23, .58))
min(trial$age, na.rm = TRUE)
max(trial$age, na.rm = TRUE)
range(trial$age, na.rm = TRUE)
var(trial$age, na.rm = TRUE)
sd(trial$age, na.rm = TRUE)
summary(trial$age)

# une variable catégorielle

table(trial$stage)
xtabs(~ stage, data = trial)
freq(trial$stage, cum = TRUE, total = TRUE)

# gtsummary

tbl_summary(trial)

# Graphiques bivariés -------

ggplot(trial) +
  aes(x = stage, y = age, fill = trt) +
  geom_boxplot() +
  scale_fill_hue() +
  labs(title = "Titre de mon graphique", subtitle = "sous-titre", caption = "enquête 2020") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Tableaux croisés ------

theme_gtsummary_mean_sd()

# %>%
# x %>% f(y)    équivalent à  f(x, y)

trial %>%
  tbl_summary(
    by = "trt",
    percent = "row",
    digits = list(
      all_categorical() ~ c(0, 1),
      all_continuous() ~ c(1, 1)
    )
  ) %>%
  add_overall(last = TRUE) %>%
  add_p()

  

# fusion de deux tableaux 

t1 <- trial %>%
  tbl_summary(include = c("trt", "age", "grade"), by = "trt")
t2 <- trial %>%
  tbl_summary(include = c("stage", "age", "grade"), by = "stage")
tbl_merge(list(t1, t2))

# ggbivariate()

ggbivariate(trial, outcome = "trt", explanatory = c("age", "stage", "grade"))
ggtable(
  trial, 
  columnsX = c("trt"), 
  columnsY = c("grade", "stage"),
  cells = "row.prop",
  fill = "std.resid"
)







