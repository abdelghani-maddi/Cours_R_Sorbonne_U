# Manipulation de données

# Il est fréquent d’enchainer des opérations en appelant successivement des fonctions sur le résultat de l’appel précédent.
# Prenons un exemple. Supposons que nous ayons un vecteur numérique v dont nous voulons calculer la moyenne puis l’afficher via un message dans la console. 
# Pour un meilleur rendu, nous allons arrondir la moyenne à une décimale, mettre en forme le résultat à la française, c’est-à-dire avec la virgule comme 
# séparateur des décimales, créer une phrase avec le résultat, puis l’afficher dans la console. Voici le code correspondant, étape par étape.

v <- c(1.2, 8.7, 5.6, 11.4)
m <- mean(v)
r <- round(m, digits = 1)
f <- format(r, decimal.mark = ",")
p <- paste0("La moyenne est de ", f, ".")
message(p)

# Cette écriture, n’est pas vraiment optimale, car cela entraine la création d’un grand nombre de variables intermédiaires totalement inutiles. 
# Nous pourrions dès lors imbriquer les différentes fonctions les unes dans les autres :

message(paste0("La moyenne est de ", format(round(mean(v),        digits = 1), decimal.mark = ","), "."))


# Le pipe de R
# Depuis la version 4.1, R a introduit ce que l’on nomme un pipe (tuyau en anglais), un nouvel opérateur noté |>.
# Le principe de cet opérateur est de passer l’élément situé à sa gauche comme premier argument de la fonction située à sa droite.
# Ainsi, l’écriture x |> f() est équivalente à f(x) et l’écriture x |> f(y) à f(x, y).

# Parfois, on souhaite passer l’objet x à un autre endroit de la fonction f() que le premier argument. Depuis la version 4.2, 
# R a introduit l’opérateur _,que l’on nomme un placeholder, pour indiquer où passer l’objet de gauche. Ainsi, x |> f(y, a = _) devient 
# équivalent à f(y, a = x). ATTENTION : le placeholder doit impérativement être transmis à un argument nommé !

v |> 
  mean() |> 
  round(digits = 1) |> 
  format(decimal.mark = ",") |> 
  paste0("La moyenne est de ", m = _, ".") |> 
  message()

# Le pipe du tidyverse : %>%
# Ce n’est qu’à partir de la version 4.1 sortie en 2021 que R a proposé de manière native un pipe, en l’occurence l’opérateur |>.
# Cet opérateur s’écrit %>% et il dispose lui aussi d’un placeholder qui est le .. La syntaxe du placeholder est un peu plus 
#souple puisqu’il peut être passé à tout type d’argument, y compris un argument sans nom. Si l’on reprend notre exemple précédent.

library(magrittr)
v %>% 
  mean() %>%
  round(digits = 1) %>%
  format(decimal.mark = ",") %>%
  paste0("La moyenne est de ", ., ".") %>%
  message()


# Facteurs et forcats : https://larmarange.github.io/guide-R/manipulation/facteurs.html 
# les facteurs sont utilisés pour représenter des variables catégorielles , c’est-à-dire des variables 
# qui ont un nombre fixé et limité de valeurs possibles (par exemple une variable sexe ou une variable niveau d’éducation).

# Le plus simple pour créer un facteur est de partir d’un vecteur textuel et d’utiliser la fonction factor().
x <- c("nord", "sud", "sud", "est", "est", "est")
x |> 
  factor()

# Par défaut, les niveaux du facteur obtenu correspondent aux valeurs uniques du fecteur textuel, triés par 
# ordre alphabétique. Si l’on veut contrôler l’ordre des niveaux, et éventuellement indiquer un niveau 
# absent des données, on utilisera l’argument levels de factor().
x |> 
  factor(levels = c("nord", "est", "sud", "ouest"))

# Si une valeur observée dans les données n’est pas indiqué dans levels, elle sera siliencieusement convertie en valeur manquante (NA).
x |> 
  factor(levels = c("nord", "sud"))

# Si l’on veut être averti par un warning dans ce genre de situation, on pourra avoir plutôt recours à la fonction readr::parse_factor() du package readr, 
# qui, le cas échéant, renverra un tableau avec les problèmes rencontrés.
x |> 
  readr::parse_factor(levels = c("nord", "sud"))

f <- factor(x)
levels(f)

# Dans certaines situations (par exemple pour la réalisation d’une régression logistique ordinale), on peut avoir avoir besoin d’indiquer que les modalités du 
# facteur sont ordonnées héarchiquement. Dans ce cas là, on aura simplement recours à ordered() pour créer/convertir notre facteur.

c("supérieur", "primaire", "secondaire", "primaire", "supérieur") |> 
  ordered(levels = c("primaire", "secondaire", "supérieur"))

class(f)
typeof(f)
as.integer(f)
as.character(f)

# Changer l’ordre des modalités
library(tidyverse)
data("hdv2003", package = "questionr")

# Affichier les modalités d'une variables
hdv2003$qualif |> 
  levels()

# Avoir les fréquences
hdv2003$qualif |> 
  questionr::freq()

# Inverser simplement d'ordre
hdv2003$qualif |> 
  fct_rev() |> 
  questionr::freq()

# On peut également seulement indiquer les premières modalités, les autres seront ajoutées à la fin sans changer leur ordre.
hdv2003$qualif |> 
  fct_relevel("Cadre", "Autre", "Technicien", "Employe") |> 
  questionr::freq()

# La fonction forcats::fct_infreq() ordonne les modalités de celle la plus fréquente à celle la moins fréquente (nombre d’observations) :
hdv2003$qualif |> 
  fct_infreq() |> 
  questionr::freq()

# Pour inverser l’ordre, on combinera forcats::fct_infreq() avec forcats::fct_rev().
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
# Pour modifier le nom des modalités, on pourra avoir recours à forcats::fct_recode() avec une syntaxe de la forme "nouveau nom" = "ancien nom".
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

# Pour transformer les valeurs manquantes (NA) en une modalité explicite, on pourra avoir recours à forcats::fct_explicit_na().
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







