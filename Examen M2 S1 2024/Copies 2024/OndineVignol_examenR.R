# EXAMEN R -----

# Importation bibliotheques ----
library(tidyverse)
library(questionr)
library(openxlsx)
library(labelled)
library(codebook)
library(DT)
library(esquisse)
library(glue)
library(gtsummary)
library(ade4)
library(explor)
theme_gtsummary_language("fr", decimal.mark = ",", big.mark = " ")

# Chargement des donnees -----
data("happy")

# PARTIE 1 -----

# 1) 
# Le jeu de donnees contient 10 variables pour 51020 observations 
question1 <-happy %>%
  slice (1:10)
view(question1)

#2) 
str(happy)
# Type de chaque variable : 
  # Id : numerique / happy: factor / year : numerique / age : numerique
  # sex : factor / marital : factor / degree : factor / finrela : Factor 
  # health : factor / wtssall : numerique

summary(happy)
# Modalites happy : not too happy / pretty happy / very happy 
# Modalites degree : lt high school / high school / junior college / bachelor / graduate
# Modalites health : poor / fair / good / excellent 


#3) 
any(is.na(happy))
colSums(is.na(happy))
# IL ya des variables manquantes dans les variables : happy, age, marital, degree, 
# finrela et health 

#happy prooprtion 
summary(happy$happy)
4717 *100 / 51020
# 9.245394

# marital rpoportion
summary(happy$marital)
14 *100 / 51020
# 0.02744022

# degree proportion 
summary(happy$degree)
164 *100 / 51020
#  0.3214426

# age proporiton 
summary(happy$age)
184 *100 / 51020
# 0.3606429

# finrela proportion 
summary(happy$finrela)
4876 *100 / 51020
# 9.557036

# health proportion 
summary(happy$health)
12529 *100 / 51020
#24.55704


#4 ) 
## Cutting happy$age into happy$age_group
happy$age_group <- cut(happy$age,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(18, 29, 49, 64, 89)
)

# rajouter variable happiness_score 
## Recoding happy$happy into happy$hapiness_score
happy$hapiness_score <- happy$happy %>%
  fct_recode(
    "1" = "not too happy",
    "2" = "pretty happy",
    "3" = "very happy"
  )

view(happy)


# PARTIE 2 -----

# 5)
summary(happy$age)
happy %>%
  tbl_summary(
    include = c(age_group,degree), 
    by = age_group
  )%>%
  add_overall(last=TRUE) %>%
  add_p() %>%
  separate_p_footnotes()

# relation sifgnitificative entre le groupe d'age et le niveai d'étude : 
# le groupe d'age 65 et plus représente 45% des personnes ayant fait moins que 
# le lycée contre 17% des 18- 29ans 

#6  ) 

question6 <-happy %>%
  tbl_summary(
    include = c(happy, sex), 
    by = sex
  )%>%
  add_overall(last=TRUE) %>%
  add_p() %>%
  separate_p_footnotes()

#Chez les hommes, 12 % se déclarent "pas très heureux", 57 % "assez heureux", et 31 % "très heureux", 
# tandis que chez les femmes, ces proportions sont respectivement de 12 %, 55 %, et 32 %,
# la différence statistiquement significative entre les deux groupes

ggplot(happy) +
  aes(x = sex, fill = sex) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_linedraw() +
  facet_wrap(vars(happy))


# PARTIE 3 ----

# 7) 
question 7 <-summary(happy$age)
happy %>%
  tbl_summary(
    include = c(happy,marital), 
    by = marital
  )%>%
  add_overall(last=TRUE) %>%
  add_p() %>%
  separate_p_footnotes()

ggplot(happy) +
  aes(x = marital, fill = happy) +
  geom_bar() +
  scale_fill_viridis_d(option = "magma", direction = 1) +
  theme_minimal()


# 8) 
table_happy_health <- table(happy$happy, happy$health)
proportions_happy_health <- prop.table(table_happy_health, margin = 2) * 100
print(proportions_happy_health)

# On peut voir que parmi les persone ayants une pauvre santé, 34% d'entes elles ne sont pas très heureuses 
# Parmi les personnes ayant une excellente santé, 46%, d'netre elles sont très heureuses et 47% assez heureuse
# IL semble que le fait d'avoir une bonne santé soit positivement corrélé avec le fait d'être heureux. 


# PARTIE 4 ----
# 9 ) 

# selection variable d'interet 
d<- happy %>%
  select(happy, marital, degree, health, finrela)

# nouvelle variable 
happy$financial_status <- happy$finrela %>%
  fct_recode(
    "low" = "far below average",
    "low" = "below average",
    "average" = "above average",
    "hight" = "far above average"
  )


# 10) 
d1<- happy %>%
  select(happy, marital, degree, health, financial_status)

acm<-dudi.acm(d1, scannf = FALSE, nf = Inf)
explor::explor(acm)

# Axe 1 : santé 
# Modalités uqi contribuent le plus
#poor" (santé perçue comme mauvaise) : Position éloignée à droite, indiquant une forte contribution positive.
#"separated" (statut marital séparé) : Située à droite, contribuant positivement.
#"happy:not too happy" (pas très heureux) : Positionnée également à droite, indiquant une association avec cet axe.
#"bachelor" (niveau d'éducation) : Positionnée à gauche, contribuant négativement.

# Axe 2 :
# Modalités uqi contribuent le plus
#"high" (niveau élevé de revenu financier) : À gauche, indiquant une forte contribution négative.
#"happy:very happy" (très heureux) : Position en bas, avec une forte contribution négative.
#"excellent" (santé perçue comme excellente) : Associée à une contribution négative en bas à gauche.
#"married" (marié) : Contribue négativement, située en bas à gauche.
#"low" (faible revenu financier) : Contribue positivement en haut.



# 11) 
res <- explor::prepare_results(acm)
explor::MCA_var_plot(res, xax = 1, yax = 2, var_sup = FALSE, var_sup_choice = ,
                     var_lab_min_contrib = 0, col_var = "Variable", symbol_var = NULL, size_var = "Cos2",
                     size_range = c(52.5, 700), labels_size = 10, point_size = 56, transitions = TRUE,
                     labels_positions = NULL, labels_prepend_var = FALSE, xlim = c(-1.54, 2.52),
                     ylim = c(-2.32, 1.73))

# PARTIE 5 -----
# Les principaux facteurs associés à un haut niveau de bonheur sont la santé, le sexe, 
#le fait d'êter marié et le revenu. 
# Pour améliorer le bonheur il fausrait rentre plus accessible l'accès
# aux soins et à la santé à plus de personne. 
