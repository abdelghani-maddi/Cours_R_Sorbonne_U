#Emma La Rosa - 191224 - Partiel R

#Preparation du fichier  -----
rm(list=ls()) 

# Packages 
library(tidyverse)
library(questionr)
library(openxlsx)
library(readxl)
library(labelled)
library(esquisse)
library(dbplyr)
library(gtsummary)
library(broom.helpers)
library(ggplot2)
library(explor)
library(ade4)

#Partie 1 : exploration et preparation des donnees ----


#1. Chargement et aperçu des donnees

data("happy")
#On constate 51020 observations pour 10 variables.


#2. Structure et types des variables

describe(happy)
#variables numeric : id , year , age , wtssall
#variables factor : sex , marital , degree , finrela , health

#modalites : 
# happy : not too happy , pretty happy , very happy
# degree : lt high school , high school , junior college , bachelor , graduate
# health : poor , fair , good , excellent


#3. Donnees manquantes
describe(happy)

#variables avec des NA : happy (9.2%) , age(0.4%), marital(>0%), degree(0.3%), 
#finrela(9.6%), health (24.6%)


#4. Creation de variables
# Recodage de happy$age en happy$age_group
happy$age_group <- cut(happy$age,
                       include.lowest = TRUE,
                       right = FALSE,
                       dig.lab = 4,
                       breaks = c(18, 29, 49, 64, 89),
                       labels=c("Jeunes adultes","Adultes","Seniors","Âges")
)

# Recodage de happy$happy en happy$happyness_score
happy$happyness_score <- happy$happy %>%
  fct_recode(
    "1" = "not too happy",
    "2" = "pretty happy",
    "3" = "very happy"
  ) %>%
  as.character() %>%
  as.numeric()


#Partie 2 - Analyse descriptive ----

#5. Resume statistique

summary(happy$age)
#    Min. 1st Qu.  Median   Mean   3rd Qu.  Max.    NA's 
#  18.00   31.00   43.00   45.43   58.00   89.00     184 

table(happy$degree)
#it high school    high school   junior college    bachelor       graduate 
#  11777             26307           2601           6918           3253 

#on remarque que environ 3/4 des repondants on un niveau scolaire soit inferieur au lycee
#soit egal au lycee.

table(happy$age_group)
#Jeunes adultes        Adultes        Seniors           Âges 
#   9993                20663          10818           9362 

#on constate qu'un peu plus d'un tier des repondants sont adultes (20 663) et 1/5 des
#repondants sont seniors,. La tranche d'age majoritaire est donc les 30-49 ans.


# 6. Croisement et visualisations
happy%>%
  tbl_summary(
    include=c("happy","sex"),
    by = sex) %>%
  add_overall()  

#on constate de façon generale que la moitie des individus (hommes et femmes confondus) se
#jugent "pretty happy", c'est-a-dire ni heureux, ni pas heureux. Pour autant les femmes
#se jugent  plus heureuses que les hommes bien que la difference ne soit pas 
#impressionnate. De plus cette differences peut aussi s'expliquer par le fait qu'il y ai
#plus de femmes dans la base de donnees (cela se voit surtout au niveau du graphique).

ggplot(happy) +
  aes(x = happy, fill = happy) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(title = "Le fait d'être heureux ou non selon le sexe") +
  theme_minimal() +
  facet_wrap(vars(sex))


#Partie 3 - Comparaisons et interpretations ----

#7. Impact du statut matrimonial sur le bonheur

happy%>%
  tbl_summary(
    include=c("happy","marital"),
    by = marital) %>%
  add_overall() 

#on constate que les personnes mariees se jugent plus heureuse que les
#personnes qui ne se sont pas maries ou alors qui sont divorces.
#ex : 40% des individus qui se declarent tres heureuses sont mariees contre
#19% des gens qui sont divorces.

ggplot(happy) +
  aes(x = happy, fill = marital) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(
    title = "Niveau de bonheur selon le statut matrimonial"
  ) +
  theme_minimal()


#8. Sante et bonheur

happy%>%
  tbl_summary(
    include=c("happy","health"),
    by = health) %>%
  add_overall() 

#On constate que 61% des personnes repondants etre plutôt heureux declare avoir une sante bonne.
#46% des indiivdus se declarant tres heureux declarent avoir une excellente sante
#35% des personnes ayant repondant ne pas etre heureux declarent avoir une sante faible.
#on en conclu que plus les personnes se declarant etre plutôt heureux voir tres heureux, ont
#s'estime etre en bonne sante voir en excellente sante.


#Partie 4 - Analayse des correspondances multiples (ACM) ----

#9. Preparation pour l'ACM

d<-happy%>%
  select(happy,marital,degree,health,finrela)

## Recodage de d$finrela en d$finrela_status
d$finrela_status <- d$finrela %>%
  fct_recode(
    "low" = "far below average",
    "low" = "below average",
    "high" = "above average",
    "high" = "far above average"
  )

d<-d%>%
  select(happy,marital,degree,health,finrela_status)

#10. Realisation de l'ACM

acm<-dudi.acm(d, scannf = FALSE, nf = Inf)
screeplot(acm)

explor::explor(acm)
#les axes les avec une inertie importante sont le 1 (14.4%) et le 2 (8%).

#le status financier bas, le niveau de diplôme highschool et avoir un
# niveau de bonheur bas contribuent en majorite a l'axe 1.

#le niveau de bonheur haut contibue en majorite a l'axe 2.


#11. Visualisation des resultats

res <- explor::prepare_results(acm)
explor::MCA_var_plot(res, xax = 1, yax = 2, var_sup = FALSE, var_sup_choice = ,
                     var_lab_min_contrib = 0, col_var = "Variable", symbol_var = NULL, size_var = NULL,
                     size_range = c(10, 300), labels_size = 10, point_size = 56, transitions = TRUE,
                     labels_positions = NULL, labels_prepend_var = FALSE)

#on observe des regroupements : 

# Le premier est en bas a gauche : statut financier haut, un diplome du superieur,
#un niveau de bonheur haut, etre marie.

# Le second est en bas a droite : statut financier bas, une sante mauvaise,
#un niveau de diplôme lycee, etre separe ou pas marie et u niveau de bonheur plutot bas

# Un troisieme profil se dessine en haut : etre divorce, etre plus ou moins heureux, une sante moyenne,
#jamais marie...

#Le premier profil renvoie au niveau de bonheur haut
#Le second profil renvoie au niveau de bonheur plutot bas
# Le troisieme profil renvoie a un niveau de bonheur plutot moyen

#Partie 5 - Synthese et recommandations ----

#Les principaux facteurs associes au niveau de bonheur very happy sont le fait d'etre marie, d'etre
#diplome du superieur, une tres bonne sante et un statut financier plutot haut.

#Mes recommandations pour ameliorer son bonheur : 

# - Prendre soin de sa sante : on a constate qu'une sante peu entretenue engage un niveau
#   de bonheur plus bas. Par consequent une mauvaise sante peut impacter le moral des
#   des individus, d'où l'importance d'en prendre soin.

# - Le statut matrimonial : oui etre marie favorise le fait d'etre heureux, pour antant recommander
#   le mariage comme facteur de bonheur n'est pas l'ideal. Pour autant on peut encourager les individus
#   a faire des rencontres ne serait-ce qu'amicales.Faites des rencontres, engagez le contacte humain 
#   augmente le niveau de bonheur. 

# - Le diplome et le statut financier: le diplome peut etre lie au niveau financier, l'argent fait-il le bonheur ?
#   Disons qu'il y contribue. 
