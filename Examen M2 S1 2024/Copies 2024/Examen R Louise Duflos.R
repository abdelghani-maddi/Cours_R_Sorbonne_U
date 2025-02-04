install.packages("codebook")
install.packages("DT")
install.packages("readxl")
install.packages("dplyr")
install.packages("esquisse")
install.packages("gtsummary")
install.packages("glue")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("questionr")
install.packages("openxlsx")
install.packages("labelled")
install.packages("funModeling")
install.packages("ade4")
install.packages("factoextra")
library(funModeling)
library(factoextra)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(questionr)
library(openxlsx)
library(labelled)
library(codebook)
library(DT)
library(readxl)
library(esquisse)
library(gtsummary)

#Partie 1------


data("happy")

#1. -----
d <- happy
head(d, 10)

# On a 51020 observations et 10 variables

#2.-----


str(d)

# l'âge est numérique, le degree est une donnée factorielle, l'année 
#est numérique, le sexe est factoriel, firela est factoriel, health factoriel
# wtsall fnumérique, happy est factoriel

levels(d$happy)
#on a "not too happy" "pretty happy"  "very happy"
levels(d$degree)
#on a "lt high school" "high school"    "junior college"
# "bachelor"       "graduate"    
levels(d$health)
#on a "poor"      "fair"      "good"      "excellent"

#3 ----

summary(d)
#on a des valeurs manquantes dans les variables happy, age, marital, degree
# et finrela. 

df_status(d)

#pour mes variables happy, on a 9.25%
# pour la variblae age on a 0.36%
# pour degree on a 9.56%
#pour health on a 24.56%

#4. -----
#regrouper les âges 

## Cutting d$age into d$age_rec
d$age_group <- cut(d$age,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(18, 30, 50, 65, 100)
)


## Recoding d$happy into d$happiness_score
d$happiness_score <- d$happy %>%
  fct_recode(
    "1" = "not too happy",
    "2" = "pretty happy",
    "3" = "very happy"
  )

# Partie 2 -----
#5 ------

summary(d$age, na.rm=TRUE)
#
#moyenne : 45.43, médiane : 43, premier quartile 31, troisième quartile
#58 

sd(d$age, na.rm = TRUE)

#l'âge type est 17,44 sans les non réponses. 

#6------


library(ggplot2)

ggplot(d) +
 aes(x = degree, fill = age_group) +
 geom_bar(position = "dodge") +
 scale_fill_brewer(palette = "RdYlBu", 
 direction = 1) +
 labs(x = "diplôme", y = "effectif", title = "répartition des diplomes des individus au regard de leur âge") +
 theme_minimal()

table(d$degree, d$age_group)
table(d$age_group)

#On voit selon ces deux informations que les 30-50 sont sur représentés dans
# toutes les catégories car ils sont sur les plus nombreux. 
#

#7------


d %>% 
  tbl_summary(include = c(happy),
              by = sex) %>% add_overall()

#selon ce tableau croisé nous voyons qu'il n'y a pas de différence 
# significative entre les groupes, les femmes et les hommes sont 
# glovalement aussi herueux avec une légère différence marquée pour les femmes
# quii sont un point plus heureuses que les hommes. mais qui sont deux
# points moins heureuses que les hommes dans la catégorie véry happy. 
# On observe un taux de non réponse plus élévé chez les femmes que chez les hommes


library(ggplot2)

d1 <- na.omit(d)
#j'ai enlevé toutes les non réponses 


library(ggplot2)

ggplot(d1) +
 aes(x = sex, fill = happy) +
 geom_bar(position = "dodge") +
 scale_fill_viridis_d(option = "inferno", 
 direction = 1) +
 labs(x = "sexe", y = "effectif", title = "Bonheur selon le sexe ") +
 theme_minimal()

#On voit donc que les femmes sont globalement plus heureuses que les hommes
# quand on enlève les non réponses. 
d1 %>%
  tbl_cross(
    row = sex,
    col = happy,
    percent = "row"
  ) %>%
  add_p(source_note = TRUE)

#Partie 3 --------


#7 --------


d1 %>%
  tbl_cross(
    row = marital,
    col = happy,
    percent = "row"
  ) %>%
  add_p(source_note = TRUE)

#Il est clair que le statut marital joue un rôle très important
# sur le niveau de bonheur.
# Les marriés sont largement les plus heureux avec 70% d'entre eux very 
# happy, les séparés sont les plus malheureux suivis par les veufs 
# puis les divorcés. 
#Cela rejoint les travaux de durkheim et le fait de ne pas se sentir
# en situation d'exclusion sociale. ne pas être marié, peut favoriser 
# un sentiment d'exclusion. 

d_marital <- d1 %>% group_by(marital, happy) %>%
  summarise(nbr = n()) %>% mutate(part = nbr / sum(nbr) * 100)

ggplot(d_marital) + aes(x = marital, y = part, fill = happy) + 
  geom_col(position = "dodge2") + theme_minimal

# Nous voyons ici qu'en proportion ce sont les marriés sont les 
#plus heureux avec environ 41% puis ensuite les veufs, les jamais mariés
# les pourcentages permettend donc de rendre l'ensemble de ces chiffre 
# plus clairs



#8 _______
d1 %>% tbl_cross(
  row = health,
  col = happy,
  percent = "row"
) %>%
  add_p(source_note = TRUE)


# plus on est en bonne santé, plus on est heureux. Ceux qui 
# ont une faible santé sont à 18% heureux et ceux qui en ont une bonne
# sont à 46% heureux


# Partie 4 ------
#9-----

install.packages("ade4", dep = TRUE)
library(ade4)
d3 <- d1 %>% 
  select(happy, marital, degree, health, finrela)

# 10 -------


## Recoding d3$finrela into d3$finrela_rec
d3$finrela_status <- d3$finrela %>%
  fct_recode(
    "low" = "far below average",
    "low" = "below average",
    "high" = "above average",
    "high" = "far above average"
  )

d4 <- d3 %>% 
  select(happy, marital, degree, health, finrela_status)

acm<- dudi.acm(d4, scannf=FALSE, nf=FALSE)
explor::explor(acm)


#11------

# voici les ACM sur le plan factoriel
fviz_mca_var(acm)
fviz_mca_var(acm, repel = TRUE)
boxplot(acm)
fviz_contrib(acm, choice = "var", axes = 1)
fviz_contrib(acm, choice = "var", axes = 2) 
#les modalités qui sont dpnc importantes sont les premières
#et les plus importantes sont degree high school, nous vooyons que 
# la ligne rouge indiqiue la contrivution attendue. 
# les variables au dessus sont donc plus importante et les variables en desssous
# ont une faible contribution. marital never married a donc peu d'importance
# alors que fonrela status high si pour la dimension 1
# pour la dimension 2, ce sont la bonne santé, )etre treèsheureux ou un peu heureux
# pour les variables avec le moins d'importance ce sont le statut marital séparé 
# et la situation financière basse

# sur le planc factoriel, nous voyons health poor qui se dégage
# et sinon il y a des petits groupes too happy et le niceau d'étude high
#school sont près, ainsi que le statu économique low. 
#Il en est de m^meme entre pretty happy et divorcé et la santé good. 
#le statut éco high et la très bonne santé sont collées ainsi que le 
#haut niveau d'études : bref il semble que ces variables soient liées
# et qye plus quelqu'un a tous ses facteurs c'est à dire very happy, 
# bonne santé et bon niveau d'études alors il sera très heureux.

# PArtie 5 synthèse et recommendation------

#les principaux facteur associés au niveau de bonheur sont 
# 1. le statut marital 
# 2. l'état de santé. 
# le sexe semble jouer un rôle de second plan
# il faudrait également utiliser les ACM : nous voyons avec les analyses factorielles
# que le bonheur est favorisé par 
# 1/ une bonne santé 
# 2/ un haut niveau éco 
# 3/ être marié
# 4/ et des bonnes études

# en tant que recommendation, il semblerait que l'isolement social joue un 
#r^pme clé dans le niveau de bonheur, la santé également. 
#je proposerai donc de favoriser les systèmes de santé en les aidant financièrement
#selon l'acm, l'école semble égalkement jouer un rôle. Il faudrait donc favotriser
# une école davantage juste et et accessible. 
