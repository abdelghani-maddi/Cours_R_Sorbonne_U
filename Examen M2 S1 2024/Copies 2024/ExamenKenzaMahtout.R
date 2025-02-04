library(tidyverse)
library(questionr)
library(openxlsx)
library(readxl)
library(labelled)
library(codebook)
library(DT)
library(tidyverse)
library(questionr)
library(gtsummary)
library(labelled)
#Partie 1: 1
data("happy")
#Le jeu de données contient 51020 observations et 10 variables 

#2
describe(happy$age)
describe(happy$degree)
describe(happy$finrela)
describe(happy$happy)
describe(happy$health)
describe(happy$marital)
describe(happy$sex)
describe(happy$wtssall)
describe(happy$year)

#afficher les modalités
install.packages("forcats")
library(forcats)
data(gss_cat)
levels(happy$happy)
levels(happy$degree)
levels(happy$health)

#3
#variables contenant des valeurs manquantes 
summary(happy)
#les variables avec des valeurs manquantes sont : "happy","age","marital","degree","finrela" et "health"

#Calcul % de valeurs manquantes pour chaque vairbale 
sum(is.na(happy$happy))/length(happy$happy)*100
sum(is.na(happy$age))/length(happy$age)*100
sum(is.na(happy$marital))/length(happy$marital)*100
sum(is.na(happy$degree))/length(happy$degree)*100
sum(is.na(happy$finrela))/length(happy$finrela)*100
sum(is.na(happy$health))/length(happy$health)*100

#4 
#Création de variable
## Cutting happy$age into happy$age_group
happy$age_group <- cut(happy$age,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(18, 30, 49, 64, 100)
)
## Recoding happy$age_group into happy$age_group
happy$age_group<- happy$age_group %>%
  fct_recode(
    "Jeunes adultes" = "[18,29)",
    "Adultes" = "[30,49)",
    "Seniors" = "[50,64)",
    "Agés" = "[65,100]"
  )

## Recoding happy$happy into happy$happiness_score
happy$happiness_score <- happy$happy %>%
  fct_recode(
    "3" = "not too happy",
    "2" = "pretty happy",
    "1" = "very happy"
  )

#Partie2

#5
summary(age)

#6
table(happy$happy,happy$sex)

library(ggplot2)

ggplot(happy) +
 aes(x = happy, fill = sex) +
 geom_bar() +
 scale_fill_hue(direction = 1) +
 theme_minimal()
#les resultats indiquent que les femmes ont tendance a avoir un niveau de bonheur plus élevées que les hommes 
# que ce soit pour la modalité "tres heureux" ou "assez heureux"

#Partie 3:

#7

table(happy$marital,happy$happy)
library(ggplot2)

ggplot(happy) +
 aes(x = marital, fill = happiness_score) +
 geom_bar() +
 scale_fill_hue(direction = 1) +
 theme_minimal()
#Le graphique nous montre que le niveau de bonheur depend du statut marital, on 
#peut voir que les personnes mariées ont un niveau de bonheur beaucoup plus élevé que les personnes qui n'ont jamais ete mariées
#ou qui sont divorcés. 

#8

happy_health <- happy %>%
  select(id, happy, health) %>%   
  group_by(happy,health) %>%      
  summarise(nbr = n()) %>%                     
  mutate(part = nbr / sum(nbr) * 100) 
#Les resultats ne sont pas vraiment représentatifs, car le nombre de NA dans la categorie health est assez elevé
#mzis nous pouvons constaté que les individus qui se considèrent en bonne santé, se considèrent assez heureux
#cela s'explique surement par le fait que l'absence de problème de santé engendre moins de problèmes et donc plus de bonheur

#Partie 4:

#9

install.packages("ade4", dep = TRUE)
library(ade4)
# Selection des variables d'intérêt
d2 <- happy %>%
  select(happy,marital,degree,health,finrela)
## Recoding happy$finrela into happy$financial_status
happy$financial_status <- happy$finrela %>%
  fct_recode(
    "low" = "far below average",
    "low" = "below average",
    "high" = "above average",
    "high" = "far above average"
  )
explor::explor(acm)
