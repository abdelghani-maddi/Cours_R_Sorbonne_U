
#LEAL LUCIE 

# Informations sur les variables : 
# D'abord on importe, puis la préparation et exploration des données 

rm(list=ls())


#Packages 
library(tidyverse)
library(questionr)
library(openxlsx)
library(readxl)
library(labelled)
library(gtsummary)
library(ggplot2)



data("happy")


#Partie 1 : 
#1) 
head(happy)
top_10lignes <- happy %>%
  slice (1:10)
top_10lignes


str(happy)  # pour un aperçu de la variable 
# La base de données contient 51020 observations et 10 variables 


#2) 
describe(happy, "age", "dregree", "finrela", "happy", "health", "marital", "sex", "wtsall", "year")
# Variables numériques : Year, Age, 
# Variables qualitatives/catégorielles : Happy, Sex, Marital, Finrela, Health 

table(happy$happy) # les modalités pour cette variable sont : not too happy, pretty happy, very happy 
table(happy$degree) # les modalités pour cette variable sont : lt high schol, high school junior, college, bachelor, graduate
table(happy$health) # les modalités pour cette variable sont : poor, fair, good, excellent 


#3) 
summary(happy) # Avec cette fonction je vais pouvoir visualiser chaque variable, avoir un résumé statistique 
# et surtout voir pour chacune des variables le nombre de NAs. 
# Les variables contenant des NAs sont : happy, age, marital, degree, finrela, health 
# Pourcentage de Na pour happy : 
sum(is.na(happy$happy)) 







#4) 
## Cutting happy$age into happy$age_group
happy$age_group <- cut(happy$age,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(18, 30, 50, 65, 89)
)


## Recoding happy$happy into happy$happpiness_score
happy$happiness_score <- happy$happy %>%
  fct_recode(
    "1" = "not too happy",
    "2" = "pretty happy",
    "3" = "very happy"
  )           


#Partie 2 : 
#5)
summary(happy$age) # affichage des statistques descritives de la variable age 
happy_degree_age_group <- happy %>%
  select(id,degree,age_group) %>%
  distinct()%>%
  group_by(age_group,degree) %>%
  count()%>%
  group_by(age_group) %>%
  mutate(part=(n/sum(n)*100))
view(happy_degree_age_group)


#6) 
tableau <- table(happy$happy,happy$sex)
tableau
# En moyenne moyenne, les hommes et les femmes ont plus tendance à répondre "pretty happy".
# On distingue quand même que les femmes ont tendance à etre plus "verry happy" que les hommes. 


#Partie 3 : 
#7) 
happy_marital <- happy %>%
  select(hapiness_score,marital) %>% # il me semble plus judiceux d'utiliser la varibale recoder
  distinct()%>%
  group_by(happy,marital) %>%
  count()%>%
  group_by(marital) %>%
  mutate(part=(n/sum(n)*100))
view(happy_marital) 
# ne donne rien de très concluant, donc il peut-etre préférable de faire un tableau croisée 

tableau2 <- table(happy$happiness_score,happy$marital)  #on utilise toujours la variable recodée
tableau2 
# On remarque que généralement les personnes les plus heureuses sont les personnes mariées 
# Avec un taux de réponse important pour les modalité verry happy =3 et pretty happy= 2 
# Toutefois, on constate également que certaines personnes mariées ne sont pas heureuse 
# et que ceux qui ne se sont jamais marié arrivent en 2ème position pour ceux qui ne sont pas trop heureux

# maintenant on fait un graphique 



