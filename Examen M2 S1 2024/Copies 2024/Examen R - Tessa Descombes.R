
#Partie 1 ----

#Q1. Chargement et aperçu des données----

# enleve tous les objets du workspace
rm(list = ls())

# IMPORT, PREPARATION ET EXPLORATION DES DONNEES

# Packages
library(tidyverse)
library(questionr)
library(openxlsx)
library(labelled)
library(codebook)
library(DT)
library(readxl)
library(esquisse)
library(gtsummary)
library(glue)

#install package
install.packages("tidyverse")
install.packages("questionr")
install.packages("openxlsx")
install.packages("labelled")
install.packages("codebook")
install.packages("DT")
install.packages(“readxl”)
install.packages(“esquisse”)
install.packages(“gtsummary”)
install.packages(“glue”)

# Importer/Exporter les données
data("happy")

# Explorationgénéraledelabasededonnées
str(happy)
dim(happy)
colnames(happy)
head(happy)
summary(happy)
describe(happy)
glimpse(happy)

#1.a. Afficher les 1à première lignes ---- 
top_10lignes <- happy %>%
  slice (1:10)

#1.b. Le jeu de données contient 51020 abservation et 10 variables----


#Q2. Structures et type des varaibles

#1.a. identifiez les types de chaque variables 
str(happy)

# id    : num  
#happy  : txt (Factor)
#year   : num  
#age    : num  
#sex    : txt(Factor)
#marital: txt (Factor)
#degree : txt (Factor)
#finrela: txt (Factor) 
#health : txt (Factor)
#wtssall: num 

#1.b. Lister les modalités pour les varaibles suivantes : happy, degree, et health.

#happy : 3 modalités : "very happy", "pretty happy", "not to happy"
#degree : 5 modalités : "lt higth school", "hight shcool", "junior college", "bachelor", "graduate"
#health : 4 modalités : "excellent", "good", "fair", "poor"


# Q3. données manquantes

#Q3.a. Identifier les variables contenants des données manquantes

is.na(happy)

# il y a des données manquantes dans la variable "finrela"

#Q3.b. calculer pourcentage de valeurs manquantes pour chaque varaibles


x1<-sample(c(NA, 1, 2,3,4,5,6,7,8,9,10),,replace=TRUE)
x2<-sample(c(NA, 1,2),20,replace=TRUE)
x3<-sample(c(NA,),20,replace=TRUE)
df1<-data.frame(x1,x2,x3)
df1

#Q4 création de variables ----

#Q4.a. créer une vraibale age_group qui regroupe les idniv en catégories d'âge ----


happy$age_group <- cut(happy$age,
                       include.lowest = TRUE,
                       right = FALSE,
                       dig.lab = 4,
                       breaks = c(18, 29.5, 49.5, 64.5, 97)
)

## Recodage de happy$age_group en happy$age_group_rec
happy$age_group_rec <- happy$age_group %>%
  fct_recode(
    "Jeunes adultes" = "[18,29.5)",
    "Adultes" = "[29.5,49.5)",
    "Seniors" = "[49.5,64.5)",
    "Agés" = "[64.5,97]"
  )

#Q.b. Ajouter une var "happiness_score" attribuant un score numérique à la var "happy"----


## Recodage de happy$happy en happy$happiness_score
happy$happiness_score <- happy$happy %>%
  fct_recode(
    "1" = "not too happy",## Recodage de happy$sex en happy$sex_rec
happy$sex_rec <- happy$sex %>%
  fct_recode(
    "1" = "male",
    "2" = "female"
  )
    "2" = "pretty happy",
    "3" = "very happy"
  )


#Partrie 2 : Analyse descriptive----

#Q5.résumé stat descriptives----

#Q5.a. Afficher stat descriptives
summary(happy$age)
describe(happy$age)
glimpse(happy)
look_for(happy, "happy")
describe(happy, "happy")


#Q5.b. Analyser la répartition des répondant selon "degree" et "age_grop"

#répartition selon "degree"

nb_ind_occup <- happy %>%
  group_by(degree) %>%
  count()
nb_ind_occup

#résultats : 
#lt high school 11777
#high school    26307
#junior college  2601
#bachelor        6918
#graduate        3253
#NA               164

#--> on constae que la population la plus représenter dans cet échantillon est faite des individus diplomé du niveau lycée.Et la moins du niveau collège (junior collège). 

#répartition selon "happy"
nb_ind_occup <- happy %>%
  group_by(age_group_rec) %>%
  count()
nb_ind_occup

#résultats : 
#Jeunes adultes 11066
#Adultes        20472
#Seniors        10492
#Agés            8806
#NA               184

# --> on constate que la population la plus représenter dans cet échantillon est faite des individus dit adultes ( de 30 à 49 ans). Et la moins des personne plus âgé (à partir de 65 ans) (junior collège). 

# --> On peut faire un lien avec la presene forte d'adultes dans l'échantillon et le niveau d'étude le plus représenté, c'est à dire niveau lycée. Les adultes comparé à leur ainé on fait plus d'études mais moins élevé que les plus jeune, passant potentiellement moins par la case enseignement supérieur.


#Q6. Croisement et visualisations

#Q6. a. relation entre happy et sex
table(happy$happy,happy$sex)

#-> Les femmes indique être plus heureux que les hommes (avec 8422 femmes "very happy" contre 6378 hommes)
# Les femmes sont plus présente dans chaques possibiolités
#cela peu etredu au fait qu'il y a plus de femme que d'hommes dans cet études

nb_ind_occup <- happy %>%
  group_by(sex) %>%
  count()
nb_ind_occup

#Q6.b. rpz graphiquement


happy$sex_rec <- happy$sex %>%
  fct_recode(
    "1" = "male",
    "2" = "female"
  )

hist(happiness_score$sex)

#Partie 3 ----
#Q7. 
#a. Analyser la distribution de happy selon marital


#b. representation visuelle
ggplot(happy) +
  aes(x = happy, y = marital) +
  geom_violin(fill = "#FFBBDD") +
  labs(x="happy",y="marital",
       title = "distribution de l'age selon les modalités de marital",
       caption = "barbapapa") +
  theme_gray()

#on constate une constante entre la différence de réponse selon les modalité marital et les réponses de la variabale happy. 

#8
#a.
happyplus <-happy %>%
  group_by(health)
  
  

#Partie 4----

#Q9. préparation ACM

#a.selec variables 
var_acm <- happy %>%
  select(happy, marital, degree, health, finrela) %>%   
  distinct() %>%                 
  summarise(nbr = n()) %>%                        
  mutate(part = nbr/sum(nbr) * 100)


#b. créer var 
## Recodage de happy$finrela en happy$`financial statut`

happy$`financial statut` <- happy$finrela %>%
  fct_recode(
    "low" = "far below average",
    "low" = "below average",
    "high" = "above average",
    "high" = "far above average"
  )

#Q10
#a. 


#Patie 5 ---- 
# a. Le fait d'être marrié influ sur le fait d'être heureux "happy" --> le fait 
# d'être marrié fait baisser le sentiment d'être heureux, notament chez les femmes.
# #B. faire une meilleur répartiotion des taches domestique dans les couples, 
# notament les couples mariés, pour baisser la charge mentale des femmes notament. 
# Les femmes ciblibatire sont plus heureuse.






      




