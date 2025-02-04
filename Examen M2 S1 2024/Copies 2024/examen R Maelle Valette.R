#Chargement des packages----

install.packages("tidyverse")
install.packages("questionr")
install.packages("openxlsx")
install.packages("labelled")
install.packages("codebook")
install.packages("DT")
install.packages("readxl")
install.packages("esquisse")
install.packages("gtsummary")
install.packages("glue")
install.packages("ade4", dep = TRUE)
install.packages("factoextra")


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
library(ade4)
library(factoextra)

#PARTIE 1 -------
###Chargement et aperçu des données ----
data("happy")
d<- happy
describe(d)
view(d)
## Recodage de d$age_group
d$age_group <- d$age_group %>%
  fct_recode(
    "Jeune adulte" = "[18,29)",
    "Adulte" = "[29,49)",
    "Senior" = "[49,64)",
    "Âgé" = "[64,89]"
  )#Il y a 51020 observations et 10 variables dans la base "happy"

###Structure et type des variables ----
class(d$age)
class(d$degree)
class(d$finrela)
class(d$happy)
class(d$health)
class(d$marital)
class(d$sex)
class(d$wtsall)
class(d$year)
#Toutes les variables sont de type "factor" sauf age et year qui sont de type "numeric" et wtsall qui est de type "NULL"

describe(d$happy)
#3 modalités: not too happy | pretty happy | very happy
describe(d$degree)
#5 modalités: lt high school | high school | junior college | bachelor | graduate
describe(d$health)
#4 modalités: poor | fair | good | excellent

###Données manquantes
summary(d$age)
#184 NA
summary(d$degree)
#164 NA
summary(d$finrela)
#4876 NA
summary(d$happy)
#4717 NA
summary(d$health)
#12529 NA
summary(d$marital)
#14 NA
summary(d$sex)
#Pas de NA
summary(d$wtsall)
#Pas de NA
summary(d$year)
#Pas de NA

sum(is.na(d$age))/length(d$age)*100
#0.36% de NA
sum(is.na(d$degree))/length(d$degree)*100
#0.32% de NA
sum(is.na(d$finrela))/length(d$finrela)*100
#9.55% de NA
sum(is.na(d$happy))/length(d$happy)*100
#9.24% de NA
sum(is.na(d$health))/length(d$health)*100
#24.55% de NA
sum(is.na(d$marital))/length(d$marital)*100
#0.02% de NA

###Création de variables ----

d$age_group <- cut(d$age,
                 include.lowest = TRUE,
                 right = FALSE,
                 dig.lab = 4,
                 breaks = c(18, 29, 49, 64, 89)
)
d$age_group <- d$age_group %>%
  fct_recode(
    "Jeune adulte" = "[18,29)",
    "Adulte" = "[29,49)",
    "Senior" = "[49,64)",
    "Âgé" = "[64,89]"
  )

## Recodage de d$happy en d$happiness_score
d$happiness_score <- d$happy %>%
  fct_recode(
    "1" = "not too happy",
    "2" = "pretty happy",
    "3" = "very happy"
  )


#PARTIE 2----

###Résumé statistique----
mean(d$age,na.rm = T)
#Moyenne=45.43058 ans
median(d$age, na.rm=T)
#Médiane=43 ans
var(d$age, na.rm=T)	
#Variance=304.19
sd(d$age, na.rm=T)
#Ecart-type de 17 ans
min(d$age, na.rm=T)
#Plus jeune=18 ans
max(d$age, na.rm=T)
#Plus vieux=89 ans

t1<-table(d$age_group, d$degree)
lprop(t1)
#Ce tableau croisé nous permet de constater plusieurs choses, comme le fait que la majorité des 
#répondants, mais surtout des jeunes adultes, ont le baccélauréat mais pas encore de diplôme supérieur, tandis que 
#parmi les seniors et les plus âgés, une grande partie se sont arrêtés avant le lycée. L'éducation semble se démocratiser
#avec le temps, les plus jeunes y ayant plus accès.

###Croisement et visualisations----

t2<-table(d$sex, d$happy)
lprop(t2)
#Ce tableau croisé permet de constater que les hommes et les femmes sont similairement heureux, 
#mais que les femmes tendent à déclarer plus de sentiments forts (soit négatifs, soit positifs) que 
#les hommes qui répondent souvent de manière neutre.

data_summary <- d %>%
  group_by(sex, happy) %>%
  summarise(count = n(), .groups = "drop")

ggplot(data_summary, aes(x = sex, y = count, fill = happy)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Distribution croisée entre sexe et bonheur",
    x = "Sexe",
    y = "Fréquence",
    fill = "Bonheur"
  ) +
  theme_minimal()           

#PARTIE 3----

###Impact statut matrimonial et bonheur----
t3<- d %>%
  tbl_summary(
    include = c("happy"),
    by="marital"
  ) %>%
  add_overall(last=TRUE)

view(t3)

#On constate que parmi les personnes mariées, la majorité se déclarent "pretty happy" ou "very happy"
#Chez les jamais mariés, ils se déclarent surtout "pretty happy"
#Les divorcés sont majoritairement "not too happy" et "pretty happy" par rapport aux autres.
#Les veufs sont similaires aux divorcés quoiqu'ayant moins de chances d'être "very happy".
#Les séparés sont la majorité des "not too happy" mais ont plus de chances d'être "pretty happy".

data_summary <- d %>%
  group_by(marital, happy) %>%
  summarise(count = n(), .groups = "drop")

ggplot(data_summary, aes(x = marital, y = count, fill = happy)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Distribution croisée entre sexe et bonheur",
    x = "Statut marital",
    y = "Fréquence",
    fill = "Bonheur"
  ) +
  theme_minimal()           

###Santé et bonheur----

t4<-table(d$happy, d$health)
lprop(t4)
#               poor  fair  good  excellent Total
#not too happy  15.7  31.2  36.7  16.5     100.0
#pretty happy    4.8  19.5  49.2  26.5     100.0
#very happy      3.1  12.2  39.8  44.9     100.0
#Ensemble        5.6  18.6  44.6  31.1     100.0

#On constate via ce tableau plusieurs tendances. Tout d'abord, plus les répondants sont "very happy"
#plus ils déclairent être en excellente santé. Ceux qui sont "pretty happy", soit neutres sur leur bonheur, 
#sont aussi en bonne santé. Enfin, ceux qui sont "not too happy" sont en majorité en bonne ou moyennement bonne
#santé. On constate alors que plus on est heureux, plus on est en bonne santé, mais être malheureux n'implique pas
#forcément la bonne santé, au contraire. Le bonheur augmente les chances d'être bien portant, ou l'inverse : être en 
#bonne santé augmente les chances d'être heureux. Une forte corrélation apparait dans les deux cas.

#PARTIE 4----

###Préparation pour l'ACM----
d2 <- d[, c("happy", "marital", "degree", "health", "finrela")]

d2$financial_status <- d2$finrela %>%
  fct_recode(
    "low" = "far below average",
    "low" = "below average",
    "high" = "above average",
    "high" = "far above average"
  )

d3 <- d2[, c("happy", "marital", "degree", "health", "financial_status")]


###Réalisation de l'ACM----
acm <- dudi.acm(d3, scannf = FALSE, nf = 5)
summary(acm)
fviz_screeplot(acm, choice = "eigenvalue")

#Les deux dimensions principales sont celles du mariage et celles du statut financier
#Le mariage est l'axe y, de bas (veufs) en haut (mariés ou jamais mariés) et le statut financier est l'axe x, 
#de gauche (excellente statut) à droite (mauvaise statut).
#Contribuant le plus au bonheur au sein du statut matrimonial sont les modalités de bonne santé, du high school degree,
#ou du statut financier moyen
#Contribuant le plus au bonheur au sein du statut financier sont les modalités de la bonne santé, du haut degré supérieur,
#et du statut d'être divorcé ou jamais marié.

###Visualisation des résultats----

s.corcircle(acm$co, 1, 2, clabel = 0.7)
s.label(acm$co, clabel = 0.7)

#On remarque un regroupement entre les modalités de la bonne santé et du fait de ne pas ou plus être marié
#Ainsi qu'entre le fait d'être marié et en excellente santé avec un haut statut financier.
#Enfin, le fait d'être en mauvaise santé se corelle avec le fait d'être veuf et de ne pas avoir fini le lycée.

#PARTIE 5----

#On est le plus heureux lorsqu'on est marié ou alors qu'on ne l'a jamais été 
# (donc pas de divorce ni de séparation ni de veuvage)
#On est aussi heureux avec un haut statut financier et une excellente santé
#Et lorsqu'on a fini ses études avec un diplôme ou au moins une licence.

#Pour être heureux il convient donc d'éviter les séparations d'avec son conjoint 
# ou de privilégier de ne jamais se marier.
#Il est aussi bon de finir ses études, sans s'arrêter avant le lycée, afin de 
# s'assurer un bon avenir financier, ce qui aide le bonheur.
#Enfin, et cela est aidé par le niveau financier, il est conseillé de prendre grand soin de sa santé.

