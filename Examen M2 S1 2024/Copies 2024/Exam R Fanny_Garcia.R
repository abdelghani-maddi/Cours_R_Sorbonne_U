#EXAMEN R M2 S1#
#Fanny Garcia#

library(tidyverse)
library(questionr)
library(openxlsx)
library(labelled)
library(gtsummary)
library(ade4)
library(explor)
library(gt)
library(officer)
library(flextable)
library(FactoMineR)
library(factoextra)

data("happy")
_______________________________________________________________________________
#question 1#
top_10lignes<-happy%>%
  slice(1:10)
view(top_10lignes)

summary(happy) 
describe(happy)

#le jeu de donnée contient 51020 obersvations et 10 variables#
_______________________________________________________________________________
#Question 2#
describe(happy)
#modalités pour happy = not happy, pretty happy, very happy / 
#modalités pur degree= lt high school | high school | junior college | bachelor | graduate
#modalités pour health = poor | fair | good | excellent
_______________________________________________________________________________
#question 3 (Echec) #
happy%>%
  filter((is.na(age))%>% 
  tbl_summary(include="age""degree""finrela""happy""health""marital""sex""wtsall""year")
              by=id,
              percent="row")
_______________________________________________________________________________
#question 4# 
## Cutting happy$age into happy$age_group
happy$age_group <- cut(happy$age,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(18, 29, 30, 49, 50, 64, 65, 90)
)

## Recoding happy$happy into happy$happiness_score
happy$happiness_score <- happy$happy %>%
  fct_recode(
    "1" = "not too happy",
    "2" = "pretty happy",
    "3" = "very happy"
  )
_______________________________________________________________________________
#question 5#
summary(happy$age)

happy%>%
  tbl_summary(
    include=c("degree","age_group")
  ) 
_______________________________________________________________________________
#question 6#
happy%>%
  tbl_summary(
    include=c("happy","sex"),
    by=sex
  )%>%
  add_overall()


library(ggplot2)

ggplot(happy) +
 aes(x = sex, fill = happy) +
 geom_bar() +
 scale_fill_hue(direction = 1) +
 theme_minimal()
_______________________________________________________________________________
#Question 7#
happy%>%
  tbl_summary(
    include=c("happy","marital"),
    by=marital,
    percent="row"
  )


library(ggplot2)

ggplot(happy) +
 aes(x = marital, fill = happy) +
 geom_bar() +
 scale_fill_hue(direction = 1) +
 theme_minimal()
_______________________________________________________________________________
#question 8#
happy%>%
  tbl_summary(
    include=c("happy","health"),
    by=health,
    percent="row"
  )
#il est possible de voir que l'état de santé a un impact sur le bohneur, ceux ayant 
#un état de santé plutôt mauvais sont moins heureux que ceux ayant un bon ou excellent état de sante# 
#il y a une petite execption, ceux qui se considèrent comme plutot heureux sont plus nombreux dans la 
#catégorie bon état de santé que excellent état de santé, mais la tendance reste la même : 
#plus on a un bon état de santé plus on est heureux. 
_______________________________________________________________________________
#question 9#

happy$financial_status <- happy$finrela %>%
  fct_recode(
    "low" = "far below average",
    "low" = "below average",
    "high" = "above average",
    "high" = "far above average"
  )

d3<-happy%>%
  select(happy, marital, degree, health, financial_status)
acm<-dudi.acm(d3, scannf = FALSE, nf= Inf)

_______________________________________________________________________________
#question 10#
fviz_screeplot(acm,choice="eigenvalue")

fviz_screeplot(acm)


s.corcircle(acm$c1)
boxplot(acm) 

scatter(acm, col = RColorBrewer::brewer.pal(5, "Set1"))

#Selon moi les deux dimensions principales sont "très heureux" et "pas heureux" , les modalités 
#principales qui y contribuent le plus sont la santé, les diplômes et la situation financière#

#_______________________________________________________________________________
#question 11#
acm3 <- MCA(d3, quali.sup = 1:3)

#on peut observer un regroupement entre les variables health et financial status et une proximité 
# entre degree et marital #
#_______________________________________________________________________________
#question 12#
#Après analyse des résultats, les principaux facteurs associés à un haut niveau de bonheur sont 
#les diplômes,le statu fianncier et la santé

#Pour améliorer le bonheur il serait donc important (selon les résultats) d'être diplômés
#de l'enseignement supérieur, d'avoir un bon capital financier et une bonne santé. 
