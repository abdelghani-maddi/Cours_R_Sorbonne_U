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
##importer la base 
data("happy")
##afficher les 10 premières lignes 
head(happy, 10)

###nombre d'observations et de variables dans la base:51020 OBSERVATION ET 10 VARIABLES  
### type de chaque varibale:id:numérique/ age: numérique/degree:caractère/finrela:caractère/happy: catactère/health:caractère/marital:caractère/sex:caractère/wtsaal: numérique/ year: numérique
str(happy)
###modalités variables
summary(happy$happy)
summary(happy$degree)
summary(happy$health)


###variables contenant valeurs manquantes:6 varibales qui sont:[1] "happy"   "age"     "marital" "degree"  "finrela" "health" 
variables_na <- names(happy)[colSums(is.na(happy)) > 0]
print(variables_na)
###pourcentage des valeurs manquantes pour chaque variable
pourcentage_v_manquants <- sapply(happy, function(x) mean(is.na(x)) * 100)
pourcentage_v_manquants

###création des variables: age_group
## Recodage de happy$age en happy$age_rec
happy$age_group <- cut(happy$age,
                     include.lowest = TRUE,
                     right = FALSE,
                     dig.lab = 4,
                     breaks = c(18, 29, 49, 64)
)
## Recodage de happy$happy en happy$happy_rec
happy$happiness_score <- happy$happy %>%
  fct_recode(
    "1" = "not too happy",
    "2" = "pretty happy",
    "3" = "very happy"
  )
####résumé statistique:
describe(happy$age)
summary(happy$age)
###analyser la repartition des repondants selon degree et age group
happy %>%
  tbl_summary(
    include = c(age_group, degree),
    percent = "row" 
  )
###croisement et visualisation
table(happy$age_group, happy$sex)
happy %>%
  tbl_summary(
    by = sex,                   
    include = c("happy", "sex"), 
    percent = "row"              
  )
##graph
ggplot(happy) +
  aes(x = happy, fill = sex) +
  geom_bar() +
  scale_fill_manual(
    values = c(male = "#421AEA",
               female = "#FF61C3")
  ) +
  labs(title = "le sentiment de joie selon le sexe ") +
  theme_minimal() +
  facet_wrap(vars(sex))
### impact du statut matrimonial sur le bonheur: 
happy %>%
  tbl_summary(
    by = happy,                   
    include = c("happy", "marital"), 
    percent = "row"              
  )
#on peut dire que le mariage fait qu'on est plus heureux dans la vie, de ce fait le mariage peut faire le bonheur puisque la catégorie qui se déclare le plus heureuse avec un taux de joie extreme de 40% est la catégoeie des mariés et le taux de malheur le moins elevé, 7,7%
##graph de visualisation
ggplot(happy) +
  aes(x = marital, fill = happy) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  theme_minimal()
###santé et bonheur 
happy %>%
  tbl_summary(
    by = health,                     
    include = c("happy", "health"),   
    percent = "row"                   
  ) %>%
  add_p()                               
theme_gtsummary_language("fr", decimal.mark = ",", big.mark = " ")  
#les tendances obervées indiquent qu'il y a un lien entre le bonheure et l'etat de santé, plus notre état de santé est bonne plus on a tendnace à etre heureux moins notre etat de santé est bonne moins on est heureux. En appliquant le test de chi2 on constate aussi qu'il existe bel et bien une corréaltion entre les deux variables 
###préparation pour ACM
library(ade4)
dataacm<-happy %>%
  select(happy,marital,degree,health,finrela)
## Recodage de dataacm$finrela en dataacm$finrela_rec
dataacm$finrela_rec <- dataacm$finrela %>%
  fct_recode(
    "low" = "far below average",
    "low" = "below average",
    "high" = "above average",
    "high" = "far above average"
  )
acm <- dudi.acm(dataacm, scannf = F, nf = Inf)
explor::explor(acm)
###Pour l'axe numéro 1 il explqiue 12,34% de l'inertie totale et pour l'axe numéro 2, 
# il exploite 10,55% de l'inertie totale ce qui nous donne un totale d'environ 23% ce 
# qui reste un taux correcte pour les sciences sociales afin de parler des corrélations existentes.
###les modalités qui contribuent à l'axe numéro 1 sont à la fois la situation maritale, l'etat de santé.
###pour les modalités qui contribuent à l'axe 2, ce sont la situation financière, 
# le niveau d'étude et le niveu de bonheur.
# ###On peut dire que sur le niveau finanacier on distingue trois groupes differents 
# avec trois types de revenus differents et qui ont une contributions significatvies dans notre acm.
# ##Nous pouvons également distinguer quelques profils en analysant les quadrant qui apparaissent dans notre acm.
# #on peut dire que ceux qui ont un niveau d'étude élevé en tendance à avoir un niveau 
# de revenues élevé, donc plus le niveau d'étude augmente plus on a tendance à 
# atteindre le niveau "high" qu'on a recodé comme l'indique le quadrant en haut à gauche. 
# Ce quadrant nous indique aussi. Les profils dans ce cadrant se 
# rejouissent d'un etat de santé excellente également. 
# #donc ce sont des individus satisfaits et préviligiés. 
# #Pour le quadrant en haut à droite c'est l'inverse, il s'agit des individus 
# défavorisés avec un niveau de revenu faible et un état de santé faible. 
# donc ils correspondent aux individus précaires et vulnérables.
# #pour le quadrant en bas à gauche on peut dire qu'il correspond aux individus de 
# la classe moyenne supérieur avec un niveau de revenu moyen, ils sont mariés et 
# avec un bon etat de santé et ils sont heureux. Pour le dernier quadrant en bas 
# à droite on trouve des individus avec un etat de santé correcte, ils sont heureux 
# et avec un niveau d'études faible.
# ###les facteurs associés à un haut niveau de bonheur sont le fait d'etre marié, 
# avoir un bon niveau d'études et avec un bon etat de santé.De ce fait on peut 
# considérer que les caractéristiques de ce que nous avons représenté comme classe 
# moyenne supérieur correspondent aux critères nécessaires pour un haut niveau de bonheur. 
# #Les recommandation que nous pouvons faire à travers la represnetation graphique 
# que nous avons fait peuvent se manifester par la nécessité de favoriser une 
# politique de santé qui va vers l'inclusion des plus défavorisés et des soins accessibles. 
# #faut aussi penser à des poltiuqes qui favorisent la formation continue surtout chez 
# les profesisonnels les plus défavorisés.
