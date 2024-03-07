# Séance 4 - Analyse factorielles ----

# Chargement des packages ----
library(tidyverse)
library(ade4)
library(factoextra)
library(explor)
library(ggplot2)
library(questionr)
library(FactoMineR)


####### Analyse jeu de données titanic ----

install.packages("carData")
library(carData)
data(TitanicSurvival)


## Recodage de TitanicSurvival$age en TitanicSurvival$age_rec
TitanicSurvival$age_rec <- cut(TitanicSurvival$age,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(0.166700006, 12.5, 23.25, 31.5, 41.5, 53.5, 80)
)


d <- TitanicSurvival %>%
  select(-age)


d$age_rec <- d$age_rec %>%
  fct_recode(
    "moins 12 ans" = "[0.1667,12.5)",
    "12 à 23 ans" = "[12.5,23.25)",
    "24 à 31 ans" = "[23.25,31.5)",
    "32 à 41 ans" = "[31.5,41.5)",
    "de 41 à 53 ans" = "[41.5,53.5)",
    "plus de 53 ans" = "[53.5,80]"
  )

res_tit <- d %>%
  dudi.acm(., scannf = FALSE, nf = Inf)


#####################
## Arbre de décision
#####################
install.packages("explore")
library(explore)
titanic <- as_tibble(Titanic)

titanic %>% explore(Sex, target = Survived, n = n)
titanic %>% explore(Class, target = Survived, n = n)
titanic %>% explore(Age, target = Survived, n = n)


titanic %>% explain_tree(target = Survived, n = n)


