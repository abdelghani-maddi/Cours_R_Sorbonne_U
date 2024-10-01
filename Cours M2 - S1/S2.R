########################################################
# Exploration, préparation et visualisation des données
########################################################

library(tidyverse)
library(questionr)
library(labelled)
library(ade4)

d <- hdv2003
d.hard <- subset(d, hard.rock == "Oui")
d.non.hard <- subset(d, hard.rock == "Non")
boxplot(d.hard$age, d.non.hard$age)


# Charger le jeu de données hdv2003
d <- hdv2003

# Filtrer les sous-ensembles de données
d.hard <- subset(d, hard.rock == "Oui")
d.non.hard <- subset(d, hard.rock == "Non")

# Créer le graphique boxplot avec des labels
boxplot(d.hard$age, d.non.hard$age,
        names = c("Hard Rock: Oui", "Hard Rock: Non"),
        main = "Distribution de l'âge selon la préférence pour le Hard Rock",
        xlab = "Préférence pour le Hard Rock",
        ylab = "Âge")


## Recodage de d$nivetud en d$nivetud_rec
d$nivetud_rec <- d$nivetud %>%
  fct_recode(
    "Primaire" = "N'a jamais fait d'etudes",
    "Primaire" = "A arrete ses etudes, avant la derniere annee d'etudes primaires",
    "Primaire" = "Derniere annee d'etudes primaires",
    "Secondaire" = "1er cycle",
    "Secondaire" = "2eme cycle",
    "Sup" = "Enseignement technique ou professionnel court",
    "Sup" = "Enseignement technique ou professionnel long",
    "Sup" = "Enseignement superieur y compris technique superieur"
  )
