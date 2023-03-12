
# Classification ascendante hiérarchique (CAH)

# Charger les données

library(questionr)
data(hdv2003)
library(tidyverse)
library(gtsummary)
library(GGally)
library(FactoMineR)


## Recodage de hdv2003$age en hdv2003$groupe_age
hdv2003$groupe_age <- cut(hdv2003$age,
                          include.lowest = TRUE,
                          right = FALSE,
                          dig.lab = 4,
                          breaks = c(18, 25, 45, 65, 97)
)
## Recodage de hdv2003$nivetud en hdv2003$etud
hdv2003$etud <- fct_recode(hdv2003$nivetud,
                           "Primaire" = "N'a jamais fait d'etudes",
                           "Primaire" = "A arrete ses etudes, avant la derniere annee d'etudes primaires",
                           "Primaire" = "Derniere annee d'etudes primaires",
                           "Secondaire" = "1er cycle",
                           "Secondaire" = "2eme cycle",
                           "Technique/Pro" = "Enseignement technique ou professionnel court",
                           "Technique/Pro" = "Enseignement technique ou professionnel long",
                           "Supérieur" = "Enseignement superieur y compris technique superieur"
)
hdv2003$etud <- fct_explicit_na(hdv2003$etud, "Non documenté")

d2 <- hdv2003 %>%
  select(groupe_age, sexe, etud, peche.chasse, cinema, cuisine, bricol, sport, lecture.bd)

library(ade4)
acm <- dudi.acm(d2, scannf = FALSE, nf = Inf)

# calcul de la matrice de distance
md <- dist.dudi(acm)

# calcul de la matrice des distances de Gower
library(cluster)
md_gower <- daisy(d2, metric = "gower")

# calcul du dendrogramme
arbre <- hclust(md, method = "ward.D2")
arbre_gower <- hclust(md_gower, method = "ward.D2")

# Représenter le dendrogramme
plot(arbre, labels = FALSE)
rect.hclust(arbre, 2, border = "red")
rect.hclust(arbre, 5, border = "blue")

# Une façon plus visuelle de représenter le dendogramme
library(dendextend)
color_branches(arbre, k = 5) %>% ggplot(labels = FALSE)

library(factoextra)
fviz_dend(arbre, k = 5, show_labels = FALSE, rect = TRUE)

# saut d'inertie
inertie <- sort(arbre$height, decreasing = TRUE)
plot(inertie[1:20], type = "s")

inertie_gower <- sort(arbre_gower$height, decreasing = TRUE)
plot(inertie_gower[1:10], type = "s")

# source(url("https://raw.githubusercontent.com/larmarange/JLutils/master/R/clustering.R"))
# best.cutree(arbre_gower, graph = TRUE)
# best.cutree(arbre, graph = TRUE)

# déterminer le nombre de classes avec des indicateurs poussés
library(WeightedCluster)
as.clustrange(arbre, md) %>% plot()
as.clustrange(arbre_gower, md_gower) %>% plot()


# Caractériser les classes ----
hdv2003$typo <- cutree(arbre, 3) # fonction cutree : apratenance de chaque observation à chaque classe (ne pas modifier l'ordre des observations dans les différents objets !!!)
hdv2003$typo_gower <- cutree(arbre_gower, 3) # même chose pour gower

# Soit faire un tbl_summary
hdv2003 %>%
  tbl_summary(include = c(names(d2), "typo"), by = "typo")

# ou bien utiliser ggtable :
hdv2003$typo <- factor(hdv2003$typo) # transfomer d'abord en facteur
hdv2003$typo_gower <- factor(hdv2003$typo_gower) # transfomer d'abord en facteur

# ggtable
ggtable(
  hdv2003,
  columnsX = c("typo", "typo_gower"),
  columnsY = names(d2),
  cells = "col.prop",
  fill = "std.resid",
  legend = 1
)

## Bonus : graphique 3D combinant ACM et CAH
library(FactoMineR)
ACM <- MCA(d2)
res.hcpc <- HCPC(ACM, graph = FALSE)
plot(res.hcpc, choice = "3D.map")
