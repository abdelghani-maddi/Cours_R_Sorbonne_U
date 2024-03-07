# Séance 5 : Suite ACM et début CAH

rm(list = ls())
# Chargement des packages ----
library(tidyverse)
library(ade4)
library(factoextra)
library(explor)
library(ggplot2)
library(questionr)
library(FactoMineR)
library(gtsummary)


#####################
## Exemple introductif ## Recodage de d$age_rec ----
## jeux de données iris avec 150 individus et plusieurs variables sur les feuilles d'iris
data(iris)

ggplot(iris) +
  aes(x = Petal.Length, y = Petal.Width) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  theme_minimal()

res <- iris %>%
  select(starts_with("Petal")) %>%
  dudi.pca(nf = 2, scannf = FALSE)

## explorer le résultat
# explor::explor(res)

##
res2 <- explor::prepare_results(res)
explor::PCA_ind_plot(res2, xax = 1, yax = 2, ind_sup = FALSE, lab_var = NULL,
                     ind_lab_min_contrib = 0, col_var = NULL, labels_size = 9, point_opacity = 0.5,
                     opacity_var = NULL, point_size = 64, ellipses = FALSE, transitions = TRUE,
                     labels_positions = NULL)

## Exemple plus complet -----
data(hdv2003)

hdv2003$grpage <- cut(hdv2003$age, c(16, 25, 45, 65, 93), right = FALSE, include.lowest = TRUE)

hdv2003$etud <- hdv2003$nivetud
levels(hdv2003$etud) <- c(
  "Primaire", "Primaire", "Primaire", "Secondaire", "Secondaire",
  "Technique/Professionnel", "Technique/Professionnel", "Supérieur"
)

d2 <- hdv2003 %>%
  select(grpage, sexe, etud, peche.chasse, cinema, cuisine, bricol, sport, lecture.bd)

# le mieux :
acm <- dudi.acm(d2, scannf = FALSE, nf = Inf)
# explor::explor(acm)
screeplot(acm) # autre façon de représenter graph des axes

##
# Usage du factoextra 
fviz_screeplot(acm, choice = "eigenvalue") # inertie
fviz_screeplot(acm) # inertie / inerie totale (en %)
summary(acm) # autre façn de connaitre l'inertie des 5 premiers axes
##

# cercle des corrélations de l'ACM
s.corcircle(acm$co)
s.corcircle(acm$co, clabel = .7) # ajuster les étiquettes
fviz_mca_var(acm)
fviz_mca_var(acm, repel = TRUE) # éviter la superposition des étiquettes avec "repel"

# graphique plus beau des barycentres avec explor : 
# explor::explor(acm)
res <- explor::prepare_results(acm)
explor::MCA_var_plot(res, xax = 1, yax = 2, var_sup = FALSE, var_sup_choice = ,
                     var_lab_min_contrib = 0, col_var = "Variable", symbol_var = "Variable",
                     size_var = NULL, size_range = c(10, 300), labels_size = 10, point_size = 56,
                     transitions = TRUE, labels_positions = NULL, labels_prepend_var = FALSE,
                     xlim = c(-1.45, 1.81), ylim = c(-2.03, 1.23))

## outil pour analyser la distribution au sein des axes
boxplot(acm) # axe 1 par défaut
boxplot(acm, xax = 2) # choisir l'axe

# Pour analyser la contribution des modalités à la formation des axes
fviz_contrib(acm, choice = "var", axes = 1) #  personnaliser l'axe
fviz_contrib(acm, choice = "var", axes = 2) #  personnaliser l'axe

# Autre façon pour analyser la contribution des modalités à la formation des axes
par(mfrow = c(2, 2))
for (i in 1:4) 
  barplot(acm$cr[, i], names.arg = row.names(acm$cr), las = 2, main = paste("Axe", i))
par(mfrow = c(1, 1)) # à remettre pour ne pas afficher le reste des graphiques deux à deux

# Représentation des individus avec facrotextra (facultatif)
fviz_mca_ind(acm, geom ="point", alpha.ind = .25)


# # devtools::install_github("larmarange/JLutils")
# library(JLutils)
# s.freq(acm$li) # La taille des carrés représente le nombre d'observations, plutôt que simplement des point de même taille


# Une autre façon intéressante
s.hist(acm$li, clabel = 0, pch = 15)

# représentation qui lie les observations aux barycentes :
s.class(acm$li, d2$sexe)
s.class(acm$li, d2$sexe, col = c("red", "darkgreen"))
# Autre façon intéressante :
fviz_mca_ind(acm)
fviz_mca_ind(acm, geom = "point")
fviz_mca_ind(acm, geom = "point", habillage = d2$sexe)
fviz_mca_ind(acm, geom = "point", habillage = d2$sexe, addEllipses = TRUE)

# Variables supplémentaires / illustratives
s.class(acm$li, hdv2003$relig)
fviz_mca_ind(acm, geom = "point", habillage = hdv2003$relig) # cela marche aussi avec "habillahe" à condition de ne pas changer l'ordre des individus !

# Résumer l'information pour toutes les variables
scatter(acm)
# plettes de couleurs : https://www.datanovia.com/en/fr/blog/palette-de-couleurs-rcolorbrewer-de-a-a-z/ 
scatter(acm, col = RColorBrewer::brewer.pal(5, "Set1")) # sortir 5 couleurs de la palette "set1"
scatter(acm, col = RColorBrewer::brewer.pal(n = 4, name = "Dark2")) # sortir 8 couleurs de la palette "Dark2"

# ACM alternative 
d3 <- hdv2003 %>%
  select(peche.chasse, cinema, cuisine, bricol, sport, lecture.bd)
acm2 <- dudi.acm(d3, scannf = FALSE, nf = Inf)

# explor::explor(acm2)

# visualisation avec les variables sociodémographiques
fviz_mca_ind(acm2, geom = "point", habillage = hdv2003$sexe, addEllipses = TRUE)
fviz_mca_ind(acm2, geom = "point", habillage = hdv2003$grpage, addEllipses = TRUE)
# Visualisation globale
scatter(acm2, col = RColorBrewer::brewer.pal(5, "Set1"))

# Variables supplémentaires avec FactorMineR
acm3 <- MCA(d2) # ACM
# ACM avec les variables supplémentaires 
acm3 <- MCA(d2, quali.sup = 1:3) # on a la représentation de la corrélation des variables avec les axes



######## 
# CAH
########


---
  #   title: Analyse en Composantes Principales avec FactoMineR sur les données du cours
  # (données températures)
  #   
  #   Script et sorties R permettant de retrouver les graphes et sorties du cours.
  # Le jeu de données doit être téléchargé et sauvegardé dans un répertoire connu (dans mon cas, le fichier a été sauvegardé dans le répertoire "C:/husson"").
  
library(FactoMineR)

# Importation des données
library(readxl)
temperature <- read_excel("Jeux de données/AnaDo_JeuDonnees_TemperatFrance.xlsx")
class(temperature)
temperature <- data.frame(temperature)
rownames(temperature) <- temperature[,1]
temperature <- temperature[,-1]

summary(temperature)
look_for(temperature)

# L'ACP
res <- PCA(temperature, quanti.sup=13:16,quali.sup=17)
explor::explor(res)
res <- PCA(temperature, quanti.sup=13:16,quali.sup=17, graph=FALSE)
plot(res,cex=.7)
plot(res,choix="var",cex=.7)

# Résumé des principales sorties R
# On résume ici les résultats des 2 premières dimensions sur tous les individus
summary(res, ncp=2, nbelements=Inf)

# Graphe en coloriant les villes en fonction de la région
plot(res,habillage="Région")



# calcul de la matrice de distance
res <- dudi.pca(temperature[1:12], scannf = FALSE, nf = Inf)
md <- dist.dudi(res)

# calcul de la matrice des distances de Gower
library(cluster)
md_gower <- daisy(temperature[1:12], metric = "gower")

# calcul du dendrogramme
arbre <- hclust(md, method = "ward.D2")
arbre_gower <- hclust(md_gower, method = "ward.D2")

# Représenter le dendrogramme
plot(arbre)
rect.hclust(arbre, 2, border = "red")
rect.hclust(arbre, 5, border = "blue")

# Une façon plus visuelle de représenter le dendogramme
library(dendextend)
color_branches(arbre, k = 5) %>% ggplot(labels = FALSE)

library(factoextra)
fviz_dend(arbre, k = 5, show_labels = TRUE, rect = TRUE)

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
temperature$typo <- cutree(arbre, 3) # fonction cutree : apratenance de chaque observation à chaque classe (ne pas modifier l'ordre des observations dans les différents objets !!!)
temperature$typo_gower <- cutree(arbre_gower, 3) # même chose pour gower

# Soit faire un tbl_summary
temperature %>%
  tbl_summary(include = c(names(temperature), "typo"), by = "typo") %>%
  add_p() %>%
  separate_p_footnotes()


# ou bien utiliser ggtable :
temperature$typo <- factor(temperature$typo) # transfomer d'abord en facteur
temperature$typo_gower <- factor(temperature$typo_gower) # transfomer d'abord en facteur

# ggtable
ggtable(
  temperature,
  columnsX = c("typo", "typo_gower"),
  columnsY = names(temperature),
  cells = "col.prop",
  fill = "std.resid",
  legend = 1
)

