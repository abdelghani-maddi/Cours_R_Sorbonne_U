https://cran.r-project.org/web/packages/explore/vignettes/explore_titanic.html 

library(dplyr)
library(tibble)
library(explore)
titanic <- as_tibble(Titanic)


titanic %>% explain_tree(target = Survived, n = n)

titanic %>% explore(Age, target = Class, n = n)

titanic %>% explore(Sex, target = Class, n = n)


comptes <- titanic %>%
  group_by(Survived) %>%
  summarise(n = sum(n)) %>%
  mutate(share = (n/2201)*100)


datasets <- data()
# Séance 3 - Analyse factorielles ----

# Chargement des packages ----
library(tidyverse)
library(ade4)
library(factoextra)
library(explor)
library(ggplot2)
library(questionr)
library(FactoMineR)


## Exemple introductif ----
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
#explor::explor(acm)
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


# devtools::install_github("larmarange/JLutils")
library(JLutils)
s.freq(acm$li) # La taille des carrés représente le nombre d'observations, plutôt que simplement des point de même taille
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
scatter(acm, col = RColorBrewer::brewer.pal(n = 8, name = "Dark2")) # sortir 8 couleurs de la palette "Dark2"

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

