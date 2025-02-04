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
