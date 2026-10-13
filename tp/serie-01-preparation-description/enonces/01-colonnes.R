# SERIE 01 - PREPARATION ET DESCRIPTION DES DONNEES
# JOUR 01 - Colonnes, types et dictionnaire de données
# Travail prévu : 3 h 30 à 4 h, rédaction comprise.
# À rendre : j01_nom.R et un dictionnaire de six variables de swiss.
#
# Une ligne de iris décrit une fleur ; une ligne de swiss décrit une région.
# Consulter help("iris", package = "datasets") et help("swiss", package = "datasets").
# Garder les données de départ intactes. Donner un nom aux résultats intermédiaires.

library(dplyr)
library(tibble)
fleurs <- as_tibble(datasets::iris)
regions <- rownames_to_column(datasets::swiss, var = "region") |> as_tibble()

# Exemple à lire : select() renvoie un tableau, pull() renvoie un vecteur.
exemple_tableau <- fleurs |> select(Species)
exemple_vecteur <- fleurs |> pull(Species)
class(exemple_tableau)
class(exemple_vecteur)

# A. Lire les données ------------------------------------------------------
# Q01. Afficher les dimensions, les noms et les six premières lignes de fleurs.
#      Écrire en commentaire ce que représentent une ligne et une colonne.

# Q02. Utiliser glimpse() et class() : quelles variables sont numériques ?
#      Quelle différence entre la classe de fleurs et celle de fleurs$Species ?

# Q03. Dans l'aide de swiss, relever l'année, l'unité d'observation et le sens
#      des six indicateurs. Préparer un dictionnaire : nom, sens, unité.
#      Pourquoi ne faut-il pas parler ici de 47 personnes enquêtées ?

# B. Sélectionner ----------------------------------------------------------
# Q04. Créer sepales avec Species, Sepal.Length et Sepal.Width, dans cet ordre.
#      Vérifier que le nombre de lignes n'a pas changé.

# Q05. Obtenir les deux colonnes Petal avec starts_with(), puis avec contains().
#      Ajouter Species dans les deux sélections et vérifier leur égalité.

# Q06. Sélectionner les longueurs avec ends_with(), puis avec matches().
#      Dans matches(), expliquer le rôle de $ et celui de \\ devant un point.

# Q07. Sélectionner les colonnes numériques avec where(is.numeric).
#      Exclure ensuite Sepal.Width. Combien de colonnes restent-elles ?

# Q08. Définir choix <- c("Species", "Petal.Length"). Sélectionner avec all_of().
#      Ajouter "absente" à choix et comparer all_of() à any_of().
#      L'erreur doit être expliquée, pas laissée active dans le script rendu.

# Q09. Sélectionner de Sepal.Width à Petal.Width avec l'opérateur :.
#      Comparer à une liste de noms explicites. Quelle écriture dépend de l'ordre ?

# Q10. Utiliser last_col() puis everything() pour placer Species en tête.
#      Expliquer pourquoi sélectionner par position peut être fragile.

# C. Nommer et extraire ----------------------------------------------------
# Q11. Renommer les quatre mesures en longueur_sepale, largeur_sepale,
#      longueur_petale, largeur_petale. Ne pas renommer fleurs lui-même.

# Q12. Sur une autre copie, passer tous les noms en minuscules avec rename_with().
#      Remplacer ensuite les points par des _ avec gsub() dans rename_with().

# Q13. Déplacer Species en tête avec relocate(), puis Petal.Width juste après.
#      Vérifier que les valeurs de Petal.Width n'ont pas changé.

# Q14. Extraire Sepal.Length avec pull(). Calculer sa moyenne.
#      Comparer class(select(fleurs, Sepal.Length)) et class(pull(...)).

# Q15. Sur regions, extraire un vecteur de fécondité nommé par région
#      avec pull(Fertility, name = region). Afficher les cinq premières valeurs.

# D. Vérifier et corriger --------------------------------------------------
# Q16. Comparer distinct(Species), n_distinct(Species) et count(Species).
#      Quel résultat donne les effectifs, lequel donne les modalités ?

# Q17. Corriger ces trois lignes et expliquer chaque erreur :
#      fleurs |> rename(Sepal.Length = longueur_sepale)
#      fleurs |> select("Species", "longueur_petale")
#      mean(fleurs |> select(Sepal.Length))

# Q18. Vérifier avec identical() que fleurs est toujours égal à as_tibble(iris).
#      Écrire un stopifnot() qui contrôle les dimensions de sepales.

# E. Refaire sur un autre tableau ------------------------------------------
# Q19. Sur regions, retenir region, Fertility, Education et Catholic.
#      Renommer les trois indicateurs ; placer region en tête ; extraire
#      Education dans un vecteur nommé. Contrôler les dimensions et les noms.

# Q20. Construire le dictionnaire de Q03 avec tibble(). Ajouter une colonne
#      contenant le type R de chaque variable. Afficher le dictionnaire.
#      Terminer par cinq lignes : sélection, renommage, extraction et contrôles.
#      Expliquer notamment pourquoi renommer Education ne traduit pas ses valeurs.

# Pour vérifier votre travail, comparer les objets et les explications au corrigé.
# Refaire Q08, Q12 et Q19 le lendemain sans consulter les réponses.
