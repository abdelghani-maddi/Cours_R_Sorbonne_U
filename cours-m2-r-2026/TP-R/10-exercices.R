# TP R - Préparation des données et tableaux descriptifs
# Sorbonne Université - M2 Sociologie
#
# Utilisation : ouvrir ce fichier dans RStudio, puis exécuter les lignes
# progressivement avec Ctrl + Entrée (Cmd + Entrée sur Mac).
# Aucun fichier externe, aucun document Quarto n'est nécessaire.
# Installation (une seule fois) :
# install.packages(c("dplyr", "tibble", "labelled", "gtsummary"))
# Charger les bibliothèques au début de chaque séance :
library(dplyr)
library(tibble)
library(labelled)
library(gtsummary)

# Travail : écrire les commandes dans les espaces prévus.
# Après chaque question, afficher l'objet obtenu et expliquer ce qui change.

# ====================================================================
# EXERCICE 01 - Explorer un tableau et sélectionner des colonnes
# Données : iris | Fonctions : glimpse(), select(), starts_with()
# ====================================================================
# 1. Afficher les six premières lignes et la structure des variables.
# Votre code :

# 2. Conserver Sepal.Length, Sepal.Width et Species.
# Votre code :

# 3. Sélectionner toutes les mesures des pétales à l'aide de starts_with().
# Votre code :


# ====================================================================
# EXERCICE 02 - Filtrer des observations
# Données : mtcars | Fonctions : filter(), select(), %in%
# ====================================================================
# 1. Repérer les voitures comportant quatre cylindres.
# Votre code :

# 2. Parmi elles, conserver celles dont la consommation mpg dépasse 25.
# Votre code :

# 3. Retenir les voitures à quatre ou six cylindres puis afficher mpg, cyl et hp.
# Votre code :


# ====================================================================
# EXERCICE 03 - Renommer et déplacer des colonnes
# Données : iris | Fonctions : rename(), relocate()
# ====================================================================
# 1. Renommer Sepal.Length en longueur_sepale et Sepal.Width en largeur_sepale.
# Votre code :

# 2. Placer Species en première position.
# Votre code :

# 3. Vérifier les noms des colonnes sans modifier le tableau iris d'origine.
# Votre code :


# ====================================================================
# EXERCICE 04 - Créer et recoder des variables
# Données : mtcars | Fonctions : mutate(), if_else(), case_when()
# ====================================================================
# 1. Créer consommation_l_100km à partir de mpg avec la formule 235.215 / mpg.
# Votre code :

# 2. Créer une variable automatique indiquant si am vaut 0.
# Votre code :

# 3. Classer la puissance hp en trois catégories : moins de 100, 100 à 199, 200 et plus.
# Votre code :


# ====================================================================
# EXERCICE 05 - Valeurs manquantes, tri et résumé
# Données : airquality | Fonctions : is.na(), filter(), arrange(), summarise()
# ====================================================================
# 1. Compter les valeurs manquantes de Ozone.
# Votre code :

# 2. Écarter les observations sans Ozone.
# Votre code :

# 3. Trouver les cinq journées aux concentrations d'ozone les plus élevées et calculer moyenne et médiane.
# Votre code :


# ====================================================================
# EXERCICE 06 - Résumer des données par groupe
# Données : ToothGrowth | Fonctions : group_by(), summarise(), count()
# ====================================================================
# 1. Compter les observations par type de supplément et par dose.
# Votre code :

# 2. Calculer la longueur moyenne des dents pour chaque supplément.
# Votre code :

# 3. Calculer longueur moyenne et écart-type pour chaque couple supplément-dose.
# Votre code :


# ====================================================================
# EXERCICE 07 - Libellés de variables et modalités
# Données : mtcars | Fonctions : labelled::var_label(), labelled::set_variable_labels(), labelled::val_labels()
# ====================================================================
# 1. Créer une variable facteur transmission à deux modalités explicites.
# Votre code :

# 2. Attribuer un libellé à mpg et hp.
# Votre code :

# 3. Créer une variable étiquetée pour am, puis afficher ses codes et leurs libellés.
# Votre code :


# ====================================================================
# EXERCICE 08 - Premier tableau descriptif
# Données : iris | Fonctions : tbl_summary(), statistic, digits, missing
# ====================================================================
# 1. Construire un tbl_summary() avec Species et les quatre mesures.
# Votre code :

# 2. Présenter les variables numériques avec moyenne et écart-type.
# Votre code :

# 3. Arrondir à une décimale et supprimer la ligne des manquants si elle est inutile.
# Votre code :


# ====================================================================
# EXERCICE 09 - Comparer deux groupes
# Données : ToothGrowth | Fonctions : tbl_summary(), add_p(), add_overall(), add_n()
# ====================================================================
# 1. Décrire len et dose selon supp (OJ ou VC).
# Votre code :

# 2. Ajouter une colonne globale puis une colonne de p-values.
# Votre code :

# 3. Ajouter le nombre d'observations renseignées pour chaque variable et commenter une différence descriptive sans lui donner un sens causal.
# Votre code :


# ====================================================================
# EXERCICE 10 - Tableau de synthèse d'une enquête fictive
# Données : mtcars | Fonctions : labelled, tbl_summary(), add_difference(), modify_header()
# ====================================================================
# 1. Créer une catégorie de consommation forte (mpg < 20) et une variable puissance en deux catégories.
# Votre code :

# 2. Attribuer des libellés à mpg, hp et puissance_cat.
# Votre code :

# 3. Comparer mpg et hp selon am avec tbl_summary(), add_difference(), add_n(), modify_header() ; interpréter les différences de moyennes.
# Votre code :

