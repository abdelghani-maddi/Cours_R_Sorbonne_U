# SERIE 01 - JOUR 08 - Construire et vérifier un tableau descriptif
# Travail prévu : 4 à 5 h. À rendre : j08_nom.R, quatre tableaux commentés et
# le calcul manuel de trois cellules choisies dans ces tableaux.

library(dplyr)
library(tibble)
library(labelled)
library(gtsummary)
air <- as_tibble(datasets::airquality) |> mutate(
  mois = factor(Month, levels = 5:9, labels = c("Mai", "Juin", "Juillet", "Août", "Septembre")),
  temp_c = (Temp - 32) * 5 / 9,
  ozone_fort = if_else(Ozone > 50, 1L, 0L, missing = NA_integer_)
) |> set_variable_labels(Ozone = "Ozone (ppb)", Solar.R = "Rayonnement solaire (Langley)",
                         Wind = "Vent (miles/h)", temp_c = "Température (°C)",
                         mois = "Mois", ozone_fort = "Ozone supérieur à 50 ppb")
# Le seuil de 50 sert au recodage de ce TP ; ce n'est pas un seuil réglementaire.
# Aide : ?gtsummary::tbl_summary ; lire en particulier type, statistic et percent.

# A. Choisir ce qui est décrit --------------------------------------------
# Q01. Produire tbl_summary() avec Ozone, Solar.R, Wind, temp_c, mois et ozone_fort.
#      Examiner la classe de l'objet et expliquer pourquoi il n'est plus un data.frame.
# Q02. Identifier les types choisis automatiquement. Pourquoi le binaire 0/1
#      apparaît-il sur une seule ligne ? Forcer son type à "categorical" pour comparer.
# Q03. Retenir seulement Ozone, Wind, temp_c et mois avec include =.
#      Refaire le tableau après select(). Les colonnes du fichier sont-elles supprimées ?
# Q04. Modifier le libellé d'Ozone dans le tableau avec label =, puis vérifier
#      que var_label(air$Ozone) n'a pas changé.

# B. Statistiques et précision --------------------------------------------
# Q05. Afficher moyenne (écart-type) pour toutes les variables quantitatives,
#      avec une décimale. Afficher les deux statistiques calculées directement sur Ozone.
# Q06. Construire un autre tableau avec médiane [Q1 ; Q3]. Préciser ce que
#      décrivent la moyenne, la médiane, l'écart-type et l'intervalle interquartile.
# Q07. Vérifier les quartiles d'Ozone avec quantile(type = 2). Comparer au type 7.
#      Expliquer pourquoi des logiciels peuvent afficher des quartiles légèrement différents.
# Q08. Utiliser type = "continuous2" pour afficher trois lignes par mesure :
#      moyenne (écart-type), médiane [Q1 ; Q3], minimum ; maximum.
# Q09. Utiliser des précisions différentes : Ozone à zéro décimale, Wind à deux,
#      temp_c à une ; effectifs entiers et pourcentages à une décimale.

# C. Valeurs manquantes ----------------------------------------------------
# Q10. Comparer missing = "ifany", "always" et "no" sur Ozone et temp_c.
#      Masquer une ligne de manquants supprime-t-il les NA des données ?
# Q11. Afficher "Non renseigné" et "{N_miss} ({p_miss}%)" sur la ligne manquante.
#      Vérifier son dénominateur avec dplyr.

# D. Groupes et pourcentages -----------------------------------------------
# Q12. Décrire Ozone, Wind, temp_c et ozone_fort selon mois avec by =.
#      Vérifier les effectifs des en-têtes à l'aide de count().
# Q13. Afficher ozone_fort comme dichotomique avec value = 1, puis value = 0.
#      Adapter le libellé à la modalité affichée. Pourquoi cette adaptation est-elle nécessaire ?
# Q14. Forcer ozone_fort au type catégoriel et comparer percent = "column",
#      "row", "cell". Calculer les trois dénominateurs avec count()/mutate().
# Q15. Vérifier que les pourcentages de Q14 somment à 100 au bon endroit.
#      Les 37 valeurs d'Ozone manquantes entrent-elles dans ces dénominateurs ?
# Q16. Classer les mois par fréquence avec sort =. Distinguer cet ordre
#      de l'ordre des colonnes quand mois est la variable by.

# E. Compléter le tableau --------------------------------------------------
# Q17. Ajouter une colonne globale avec add_overall(), d'abord en tête, puis à la fin.
#      Vérifier la moyenne globale : est-ce la moyenne simple des cinq moyennes mensuelles ?
# Q18. Ajouter add_n(statistic = "{N_nonmiss} / {N_obs}"). Que décrit cette
#      colonne ? Pourquoi n'est-elle pas toujours égale au N de l'en-tête ?
# Q19. Ajouter le libellé des statistiques avec add_stat_label(location = "column").
#      Passer les textes automatiques en français avec theme_gtsummary_language().
# Q20. Dans une copie, rendre mois manquant pour la première ligne. Construire
#      le tableau par mois et relever l'avertissement et l'effectif utilisé.
#      Pourquoi missing = "always" ne crée-t-il pas un groupe « mois inconnu » ?

# F. Vérification et réemploi ----------------------------------------------
# Q21. Convertir un tableau avec as_tibble(). Examiner les types des colonnes.
#      Pourquoi ce tableau de présentation ne remplace-t-il pas les données d'analyse ?
# Q22. Dans ToothGrowth, décrire dose comme une variable catégorielle, puis
#      comme une variable continue. Justifier le choix selon la question posée.
# Q23. Produire un tableau final par mois : Ozone, Wind, temp_c et ozone_fort ;
#      moyenne (écart-type), deux modalités pour ozone_fort, NA visibles, total
#      et effectifs disponibles. Vérifier une moyenne, un effectif et un pourcentage.
# Q24. Refaire un tableau sur swiss selon Catholic >= 50 ou < 50. Décrire
#      Fertility, Education et Agriculture, avec des libellés documentés.
#      Rédiger 150 mots en précisant les unités d'observation et les limites des comparaisons.
