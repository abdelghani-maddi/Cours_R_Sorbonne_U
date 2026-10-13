# SERIE 01 - JOUR 02 - Filtres, ordre, ex aequo et doublons
# Travail prévu : 4 h. À rendre : j02_nom.R et un tableau des effectifs
# avant/après chacun des filtres. Commencer par refaire J01 Q08 et Q19.
# Aide : help("airquality", package = "datasets"), ?filter, ?slice, ?near.

library(dplyr)
library(tibble)
air <- as_tibble(datasets::airquality) |> mutate(id_jour = row_number())
voitures <- as_tibble(datasets::mtcars, rownames = "modele")

# Exemple : deux filtres successifs imposent que les deux conditions soient vraies.
exemple <- air |> filter(Month == 5) |> filter(Temp > 75)
nrow(exemple)

# A. Conditions logiques ---------------------------------------------------
# Q01. Compter les journées et les mois distincts. Vérifier que id_jour est unique.
# Q02. Retenir mai, puis mai OU juin. Proposer une écriture avec | et une avec %in%.
# Q03. Retenir les journées de mai à température > 75 (degrés Fahrenheit).
#      Comparer deux conditions séparées par une virgule, &, puis |.
# Q04. Retenir Temp entre 70 et 80, bornes comprises, avec between().
#      Vérifier le résultat avec >= et <=.
# Q05. Retenir (mai ou juin) ET Ozone > 50. Écrire les parenthèses explicitement.
#      Comparer à Month == 5 | Month == 6 & Ozone > 50 et expliquer l'écart.
# Q06. Retenir les journées qui ne sont ni en mai ni en juin. Employer ! et %in%.

# B. Conditions et valeurs manquantes --------------------------------------
# Q07. Compter les NA de Ozone. Pourquoi filter(Ozone == NA) ne les sélectionne-t-il pas ?
# Q08. Compter séparément Ozone > 50, Ozone <= 50 et Ozone manquant.
#      Vérifier que les trois nombres retrouvent l'effectif initial.
# Q09. Conserver Ozone > 50 OU manquant. Expliquer la différence avec Q08.
# Q10. Comparer 0.1 + 0.2 == 0.3 et near(0.1 + 0.2, 0.3).
#      Expliquer pourquoi near() ne remplace pas == pour les catégories ou codes.

# C. Trier et prélever -----------------------------------------------------
# Q11. Trier par mois croissant, puis Ozone décroissant, puis Day croissant.
#      Repérer où les NA sont placés. Le tri change-t-il l'effectif ?
# Q12. Extraire les cinq journées aux valeurs d'Ozone les plus fortes, d'abord
#      avec arrange()/slice_head(), puis avec slice_max(na_rm = TRUE).
# Q13. Chercher les trois plus petites valeurs de Ozone. Comparer with_ties
#      TRUE et FALSE ; compter les lignes. Justifier le choix pour un classement.
# Q14. Utiliser slice(), slice_head(), slice_tail() pour extraire respectivement
#      les lignes 1, 10, 20 ; les trois premières ; les trois dernières.
# Q15. Tirer huit journées au hasard avec slice_sample(). Refaire le tirage
#      après le même set.seed(). Vérifier l'égalité des deux résultats.

# D. Doublons --------------------------------------------------------------
# Q16. Construire air_double <- bind_rows(air, slice_head(air, n = 3)).
#      Comparer nrow(), n_distinct(id_jour), count(id_jour) et distinct().
# Q17. Ajouter encore une copie de la première ligne en remplaçant Temp par 99.
#      Comparer distinct() et distinct(id_jour, .keep_all = TRUE).
#      Pourquoi la deuxième commande ne résout-elle pas le conflit de mesure ?
# Q18. Corriger et expliquer :
#      air |> filter(Month == c(5, 6))
#      air |> filter(Ozone != NA)
#      air |> arrange(Ozone) |> slice_head(n = 5)  # on voulait les maxima

# E. Application -----------------------------------------------------------
# Q19. Dans voitures, retenir cyl égal à 4 ou 6, hp inférieur à 150 et mpg
#      supérieur à 20. Garder modele, cyl, hp, mpg ; classer mpg décroissant.
#      Produire le bilan des effectifs : départ, filtre cylindres, filtre hp,
#      filtre mpg. Ne pas confondre filtres successifs et filtres indépendants.
# Q20. Écrire trois contrôles : unicité des modèles, respect de chaque condition,
#      conservation des données brutes. Rédiger six lignes sur les cas perdus,
#      les NA, les doublons et les ex aequo. Refaire Q05 sur un seuil de 80.
