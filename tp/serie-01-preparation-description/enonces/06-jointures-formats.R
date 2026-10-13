# SERIE 01 - JOUR 06 - Plusieurs tables, clés et formats
# Travail prévu : 4 à 5 h. À rendre : j06_nom.R, schéma des tables et bilan
# des cardinalités. Refaire J05 Q05 et Q11 avant de commencer.

library(dplyr)
library(tidyr)
library(tibble)
longitudinal <- as_tibble(datasets::ChickWeight) |> mutate(Chick = as.character(Chick))
animaux <- longitudinal |> distinct(Chick, Diet)
mesures <- longitudinal |> select(Chick, Time, weight)
# Les deux tables sont issues du même fichier. Chick identifie le poussin ;
# (Chick, Time) identifie une mesure. Lire help("ChickWeight", package = "datasets").

# A. Comprendre les clés ---------------------------------------------------
# Q01. Compter les lignes de chaque table et les identifiants distincts.
#      Quelle clé est unique dans animaux ? Quelle clé l'est dans mesures ?
# Q02. Avec count()/filter(), vérifier que Chick n'est pas unique dans mesures,
#      mais que la paire Chick-Time l'est. Dessiner le lien entre les deux tables.
# Q03. Reconstituer longitudinal avec left_join() et join_by(Chick).
#      Indiquer relationship = "many-to-one". Vérifier effectif, poids et régime.

# B. Choisir une jointure --------------------------------------------------
# Q04. Retirer le poussin "1" de animaux. Comparer left_join() et inner_join()
#      sur mesures : effectifs obtenus, lignes perdues et valeurs de Diet manquantes.
# Q05. Avec anti_join(), retrouver les mesures dont le poussin est absent de cette
#      table d'animaux. Avec semi_join(), garder les mesures ayant une correspondance.
# Q06. Ajouter à animaux un identifiant fictif "hors_fichier", Diet = "1".
#      Comparer right_join() et full_join() à left_join() ; repérer la ligne sans poids.
#      Cette ligne a été créée pour illustrer une jointure, pas observée dans l'expérience.
# Q07. Renommer Chick en id_poussin dans animaux. Faire la jointure avec
#      join_by(Chick == id_poussin), sans renommer la table de mesures.

# C. Détecter les multiplications de lignes --------------------------------
# Q08. Ajouter une copie exacte de la première ligne de animaux. Expliquer pourquoi
#      relationship = "many-to-one" doit échouer. Capturer l'erreur avec tryCatch().
# Q09. Autoriser many-to-many uniquement pour observer ce que produit cette table
#      défectueuse. Compter les lignes ajoutées. Corriger ensuite la clé, puis refaire
#      une jointure many-to-one. Dans quel cas distinct() serait-il insuffisant ?
# Q10. Ajouter une colonne weight à une copie de animaux, avec une valeur arbitraire.
#      Examiner les suffixes .x et .y, puis utiliser suffix = c("_mesure", "_fiche").
#      Pourquoi les noms communs ne sont-ils pas nécessairement des clés communes ?
# Q11. Prendre les poids au temps 0, inverser leur ordre et utiliser bind_cols()
#      pour les accoler à animaux. Comparer à une jointure sur Chick et identifier
#      les erreurs silencieuses. Quand bind_cols() est-il justifié ?

# D. Empiler, comparer et mettre à jour ------------------------------------
# Q12. Séparer mesures en Time <= 10 et Time > 10. Les réunir avec bind_rows(.id).
#      Retrouver l'original après retrait de la colonne de provenance et tri.
# Q13. Sur les identifiants des temps 0 et 21, utiliser intersect(), union(),
#      setdiff() et setequal(). Quels poussins n'ont pas de mesure au temps 21 ?
# Q14. Créer une rectification de démonstration pour la première mesure : weight + 1.
#      Appliquer rows_update(by = c("Chick", "Time")) à une copie et vérifier qu'une
#      seule valeur change. Créer un NA au même endroit et tester rows_patch().

# E. Passer du format large au format long ---------------------------------
# Q15. Ajouter id_fleur = row_number() à iris. Passer les quatre mesures au format
#      long avec pivot_longer(). Prévoir noms de colonnes et nombre de lignes avant.
# Q16. Refaire le pivot en séparant le nom en organe et dimension avec names_sep.
#      Calculer la moyenne par Species, organe et dimension.
# Q17. Reconstituer iris au format large avec pivot_wider(). Replacer les colonnes
#      dans leur ordre initial et vérifier l'égalité des observations.
# Q18. Dupliquer une ligne du format long. Diagnostiquer les clés dupliquées avant
#      un nouveau pivot. Pourquoi values_fn = mean pourrait-il masquer une erreur ?

# F. Rendre les absences explicites ----------------------------------------
# Q19. Sur mesures, utiliser complete(Chick, Time). Compter les combinaisons ajoutées
#      et les poids NA. Pourquoi ne faut-il pas remplir ces poids par zéro ?
# Q20. Mettre les poids au format large : un poussin par ligne, une date par colonne.
#      Puis joindre Diet. Vérifier qu'il reste 50 poussins et que les trous sont visibles.
# Q21. Refaire le découpage en table de plantes et table de mesures sur CO2.
#      Identifier la clé de chaque table, faire une jointure contrôlée et vérifier
#      que 84 mesures sont conservées. Ne pas traiter les 84 lignes comme 84 plantes.
# Q22. Rédiger huit règles de contrôle avant/après une jointure ou un pivot.
#      Illustrer deux règles avec un stopifnot() susceptible de repérer une vraie erreur.
