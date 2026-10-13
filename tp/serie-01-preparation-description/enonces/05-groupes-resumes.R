# SERIE 01 - JOUR 05 - Groupes, dénominateurs et calculs ordonnés
# Travail prévu : 4 à 5 h. À rendre : j05_nom.R, trois tableaux de synthèse
# et une note sur la différence entre nombre de lignes et nombre d'individus.

library(dplyr)
library(tibble)
air <- as_tibble(datasets::airquality)
voitures <- as_tibble(datasets::mtcars, rownames = "modele")
passagers <- as_tibble(as.data.frame(datasets::Titanic))

# Exemple : la moyenne d'un groupe est calculée sur ses seules observations.
exemple <- air |> group_by(Month) |> summarise(n = n(), .groups = "drop")

# A. Résumer par groupe ----------------------------------------------------
# Q01. Compter les journées par mois avec count(), puis group_by()/tally().
#      Vérifier que la somme des effectifs retrouve 153.
# Q02. Produire par mois : nombre de jours, nombre d'Ozone renseignés,
#      moyenne, médiane et écart-type d'Ozone. Retirer le groupement à la fin.
# Q03. Refaire Q02 avec .by = Month. Contrôler l'égalité après classement par mois.
# Q04. Calculer moyenne et écart-type de Ozone, Solar.R, Wind et Temp avec across().
#      La sortie doit comporter une ligne par mois et des noms explicites.
# Q05. Ajouter une catégorie de température : < 75 ou >= 75. Compter par mois
#      et catégorie, puis calculer les pourcentages à l'intérieur de chaque mois.
# Q06. Refaire Q05 en calculant cette fois des pourcentages sur toutes les journées.
#      Expliquer les dénominateurs et vérifier où les pourcentages somment à 100.
# Q07. Utiliser add_count(Month). Pourquoi le tableau conserve-t-il 153 lignes ?
#      Comparer count(), add_count(), tally() et add_tally() sur cet exemple.
# Q08. Créer un objet groupé par Month, puis ajouter temp_cat avec group_by(.add=TRUE).
#      Examiner group_vars(). Utiliser ungroup() avant un pourcentage global.

# B. Les lignes ne représentent pas toujours des individus -----------------
# Q09. Lire l'aide de Titanic. Comparer nrow(passagers) et sum(Freq).
#      Que représente une ligne, et que représente Freq ?
# Q10. Produire les effectifs par Class et Survived avec count(wt = Freq).
#      Comparer au résultat de count() sans wt, et expliquer l'erreur d'unité.
# Q11. Calculer le pourcentage de survivants dans chaque classe en utilisant Freq.
#      Afficher aussi le nombre total de personnes de chaque classe.
# Q12. Retrouver l'effectif des survivants avec filter()/summarise(), puis avec
#      count(wt = Freq). Ces Freq sont-ils des poids de sondage ? Justifier.

# C. Plusieurs résultats par groupe ---------------------------------------
# Q13. Avec reframe(), produire les quartiles 25 %, 50 % et 75 % d'Ozone pour
#      chaque mois. Fournir une colonne probabilite et une colonne quantile.
#      Utiliser type = 2 dans quantile(). Pourquoi pas summarise() ici ?
# Q14. Centrer Temp sur sa moyenne mensuelle avec mutate() et .by.
#      Vérifier que l'écart moyen est nul dans chaque mois et que 153 lignes restent.
# Q15. Dans voitures, calculer les rangs de mpg avec min_rank(desc(mpg)),
#      dense_rank(desc(mpg)) et row_number(desc(mpg)). Examiner les ex aequo.
# Q16. Former quatre groupes avec ntile(mpg, 4), puis calculer leurs effectifs,
#      minimums et maximums. Des mpg identiques peuvent-ils être séparés ?

# D. Ordre chronologique ---------------------------------------------------
# Q17. Trier air par Month et Day. Calculer Temp de la veille avec lag(),
#      du lendemain avec lead(), puis l'écart à la veille. Traiter le premier NA.
# Q18. Refaire lag() séparément dans chaque mois. Combien de NA apparaissent ?
#      Pourquoi la veille du premier jour de juin est-elle perdue dans cette version ?
# Q19. Calculer cummean(Temp) et le nombre cumulé de jours Temp >= 80 avec cumsum().
#      Comparer la dernière moyenne cumulée à la moyenne globale.
# Q20. Par mois, extraire la première température, la dernière et la troisième
#      avec first(), last(), nth(). Vérifier que le tableau a été trié avant.

# E. Réemploi sur des mesures répétées -------------------------------------
# Q21. Dans ChickWeight, compter mesures et poussins distincts par Diet.
#      Calculer un poids moyen par poussin, puis résumer ces moyennes par Diet.
#      Comparer à une moyenne directe de toutes les mesures ; expliquer pourquoi
#      les poussins peuvent y contribuer avec des poids différents.
# Q22. Corriger le pourcentage suivant et expliquer le problème :
#      air |> count(Month, Day) |> group_by(Month) |> mutate(p = n / sum(n))
#      L'objectif annoncé était la part de chaque ligne dans l'ensemble du fichier.
#      Rédiger ensuite 150 mots sur unités, groupements et dénominateurs.
