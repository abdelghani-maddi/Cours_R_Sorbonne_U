# SERIE 01 - JOUR 10 - Tableaux, exports et dossier de synthèse
# Travail prévu : 5 à 6 h. À rendre : j10_nom.R, tableaux HTML, fichier RDS,
# dictionnaire, contrôles et note de 200 à 300 mots sur les régions suisses.
# Prévoir un premier essai des questions 15 à 26 sans consulter les corrigés.

library(dplyr)
library(tidyr)
library(tibble)
library(labelled)
library(gtsummary)
library(gt)
voitures <- as_tibble(datasets::mtcars, rownames = "modele") |> mutate(
  transmission = factor(am, levels = c(0, 1), labels = c("Automatique", "Manuelle")),
  cyl_cat = factor(cyl, levels = c(4, 6, 8))
) |> set_variable_labels(mpg = "Distance parcourue (miles/gallon US)",
                         wt = "Poids (milliers de livres)", hp = "Puissance (hp bruts)",
                         cyl_cat = "Nombre de cylindres")

# A. Préparer la présentation ---------------------------------------------
# Q01. Décrire mpg, wt, hp et cyl_cat selon transmission, avec moyenne (écart-type),
#      une décimale et les effectifs des catégories. Conserver l'objet tbl_voitures.
# Q02. Lire show_header_names(). Modifier l'en-tête des variables et celui des
#      colonnes statistiques en y insérant le nom du groupe et son effectif.
# Q03. Ajouter un en-tête englobant « Transmission » avec modify_spanning_header()
#      et un titre avec modify_caption().
# Q04. Ajouter une note aux en-têtes statistiques avec modify_footnote_header(),
#      sans effacer la note précisant les statistiques utilisées.
# Q05. Ajouter une note à la ligne mpg avec modify_footnote_body() : une valeur
#      élevée signifie davantage de distance par gallon, pas plus de litres consommés.
# Q06. Mettre les libellés en gras et les modalités en italique. Comparer avant/après.
# Q07. Masquer la deuxième colonne de groupe avec modify_column_hide(), puis la
#      réafficher avec modify_column_unhide(). Les données sont-elles supprimées ?
# Q08. Préparer une fonction n_valeurs(data, variable, ...) renvoyant un tibble
#      d'une ligne avec le nombre de valeurs distinctes non manquantes.
# Q09. L'utiliser dans add_stat() pour les mesures quantitatives. Modifier
#      l'en-tête ajouté et son format avec modify_fmt_fun().

# B. Assembler et exporter -------------------------------------------------
# Q10. Construire deux tableaux globaux des mêmes mesures mpg, wt et hp :
#      moyenne (écart-type) puis médiane [Q1 ; Q3]. Les juxtaposer avec tbl_merge().
# Q11. Construire deux tableaux par transmission : mpg/wt d'une part, hp/cyl_cat
#      d'autre part. Les empiler avec tbl_stack() et des titres de sections.
# Q12. Exporter les tableaux mis en forme, fusionnés et empilés en HTML avec
#      as_gt() puis gt::gtsave(). Écrire dans resultats/serie-01/j10/.
# Q13. Exporter la version as_tibble() d'un tableau avec write.csv2().
#      Préciser ce qui disparaît : notes, gras, titres englobants ou données sources.
# Q14. Enregistrer le tableau et les données documentées avec saveRDS().
#      Les relire et contrôler leur classe et leur contenu.

# C. Dossier de synthèse : régions suisses ---------------------------------
# Question : comment les indicateurs de fécondité, d'instruction et d'activité
# agricole se distribuent-ils selon la part de catholiques dans les régions ?
# Q15. Charger swiss avec les noms de lignes dans une colonne region.
#      Présenter l'époque, l'unité d'observation et les définitions des indicateurs.
# Q16. Produire un diagnostic : dimensions, unicité de region, types, NA et étendues.
#      Vérifier que les six indicateurs sont compris entre 0 et 100.
# Q17. Séparer region/Fertility/Education/Agriculture d'une table
#      region/Catholic/Infant.Mortality/Examination. Les réunir par une jointure
#      one-to-one ; vérifier qu'aucune région ni valeur n'a changé.
# Q18. Créer catholicite : < 50 ou >= 50, dans cet ordre. Créer education_cat :
#      < 10, [10,20[, >= 20. Vérifier les bornes et croiser les deux catégories.
# Q19. Attribuer des libellés aux variables utiles. Produire un dictionnaire
#      indiquant aussi l'unité, les regroupements et les populations concernées.
# Q20. Calculer par catholicite les effectifs, moyennes et écarts-types des trois
#      indicateurs. Retrouver la moyenne globale en pondérant les moyennes de groupe.
# Q21. Construire le tableau descriptif avec trois mesures et education_cat,
#      selon catholicite, avec total, n disponibles, titres et notes nécessaires.
# Q22. Dans un tableau séparé, ajouter les différences de moyennes des trois mesures,
#      dans le sens < 50 moins >= 50. Vérifier une estimation à la main.
#      Expliquer les limites d'une inférence à partir de ces données régionales.
# Q23. Passer les trois mesures en format long et reconstruire une synthèse
#      variable x catholicite. Vérifier sa cohérence avec Q20, sans réutiliser
#      les chaînes de caractères du tableau de présentation.
# Q24. Refaire le regroupement de catholicité avec un seuil de 75. Compter les
#      régions changeant de groupe ; comparer effectifs et écarts moyens.
#      Pourquoi ne faut-il pas choisir le seuil en fonction de la p-value souhaitée ?
# Q25. Exporter données, dictionnaire, résumés et tableaux. Écrire la note de
#      200 à 300 mots : question, données, trois résultats chiffrés, sens des
#      comparaisons, sensibilité au seuil et limites. Joindre sessionInfo().
# Q26. Redémarrer R et exécuter votre script entier. Vérifier clés, effectifs,
#      recodages, moyennes et fichiers créés. Reprendre ensuite une question
#      de chacun des jours 02, 04, 06, 07 et 08 sans ouvrir les corrigés.
