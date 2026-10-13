# Travaux pratiques R - préparation des données et tableaux descriptifs

Ces dix exercices peuvent être réalisés directement dans RStudio. Aucun document Quarto n'est nécessaire. Ils utilisent exclusivement les jeux de données fournis avec R : `iris`, `mtcars`, `airquality` et `ToothGrowth`.

- [Énoncés : 10-exercices.R](10-exercices.R) : questions à résoudre, classées par compétence.
- [Corrigés commentés : 10-corriges-commentes.R](10-corriges-commentes.R) : commandes R, vérifications et précautions d'interprétation.

## Préparer RStudio

Installer les bibliothèques une fois avec `install.packages(c("dplyr", "tibble", "labelled", "gtsummary"))`. Ouvrir les scripts, sélectionner une ligne ou un bloc et utiliser Ctrl + Entrée (Cmd + Entrée sur Mac). Il est conseillé de tenter chaque exercice avant d'ouvrir son corrigé.

## Progression

| Numéro | Contenu |
| --- | --- |
| 1 | Explorer un tableau et sélectionner des colonnes — `glimpse(), select(), starts_with()` |
| 2 | Filtrer des observations — `filter(), select(), %in%` |
| 3 | Renommer et déplacer des colonnes — `rename(), relocate()` |
| 4 | Créer et recoder des variables — `mutate(), if_else(), case_when()` |
| 5 | Valeurs manquantes, tri et résumé — `is.na(), filter(), arrange(), summarise()` |
| 6 | Résumer des données par groupe — `group_by(), summarise(), count()` |
| 7 | Libellés de variables et modalités — `labelled::var_label(), labelled::set_variable_labels(), labelled::val_labels()` |
| 8 | Premier tableau descriptif — `tbl_summary(), statistic, digits, missing` |
| 9 | Comparer deux groupes — `tbl_summary(), add_p(), add_overall(), add_n()` |
| 10 | Tableau de synthèse d'une enquête fictive — `labelled, tbl_summary(), add_difference(), modify_header()` |

Les chapitres Quarto du dépôt restent disponibles pour consulter le cours, mais la pratique de ces TP ne nécessite ni Quarto ni de fichier `.qmd`.
