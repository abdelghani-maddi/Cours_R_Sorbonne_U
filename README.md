# Analyse des données d'enquête avec R — M2 Sociologie

**Parcours Évaluation et conseil — Sorbonne Université**

Ce dépôt regroupe les supports du cours, les travaux pratiques et leurs corrigés.

## Commencer par les exercices R

Les étudiants peuvent travailler directement dans RStudio, sans connaître Quarto ni les fichiers `.qmd`.

- [10 exercices de préparation des données et statistiques descriptives](cours-m2-r-2026/TP-R/10-exercices.R)
- [Corrigés R détaillés et commentés](cours-m2-r-2026/TP-R/10-corriges-commentes.R)
- [Consignes et progression des travaux pratiques](cours-m2-r-2026/TP-R/README.md)

Les exercices utilisent les données fournies avec R : `iris`, `mtcars`, `airquality`, `ToothGrowth`. Les thèmes sont `select()`, `filter()`, `rename()`, `mutate()`, les regroupements, les valeurs manquantes, les variables étiquetées avec `labelled` et les tableaux produits avec `gtsummary::tbl_summary()`.

**Première utilisation :** installer une fois les packages avec :

```r
install.packages(c("dplyr", "tibble", "labelled", "gtsummary"))
```

Ouvrir ensuite le script d'exercices dans RStudio. Les lignes s'exécutent avec Ctrl + Entrée (Cmd + Entrée sur Mac). Les corrigés sont dans un fichier séparé.

## Supports de cours

Les [chapitres du cours M2](cours-m2-r-2026/) présentent les différentes étapes d'une analyse d'enquête : découverte des données, préparation, statistiques descriptives, tests, régression logistique, ACM, pondérations et restitution. Ils sont aussi disponibles en Quarto pour celles et ceux qui souhaitent consulter ou produire une version mise en page, mais Quarto n'est pas requis pour les travaux pratiques.

Les documents plus anciens restent accessibles dans les autres dossiers du dépôt comme ressources complémentaires. Ils ne constituent pas l'ordre conseillé pour commencer.
