# Analyse des données d’enquête avec R — M2 Sociologie

**Parcours : Évaluation et conseil — Sorbonne Université**

Ce dépôt est en cours de refonte pédagogique complète. Le nouveau cours de référence se trouve dans **`cours-m2-r-2026/`**.

## Finalité du cours

L’objectif n’est pas d’apprendre une collection de commandes R, mais de savoir conduire une analyse d’enquête de bout en bout :

1. formuler une question sociologique ;
2. comprendre la structure d’un fichier d’enquête ;
3. contrôler la qualité des données ;
4. recoder sans perdre le sens des variables ;
5. produire des descriptifs lisibles ;
6. tester des associations avec discernement ;
7. estimer et interpréter une régression logistique ;
8. construire et lire une ACM ;
9. tenir compte des pondérations lorsque le plan d’enquête l’exige ;
10. restituer un résultat à un commanditaire sans surinterpréter.

## Nouvelle structure

- `cours-m2-r-2026/index.qmd` : page d’accueil et progression.
- `cours-m2-r-2026/00-guide-enseignant.qmd` : déroulé pédagogique, compétences et évaluation.
- `cours-m2-r-2026/01-...` à `07-...` : chapitres progressifs.
- `cours-m2-r-2026/exercices/` : énoncés distribuables aux étudiants.
- `cours-m2-r-2026/corriges/` : corrigés détaillés, commentés étape par étape.
- `cours-m2-r-2026/R/setup.R` : environnement commun.
- `cours-m2-r-2026/annexes/` : contenus utiles mais non centraux (ACP, CAH, compléments).

## Principes pédagogiques

Chaque séance suit la même logique : **question → choix → code → contrôle → résultat → interprétation → limites**.

Les corrigés ne donnent pas seulement “le bon code”. Ils expliquent :
- pourquoi on fait une opération ;
- ce que R renvoie ;
- ce qu’il faut regarder dans la sortie ;
- ce qu’on peut dire sociologiquement ;
- ce qu’on ne peut pas conclure ;
- les erreurs fréquentes.

## Jeux de données privilégiés

Le cours s’appuie sur des données intégrées à des packages R afin de limiter les problèmes d’installation et de chemins :
- `questionr::hdv2003` ;
- `forcats::gss_cat` ;
- `MASS::survey` et `MASS::housing` ;
- `carData::GSSvocab` ;
- `TraMineR::biofam`.

## Rendu du cours

Le nouveau cours est un projet **Quarto**. Depuis RStudio, ouvrir `cours-m2-r-2026/` puis exécuter :

```bash
quarto render
```

Les anciens scripts restent présents pendant la transition et servent d’archive. Ils ne constituent plus l’ordre pédagogique recommandé.
