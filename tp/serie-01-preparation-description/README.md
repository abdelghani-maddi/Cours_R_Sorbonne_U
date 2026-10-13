# Série 01 — Préparation et description des données

Cette série approfondit la préparation des tableaux et leur description statistique. Le travail porte autant sur les fonctions que sur les décisions qui les accompagnent : choix des observations, recodage, dénominateurs, correspondances entre tables et interprétation des résultats.

**Organisation : dix journées de travail, 220 questions et un dossier de synthèse.** Prévoir environ 40 à 50 heures pour l'ensemble, en incluant la recherche dans l'aide, les essais, les contrôles et la rédaction. La durée dépend de la familiarité avec les fonctions ; les reprises prévues d'une journée à l'autre font partie du travail.

## Programme

| Jour | Dossier | Questions | Travail prévu | Fichiers |
| --- | --- | ---: | --- | --- |
| 01 | Colonnes, types et dictionnaire | 20 | 3 h 30 à 4 h | [Énoncé](enonces/01-colonnes.R) · [Corrigé](corriges/01-colonnes.R) |
| 02 | Filtres, ordre, ex aequo et doublons | 20 | 4 h | [Énoncé](enonces/02-lignes.R) · [Corrigé](corriges/02-lignes.R) |
| 03 | Calculs, recodages, facteurs et texte | 22 | 4 à 5 h | [Énoncé](enonces/03-recodages-facteurs.R) · [Corrigé](corriges/03-recodages-facteurs.R) |
| 04 | Valeurs manquantes et contrôle d'un fichier | 20 | 4 à 5 h | [Énoncé](enonces/04-manquants-controles.R) · [Corrigé](corriges/04-manquants-controles.R) |
| 05 | Groupes, dénominateurs et calculs ordonnés | 22 | 4 à 5 h | [Énoncé](enonces/05-groupes-resumes.R) · [Corrigé](corriges/05-groupes-resumes.R) |
| 06 | Jointures, clés et formats long et large | 22 | 4 à 5 h | [Énoncé](enonces/06-jointures-formats.R) · [Corrigé](corriges/06-jointures-formats.R) |
| 07 | Codes, libellés et valeurs manquantes déclarées | 22 | 4 h | [Énoncé](enonces/07-labels.R) · [Corrigé](corriges/07-labels.R) |
| 08 | Construction et vérification de tbl_summary() | 24 | 4 à 5 h | [Énoncé](enonces/08-tbl-summary.R) · [Corrigé](corriges/08-tbl-summary.R) |
| 09 | Différences, intervalles de confiance et tests | 22 | 4 à 5 h | [Énoncé](enonces/09-comparaisons.R) · [Corrigé](corriges/09-comparaisons.R) |
| 10 | Présentation, exports et dossier de synthèse | 26 | 5 à 6 h | [Énoncé](enonces/10-restitution-synthese.R) · [Corrigé](corriges/10-restitution-synthese.R) |

## Travail quotidien

Commencer par reprendre les questions indiquées de la journée précédente. Ouvrir ensuite l'énoncé du jour et examiner les données : unité d'observation, noms, types et effectifs. Pour une fonction nouvelle, consulter l'aide indiquée et tester sa syntaxe sur quelques lignes avant de l'appliquer au fichier entier.

Pour chaque question, conserver le premier essai, expliquer l'erreur éventuelle et écrire la correction. Afficher l'objet obtenu et vérifier au moins un résultat : dimensions, effectifs, modalités, correspondances entre anciennes et nouvelles variables, ou calcul indépendant d'une cellule du tableau. Les questions de rédaction se traitent dans des commentaires R, avec `#` au début de chaque ligne.

Consulter le corrigé après le premier essai. Comparer les résultats, pas seulement la forme du code : plusieurs solutions sont possibles. Lorsqu'une réponse a été consultée, refaire la question sans l'ouvrir à la séance suivante.

Terminer la journée en redémarrant R et en exécutant son script depuis le début. Un fichier rendu doit recréer ses objets et retrouver ses résultats sans utiliser une variable restée dans la session précédente.

## Ce qui est à rendre

Chaque journée donne lieu à un script `jNN_nom.R`, avec les réponses numérotées et les contrôles. Les productions complémentaires sont précisées dans les en-têtes des énoncés : dictionnaire de variables, bilan des filtres, journal des anomalies, tableaux ou note d'interprétation.

Le dossier du jour 10 comprend le script, les données préparées en RDS, le dictionnaire, les résumés numériques, les tableaux HTML et une note de 200 à 300 mots. La comparaison sur `swiss` doit distinguer les associations entre régions d'une relation entre individus. Les résultats obtenus avec deux seuils de regroupement sont comparés et commentés.

## Quelques points de contrôle

| Données | Repère à retrouver | Attention particulière |
| --- | --- | --- |
| iris | 150 fleurs, trois espèces, 50 fleurs par espèce | Le format long des quatre mesures compte 600 lignes, mais toujours 150 fleurs. |
| mtcars | 32 voitures | Les codes de transmission et de cylindres ne sont pas des mesures continues. |
| airquality | 153 journées ; 37 valeurs d'Ozone manquantes avant les altérations du jour 04 | Les effectifs disponibles dépendent de la variable. |
| Titanic | 32 cellules d'effectifs, 2 201 personnes | n() compte les cellules ; sum(Freq) compte les personnes. |
| ChickWeight | 578 mesures portant sur 50 poussins | Plusieurs lignes peuvent concerner le même individu. |
| CO2 | 84 mesures portant sur 12 plantes | La plante et la mesure n'ont pas la même clé. |
| ToothGrowth | 60 animaux, dix par combinaison supplément-dose | Le mode d'administration et la dose sont deux variables différentes. |
| swiss | 47 régions | Les indicateurs portent sur des populations de référence différentes. |

Les modifications introduites pour les exercices sont signalées dans le code. Elles ne doivent pas être présentées comme des erreurs présentes dans les données d'origine. Les [fiches des jeux de données](../../ressources/jeux-de-donnees.md) renvoient à leur documentation R.

## Retrouver une fonction

Le [répertoire des fonctions](../../ressources/index-fonctions.md) indique les dossiers et questions où chaque famille de commandes est travaillée. Les scripts chargent leurs bibliothèques et recréent les données au début de chaque journée ; ils se travaillent dans l'ordre du programme.

[Répertoire des séries](../README.md) · [Accueil du cours](../../README.md)
