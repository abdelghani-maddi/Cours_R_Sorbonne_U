# Organisation de l'enseignement

## Série 01 : progression sur dix journées

Les dossiers approfondissent les fonctions de préparation et de description déjà abordées en cours. Ils comportent 220 questions, dont des applications sur un second jeu de données, des erreurs à expliquer et des vérifications indépendantes des tableaux.

Le volume prévu est d'environ 40 à 50 heures. Il s'agit d'une estimation pédagogique, à ajuster à partir des premiers travaux rendus, et non d'une durée mesurée auprès d'une promotion. La progression se répartit sur dix journées distinctes. Les recherches dans l'aide, les essais, les réponses rédigées et les reprises sans corrigé font partie de ce volume. Exécuter le corrigé ne constitue pas le travail demandé.

| Jour | Point à valider avant la suite | Production à examiner |
| --- | --- | --- |
| 01 | Distinguer sélection, renommage et extraction ; lire l'unité d'observation | Dictionnaire de swiss et contrôles de dimensions |
| 02 | Combiner les conditions et comptabiliser les exclusions | Bilan des filtres ; traitement des NA, ex aequo et doublons |
| 03 | Tester les bornes et conserver le sens des catégories | Tables ancien/nouveau codage ; dictionnaire des variables créées |
| 04 | Distinguer valeur manquante, sentinelle et erreur de mesure | Journal des anomalies ; effectifs disponibles par variable |
| 05 | Choisir le dénominateur et l'unité de comptage | Pourcentages par groupe ; Titanic ; mesures répétées |
| 06 | Définir les clés et contrôler les cardinalités | Schéma des tables ; jointure contrôlée ; retour long/large |
| 07 | Séparer codes, libellés et facteurs d'analyse | Tableau des correspondances ; conservation des métadonnées |
| 08 | Justifier types, statistiques et pourcentages | Tableau descriptif et contrôle manuel de trois cellules |
| 09 | Lire le sens d'un contraste, l'IC et les hypothèses du test | Comparaison à dose fixée ; interprétation avec effectifs |
| 10 | Produire et vérifier une restitution complète | Script autonome, données, dictionnaire, tableaux et note |

Prévoir un court retour sur le travail précédent au début de chaque journée. Faire réécrire une sélection, un recodage ou un contrôle sans ouvrir le corrigé. Aux jours 06 à 10, varier les noms des variables ou les seuils afin de vérifier que les opérations sont comprises, plutôt que reproduites à l'identique.

Les questions de tests du jour 09 sont des exercices de lecture des sorties et de choix explicite des méthodes. Elles ne dispensent pas de traiter en cours les hypothèses d'indépendance, les intervalles de confiance, les comparaisons multiples et les limites d'une interprétation causale. Les alertes sur les données agrégées et répétées sont à conserver lors de la correction.

## Barème de travail, sur 20

| Critère | Points | Éléments attendus |
| --- | ---: | --- |
| Préparation et documentation | 6 | Sélections et recodages corrects, types adaptés, libellés et dictionnaire précis |
| Contrôles | 4 | Effectifs avant/après, clés, bornes, manquants et vérification numérique |
| Tableaux | 4 | Statistiques et dénominateurs justifiés, groupes et modalités correctement affichés |
| Interprétation | 4 | Résultats chiffrés, unités, sens des différences, limites et absence de conclusion excessive |
| Reproductibilité | 2 | Script autonome, fichiers retrouvés et versions consignées |

Une erreur de code corrigée et expliquée n'a pas le même statut qu'une commande copiée sans contrôle. Pour le dossier final, demander une courte explication orale d'un recodage, d'une jointure et d'une cellule du tableau.

## Ajouter une série au fil du programme

Conserver les identifiants des séries distribuées. La série suivante prendra un nom de la forme `tp/serie-02-theme/`, puis `serie-03-theme/`, sans renommer la série 01. Une série comprend un `README.md`, un dossier `enonces/` et un dossier `corriges/`. Les fichiers d'un même exercice portent exactement le même nom dans les deux dossiers.

Les [modèles d'énoncé](modeles/enonce.R) et de [corrigé](modeles/corrige.R) donnent les rubriques à renseigner. La numérotation `# Q01.`, `# Q02.`, etc. doit être identique dans les deux fichiers. Chaque corrigé recrée ses données, traite toutes les questions, comporte des contrôles et explique les choix d'analyse. Les sorties sont écrites dans `resultats/serie-NN/jNN/`.

Lorsqu'une série est prête, ajouter une ligne dans [le répertoire des TP](../tp/README.md) et dans [l'accueil du cours](../README.md). Renseigner les prérequis, le volume de travail, les fichiers à rendre et les liens vers chaque dossier. N'ajouter au catalogue que les séries distribuables ; les dossiers vides ne servent pas de programme prévisionnel.

Les ressources communes restent dans `ressources/`. Les nouvelles entrées du répertoire des fonctions précisent la série, le dossier et les questions concernées. Une correction ponctuelle ne doit pas produire plusieurs fichiers nommés « final », « v2 » ou « nouveau » : modifier le fichier de référence et laisser l'historique Git conserver les versions antérieures.

## Vérifier les scripts

Depuis la racine du projet, exécuter :

```r
source("enseignant/verifier_corriges.R", encoding = "UTF-8")
```

Le même script est lancé par le workflow GitHub **Verifier les scripts R**. Il contrôle l'appariement des énoncés et corrigés, la numérotation des questions, la syntaxe et l'exécution dans des environnements distincts. Les assertions intégrées aux corrigés vérifient les effectifs, recodages, jointures, calculs et exports. Une erreur provoque un échec de la vérification.

Les résultats sont écrits dans `resultats/verification/` : `bilan.csv`, `versions.csv` et `sessionInfo.txt`. Les avertissements sont enregistrés dans le bilan, même lorsque l'exécution réussit. Dans GitHub Actions, l'archive `verification-r` contient ces fichiers et les sorties des exercices pendant quatorze jours. Examiner également les tableaux HTML : une exécution sans erreur ne garantit ni leur lisibilité ni la pertinence statistique de tous les choix.

Les versions minimales sont indiquées dans `installation.R`. Le workflow teste les versions disponibles au moment de l'exécution et conserve leurs numéros ; il ne fige pas une installation identique sur tous les postes.

## Supports antérieurs

L'état complet du dépôt précédant cette série est conservé dans la branche [archives/avant-serie-01-2026-10-13](https://github.com/abdelghani-maddi/Cours_R_Sorbonne_U/tree/archives/avant-serie-01-2026-10-13), au commit `c52931a0ebe01498532a4e87da1ccdd838a1928e`.

Cette archive contient les anciens chapitres, exercices, corrections, examens et jeux de données. Ils ne sont pas dupliqués dans le parcours courant. Pour réemployer un support, vérifier ses données, ses chemins et ses fonctions, puis l'intégrer à une série identifiée plutôt que rétablir plusieurs entrées concurrentes.

[Accueil du cours](../README.md)
