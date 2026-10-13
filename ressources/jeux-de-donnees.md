# Jeux de données et unités d'observation

La documentation se consulte dans R avec `help("nom", package = "datasets")`. Lire au minimum les rubriques Description, Format et Details avant de renommer les variables ou de rédiger un résultat.

Les données sont appelées sous la forme `datasets::iris`, `datasets::mtcars`, etc. Un objet de travail est créé au début de chaque script ; les données d'origine ne sont pas remplacées.

| Données | Unité d'une ligne | Dimensions et contenu | Utilisation dans la série |
| --- | --- | --- | --- |
| iris | Une fleur | 150 fleurs ; quatre mesures de sépales et pétales en centimètres ; trois espèces | Sélection, recodages, formats long/large, comparaisons |
| mtcars | Un modèle de voiture | 32 voitures ; rendement, caractéristiques du moteur, poids et transmission | Filtres, facteurs, libellés et présentation des tableaux |
| airquality | Une journée | 153 journées de mai à septembre 1973 à New York ; mesures environnementales | Valeurs manquantes, groupes, dates et tableaux descriptifs |
| ToothGrowth | Un cobaye | 60 animaux ; longueur des odontoblastes, dose et mode d'administration de vitamine C | Variables étiquetées, comparaisons à dose fixée |
| ChickWeight | Une mesure d'un poussin à une date | 578 mesures sur 50 poussins ; poids, âge et régime | Mesures répétées, clés, jointures et calendrier incomplet |
| CO2 | Une mesure d'une plante à une concentration | 84 mesures sur 12 plantes ; absorption de CO2 | Réemploi des jointures et distinction individu/mesure |
| Titanic | Une cellule de tableau croisé | 32 combinaisons de classe, sexe, âge et survie ; effectif Freq | Comptage d'effectifs agrégés |
| PlantGrowth | Une plante | 30 observations dans trois groupes ; poids sec | Comparaison de deux groupes après filtrage |
| swiss | Une région | 47 régions francophones de Suisse autour de 1888 ; six indicateurs régionaux | Dictionnaire, description et dossier de synthèse |

## Voitures : mpg, poids et codes

Dans `mtcars`, `mpg` exprime des miles par gallon américain. Une valeur élevée indique davantage de distance parcourue pour un gallon. La conversion employée dans les exercices est `235.215 / mpg`, en litres pour 100 kilomètres. `wt` est exprimé en milliers de livres : le passage aux kilogrammes utilise `wt * 453.59237`. `hp` désigne une puissance brute en horsepower ; le libellé doit conserver cette précision.

Les codes `am = 0` et `am = 1` correspondent respectivement aux transmissions automatique et manuelle. Un code numérique ne suffit pas à décider qu'une variable doit être résumée par une moyenne.

## Mesures environnementales

Dans `airquality`, `Ozone` est mesuré en ppb, `Solar.R` en Langley, `Wind` en miles par heure et `Temp` en degrés Fahrenheit. Les énoncés précisent la conversion utilisée lorsqu'ils créent une température en degrés Celsius.

Le fichier d'origine comporte 37 valeurs manquantes d'Ozone et 7 de rayonnement solaire. Les sentinelles 999 et -999 et le vent négatif du jour 04 sont des altérations créées pour l'exercice. Le seuil d'Ozone supérieur à 50 sert à travailler un recodage ; il ne doit pas être présenté comme un seuil réglementaire.

## Expériences et mesures répétées

Dans `ToothGrowth`, les doses sont 0,5, 1 et 2 mg par jour. `supp` distingue le jus d'orange et l'acide ascorbique. L'aide décrit `len` comme une longueur d'odontoblastes ; elle ne fournit pas d'unité à ajouter au libellé. Les comparaisons de la série portent sur les animaux de cette expérience, pas sur une recommandation de supplémentation.

Dans `ChickWeight`, le poids est en grammes et `Time` en jours depuis la naissance. La clé d'une mesure est le couple poussin-date, alors que la fiche de régime a une ligne par poussin. Dans `CO2`, plusieurs concentrations sont mesurées pour une même plante. Les observations répétées d'un même individu ne sont pas des individus supplémentaires. Consulter les définitions de `conc` et `uptake` dans l'aide avant d'interpréter les mesures.

## Effectifs agrégés

`as.data.frame(Titanic)` produit 32 lignes, mais `sum(Freq)` donne 2 201 personnes. `Freq` contient des effectifs de cellules, pas des poids de sondage. La somme de la colonne est donc requise pour compter les personnes décrites.

## Indicateurs régionaux

Dans `swiss`, `Fertility` est l'indicateur standardisé de fécondité Ig. `Agriculture` décrit la part des hommes travaillant dans l'agriculture ; `Examination` et `Education` concernent les conscrits ; `Catholic` concerne la part de catholiques dans la population ; `Infant.Mortality` décrit la mortalité avant un an parmi les naissances vivantes.

Ces indicateurs ne doivent pas être interprétés comme les caractéristiques de 47 personnes. Les résumés de la série attribuent le même poids à chaque région. Une association régionale ne permet pas, à elle seule, d'établir la même relation entre individus. Les seuils de regroupement sont des choix d'analyse à documenter.

## Documentation d'origine

Les descriptions, unités et sources détaillées sont celles de l'aide du package `datasets` :

[iris](https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/iris.html) · [mtcars](https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/mtcars.html) · [airquality](https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/airquality.html) · [ToothGrowth](https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/ToothGrowth.html) · [ChickWeight](https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/ChickWeight.html) · [CO2](https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/zCO2.html) · [Titanic](https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/Titanic.html) · [PlantGrowth](https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/PlantGrowth.html) · [swiss](https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/swiss.html)

[Programme de la série 01](../tp/serie-01-preparation-description/README.md) · [Accueil du cours](../README.md)
