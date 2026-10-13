# SERIE 01 - JOUR 07 - CORRIGE
library(dplyr)
library(tibble)
library(labelled)
library(haven)
voitures <- as_tibble(datasets::mtcars, rownames = "modele")

# Q01. am est le nom informatique ; 0 et 1 sont les codes ; Automatique et
# Manuelle en donnent le sens. « Transmission » décrit la variable entière.
print(count(voitures, am))
stopifnot(all(voitures$am %in% c(0, 1)))

# Q02. var_label ajoute un attribut ; le nom mpg et ses valeurs restent inchangés.
var_label(voitures$mpg) <- "Distance parcourue (miles/gallon US)"
print(var_label(voitures$mpg))
stopifnot("mpg" %in% names(voitures), identical(as.numeric(voitures$mpg), datasets::mtcars$mpg))

# Q03. Le sens et l'unité doivent être suffisamment précis pour lire un tableau seul.
voitures <- voitures |> set_variable_labels(
  hp = "Puissance du moteur (hp bruts)", wt = "Poids (milliers de livres)",
  am = "Transmission", cyl = "Nombre de cylindres", modele = "Modèle de voiture"
)
variables_etiquetees <- c("modele", "mpg", "hp", "wt", "am", "cyl")
dictionnaire <- tibble(
  variable = variables_etiquetees,
  libelle = vapply(variables_etiquetees, function(v) var_label(voitures[[v]]), character(1))
)
print(dictionnaire)

# Q04. rename modifie le nom, pas le libellé associé au vecteur.
renomme <- voitures |> rename(rendement = mpg)
stopifnot(identical(var_label(renomme$rendement), var_label(voitures$mpg)))

# Q05. Les étiquettes n'effacent pas les codes numériques.
val_labels(voitures$am) <- c(Automatique = 0, Manuelle = 1)
print(voitures$am)
print(val_labels(voitures$am))
print(class(voitures$am))
stopifnot(identical(as.numeric(voitures$am), datasets::mtcars$am))

# Q06. La syntaxe en pipeline attribue les étiquettes à plusieurs colonnes.
voitures_2 <- as_tibble(datasets::mtcars, rownames = "modele") |>
  set_value_labels(am = c(Automatique = 0, Manuelle = 1),
                   vs = c("Moteur en V" = 0, "Moteur en ligne" = 1))
stopifnot(identical(val_labels(voitures_2$am), val_labels(voitures$am)))

# Q07. labelled réunit valeurs, libellés de modalités et libellé de variable.
transmission_code <- labelled(datasets::mtcars$am,
  labels = c(Automatique = 0, Manuelle = 1), label = "Transmission")
stopifnot(identical(as.numeric(transmission_code), datasets::mtcars$am))
stopifnot(identical(unname(val_labels(transmission_code)), c(0, 1)))

# Q08. Le setter cible un seul libellé ; il ne recode aucune voiture.
transmission_texte <- transmission_code
print(val_label(transmission_texte, 1))
val_label(transmission_texte, 1) <- "Boîte manuelle"
stopifnot(identical(as.numeric(transmission_texte), as.numeric(transmission_code)))

# Q09. Une modalité peut être documentée sans être observée.
avec_code_9 <- add_value_labels(transmission_code, .labels = c("Non renseigné" = 9))
stopifnot(9 %in% unname(val_labels(avec_code_9)), !any(as.numeric(avec_code_9) == 9))

# Q10. Le facteur utilise les libellés pour les catégories ; les codes d'origine
# restent disponibles dans transmission_code. Ne pas écraser cet objet pendant le contrôle.
transmission_facteur <- to_factor(transmission_code)
print(tibble(code = as.numeric(transmission_code), modalite = as.character(transmission_facteur)))
stopifnot(is.factor(transmission_facteur), nlevels(transmission_facteur) == 2L)

# Q11. Les préfixes permettent de voir code et sens dans une même modalité.
transmission_prefixee <- to_factor(transmission_code, levels = "prefixed")
transmission_ordonnee <- to_factor(transmission_code, sort_levels = "values")
print(levels(transmission_prefixee))
print(levels(transmission_ordonnee))
stopifnot(identical(levels(transmission_ordonnee), c("Automatique", "Manuelle")))

# Q12. On retire les étiquettes de modalités, pas nécessairement le libellé de variable.
sans_modalites <- remove_val_labels(transmission_code)
print(class(sans_modalites))
print(var_label(sans_modalites))
stopifnot(is.null(val_labels(sans_modalites)))
stopifnot(identical(as.numeric(sans_modalites), datasets::mtcars$am))

# Q13. Une étiquette de variable seule ne transforme pas une mesure en catégorie.
pour_analyse <- voitures_2 |> select(am, vs, mpg, hp) |> to_factor()
stopifnot(is.factor(pour_analyse$am), is.factor(pour_analyse$vs),
          is.numeric(pour_analyse$mpg), is.numeric(pour_analyse$hp))

# Q14. Sans règle supplémentaire, le code 2 reste visible comme modalité non étiquetée.
x_incomplet <- labelled(c(0, 1, 2, NA), labels = c(Automatique = 0, Manuelle = 1))
conversion_complete <- to_factor(x_incomplet)
conversion_perte <- to_factor(x_incomplet, nolabel_to_na = TRUE)
conversion_stricte <- to_factor(x_incomplet, strict = TRUE)
print(conversion_complete)
print(conversion_perte)
print(conversion_stricte)
stopifnot(sum(is.na(conversion_complete)) == 1L, sum(is.na(conversion_perte)) == 2L)
stopifnot(inherits(conversion_stricte, "haven_labelled"))
# strict conserve le vecteur étiqueté si tous les codes ne sont pas documentés.
# nolabel_to_na fait disparaître le code 2 : il faut d'abord savoir ce qu'il signifie.

# Q15. La classe SPSS déclare 9 manquant, bien que 9 reste stocké numériquement.
x_spss <- haven::labelled_spss(c(0, 1, 9, NA),
  labels = c(Automatique = 0, Manuelle = 1, "Non renseigné" = 9),
  na_values = 9, label = "Transmission")
print(na_values(x_spss))
print(val_labels(x_spss))
print(is.na(x_spss))
print(as.numeric(x_spss))
stopifnot(sum(is.na(x_spss)) == 2L)

# Q16. zap_missing convertit les manquants déclarés en NA de R.
x_nettoye <- haven::zap_missing(x_spss)
x_facteur <- to_factor(x_nettoye, drop_unused_labels = TRUE)
stopifnot(sum(is.na(x_nettoye)) == 2L, nlevels(x_facteur) == 2L)
print(x_facteur)

# Q17. La conversion numérique brute perd la règle SPSS et fait entrer 9 dans le calcul.
moyennes_codes <- c(brute = mean(as.numeric(x_spss), na.rm = TRUE),
                    nettoyee = mean(as.numeric(x_nettoye), na.rm = TRUE))
print(moyennes_codes)
stopifnot(near(unname(moyennes_codes[2]), 0.5))
# Même 0.5 n'est pas une « transmission moyenne ». Pour des codes 0/1 bien
# définis, la moyenne a seulement l'interprétation de proportion du code 1.

# Q18. look_for cherche dans les noms et les libellés. Les opérations
# arithmétiques peuvent conserver des attributs : il faut vérifier leur sens.
print(look_for(voitures, "moteur"))
voitures <- voitures |> mutate(hp_double = 2 * hp)
print(var_label(voitures$hp_double))
var_label(voitures$hp_double) <- "Deux fois la puissance du moteur (hp bruts)"
stopifnot(identical(var_label(voitures$hp_double), "Deux fois la puissance du moteur (hp bruts)"))

# Q19. RDS conserve la structure R, dont les attributs et les facteurs.
dossier <- "resultats/serie-01/j07"
dir.create(dossier, recursive = TRUE, showWarnings = FALSE)
saveRDS(voitures, file.path(dossier, "voitures_etiquetees.rds"))
relues_rds <- readRDS(file.path(dossier, "voitures_etiquetees.rds"))
stopifnot(identical(relues_rds, voitures))

# Q20. CSV transporte des cellules de texte, pas tous les attributs R.
write.csv(voitures, file.path(dossier, "voitures.csv"), row.names = FALSE, fileEncoding = "UTF-8")
relues_csv <- read.csv(file.path(dossier, "voitures.csv"), fileEncoding = "UTF-8", check.names = FALSE)
write.csv(dictionnaire, file.path(dossier, "dictionnaire.csv"), row.names = FALSE, fileEncoding = "UTF-8")
stopifnot(nrow(relues_csv) == 32L, is.null(var_label(relues_csv$mpg)))

# Q21. len mesure la longueur des odontoblastes ; l'aide ne donne pas ici
# d'unité à reproduire dans le libellé. dose est en mg par jour.
dents <- as_tibble(datasets::ToothGrowth) |>
  mutate(supp_code = if_else(supp == "OJ", 1L, 2L)) |>
  set_value_labels(supp_code = c("Jus d'orange" = 1, "Acide ascorbique" = 2)) |>
  set_variable_labels(len = "Longueur des odontoblastes (len)",
                      dose = "Dose de vitamine C (mg/jour)", supp_code = "Mode d'administration") |>
  mutate(supp_fr = to_factor(supp_code))
print(count(dents, supp, supp_code, supp_fr))
stopifnot(all(as.numeric(dents$supp_code[dents$supp == "OJ"]) == 1))
stopifnot(all(as.character(dents$supp_fr[dents$supp == "VC"]) == "Acide ascorbique"))

# Q22. On garde le tableau documenté et une version d'analyse avec des classes adaptées.
analyse_dents <- dents |> select(len, dose, supp_fr)
var_label(analyse_dents$supp_fr) <- "Mode d'administration"
dictionnaire_dents <- tibble(
  variable = names(analyse_dents),
  libelle = vapply(analyse_dents, function(x) var_label(x), character(1))
)
print(dictionnaire_dents)
stopifnot(nrow(analyse_dents) == 60L, is.factor(analyse_dents$supp_fr))
# Conserver une copie des codes importés. Lire le dictionnaire avant de recoder.
# Distinguer absence d'étiquette et absence de réponse. Repérer les manquants
# déclarés, puis décider de leur conversion. Transformer les variables
# catégorielles en facteurs avant les tableaux ou les modèles qui l'exigent.
# Garder numériques les mesures quantitatives. Vérifier l'ordre des modalités.
# Contrôler les correspondances après conversion. Mettre à jour les libellés
# des variables dérivées. Enregistrer les objets documentés en RDS et joindre
# un dictionnaire aux fichiers de cellules. Ces opérations doivent être
# consignées dans le script, pas effectuées uniquement dans le tableur.
