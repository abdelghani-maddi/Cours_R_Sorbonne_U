# SERIE 01 - JOUR 08 - CORRIGE
library(dplyr)
library(tibble)
library(labelled)
library(gtsummary)
# Ce réglage concerne la présentation ; il ne change pas les données.
theme_gtsummary_language(language = "fr")
air <- as_tibble(datasets::airquality) |> mutate(
  mois = factor(Month, levels = 5:9, labels = c("Mai", "Juin", "Juillet", "Août", "Septembre")),
  temp_c = (Temp - 32) * 5 / 9,
  ozone_fort = if_else(Ozone > 50, 1L, 0L, missing = NA_integer_)
) |> set_variable_labels(Ozone = "Ozone (ppb)", Solar.R = "Rayonnement solaire (Langley)",
                         Wind = "Vent (miles/h)", temp_c = "Température (°C)",
                         mois = "Mois", ozone_fort = "Ozone supérieur à 50 ppb")

# Q01. Un objet gtsummary contient les données de tableau et les règles de présentation.
tbl_initial <- air |> tbl_summary(include = c(Ozone, Solar.R, Wind, temp_c, mois, ozone_fort))
print(tbl_initial)
print(class(tbl_initial))
stopifnot(inherits(tbl_initial, "tbl_summary"), !is.data.frame(tbl_initial))

# Q02. Un numérique 0/1 est décrit par défaut comme dichotomique : seule la modalité 1.
# Les numériques ayant peu de valeurs distinctes peuvent être traités comme catégories.
tbl_deux_modalites <- air |> tbl_summary(include = ozone_fort, type = ozone_fort ~ "categorical")
print(tbl_deux_modalites)

# Q03. include et select délimitent le tableau ; air lui-même reste inchangé.
tbl_include <- air |> tbl_summary(include = c(Ozone, Wind, temp_c, mois))
tbl_select <- air |> select(Ozone, Wind, temp_c, mois) |> tbl_summary()
stopifnot(identical(as_tibble(tbl_include), as_tibble(tbl_select)))

# Q04. label surcharge le texte pour CE tableau sans changer le libellé stocké.
tbl_libelle <- air |> tbl_summary(include = Ozone, label = Ozone ~ "Concentration en ozone (ppb)")
stopifnot(identical(var_label(air$Ozone), "Ozone (ppb)"))

# Q05. all_continuous sélectionne les variables quantitatives du tableau.
tbl_moyennes <- air |> tbl_summary(
  include = c(Ozone, Wind, temp_c),
  statistic = all_continuous() ~ "{mean} ({sd})", digits = all_continuous() ~ 1
)
controle_moyenne <- air |> summarise(n = sum(!is.na(Ozone)),
                                    moyenne = mean(Ozone, na.rm = TRUE), et = sd(Ozone, na.rm = TRUE))
print(tbl_moyennes)
print(controle_moyenne)
stopifnot(controle_moyenne$n == 116L)

# Q06. La moyenne utilise toutes les valeurs ; la médiane partage les données
# ordonnées en deux. Écart-type et intervalle interquartile mesurent des dispersions
# différentes. Un écart-type n'est pas une incertitude sur la moyenne.
tbl_medianes <- air |> tbl_summary(include = c(Ozone, Wind, temp_c),
  statistic = all_continuous() ~ "{median} [{p25} ; {p75}]", digits = all_continuous() ~ 1)

# Q07. Le calcul des quartiles dépend de la convention d'interpolation.
q_type2 <- quantile(air$Ozone, c(0.25, 0.5, 0.75), na.rm = TRUE, type = 2)
q_type7 <- quantile(air$Ozone, c(0.25, 0.5, 0.75), na.rm = TRUE, type = 7)
print(rbind(type2 = q_type2, type7 = q_type7))
# Pour contrôler p25/p75 de ces tableaux, utiliser type = 2, pas le défaut type = 7.

# Q08. continuous2 produit plusieurs lignes ; statistic doit être un vecteur.
tbl_detail <- air |> tbl_summary(
  include = c(Ozone, Wind, temp_c), type = all_continuous() ~ "continuous2",
  statistic = all_continuous() ~ c("{mean} ({sd})", "{median} [{p25} ; {p75}]", "{min} ; {max}"),
  digits = all_continuous() ~ 1
)
print(tbl_detail)

# Q09. c(0, 1) correspond aux deux statistiques catégorielles : n puis p.
tbl_precision <- air |> tbl_summary(
  include = c(Ozone, Wind, temp_c, mois),
  statistic = all_continuous() ~ "{mean} ({sd})",
  digits = list(Ozone ~ 0, Wind ~ 2, temp_c ~ 1, all_categorical() ~ c(0, 1))
)

# Q10. Le paramètre missing change l'affichage, pas le tableau source.
tbl_na_si <- air |> tbl_summary(include = c(Ozone, temp_c), missing = "ifany")
tbl_na_toujours <- air |> tbl_summary(include = c(Ozone, temp_c), missing = "always")
tbl_na_masque <- air |> tbl_summary(include = c(Ozone, temp_c), missing = "no")
stopifnot(sum(is.na(air$Ozone)) == 37L, nrow(air) == 153L)

# Q11. Le pourcentage manquant utilise les observations totales de la variable.
tbl_na_pct <- air |> tbl_summary(include = c(Ozone, temp_c), missing = "always",
  missing_text = "Non renseigné", missing_stat = "{N_miss} ({p_miss}%)")
print(tbl_na_pct)
print(air |> summarise(n_manquant = sum(is.na(Ozone)), pct_manquant = 100 * mean(is.na(Ozone))))

# Q12. N d'en-tête décrit le groupe entier, pas le nombre disponible sur chaque variable.
tbl_mois <- air |> tbl_summary(by = mois, include = c(Ozone, Wind, temp_c, ozone_fort),
  statistic = all_continuous() ~ "{mean} ({sd})", missing_text = "Non renseigné")
print(count(air, mois))
stopifnot(sum((air |> count(mois))$n) == 153L)

# Q13. La modalité affichée et le libellé doivent désigner le même événement.
tbl_fort <- air |> tbl_summary(by = mois, include = ozone_fort,
  type = ozone_fort ~ "dichotomous", value = ozone_fort ~ 1,
  label = ozone_fort ~ "Ozone > 50 ppb")
tbl_faible <- air |> tbl_summary(by = mois, include = ozone_fort,
  type = ozone_fort ~ "dichotomous", value = ozone_fort ~ 0,
  label = ozone_fort ~ "Ozone <= 50 ppb")

# Q14. Le type catégoriel affiche les deux modalités et facilite le contrôle.
tbl_colonne <- air |> tbl_summary(by = mois, include = ozone_fort,
  type = ozone_fort ~ "categorical", percent = "column")
tbl_ligne <- air |> tbl_summary(by = mois, include = ozone_fort,
  type = ozone_fort ~ "categorical", percent = "row")
tbl_cellule <- air |> tbl_summary(by = mois, include = ozone_fort,
  type = ozone_fort ~ "categorical", percent = "cell")
frequences <- air |> filter(!is.na(ozone_fort)) |> count(mois, ozone_fort) |>
  mutate(pct_colonne = 100 * n / sum(n), .by = mois) |>
  mutate(pct_ligne = 100 * n / sum(n), .by = ozone_fort) |>
  mutate(pct_cellule = 100 * n / sum(n))
print(frequences)

# Q15. Les manquants sont signalés séparément, mais exclus des catégories observées.
controle_colonnes <- frequences |> summarise(somme = sum(pct_colonne), .by = mois)
controle_lignes <- frequences |> summarise(somme = sum(pct_ligne), .by = ozone_fort)
stopifnot(all(near(controle_colonnes$somme, 100)), all(near(controle_lignes$somme, 100)))
stopifnot(near(sum(frequences$pct_cellule), 100), sum(frequences$n) == 116L)

# Q16. sort agit sur les modalités décrites en lignes. Pour l'ordre de by,
# fixer les niveaux de son facteur dans le tableau source.
tbl_tri <- air |> tbl_summary(include = mois, sort = all_categorical() ~ "frequency")
print(tbl_tri)
stopifnot(identical(levels(air$mois), c("Mai", "Juin", "Juillet", "Août", "Septembre")))

# Q17. La moyenne globale pondère chaque observation disponible de la même manière,
# pas chaque moyenne mensuelle. Les effectifs disponibles varient selon le mois.
tbl_total_debut <- tbl_mois |> add_overall()
tbl_total_fin <- tbl_mois |> add_overall(last = TRUE)
mensuel <- air |> summarise(n = sum(!is.na(Ozone)), m = mean(Ozone, na.rm = TRUE), .by = mois)
stopifnot(near(weighted.mean(mensuel$m, mensuel$n), mean(air$Ozone, na.rm = TRUE)))
print(c(moyenne_globale = mean(air$Ozone, na.rm = TRUE), moyenne_des_moyennes = mean(mensuel$m)))

# Q18. N_nonmiss/N_obs fournit un contrôle variable par variable, tous groupes réunis.
tbl_effectifs <- tbl_total_fin |> add_n(statistic = "{N_nonmiss} / {N_obs}",
                                       col_label = "**Renseignés / total**")
print(tbl_effectifs)

# Q19. Le thème traduit les textes automatiques ; les libellés choisis restent à écrire.
theme_gtsummary_language(language = "fr")
tbl_statistiques <- tbl_moyennes |> add_stat_label(location = "column")
print(tbl_statistiques)

# Q20. Les observations sans groupe ne peuvent pas être affectées à une colonne by.
air_sans_mois <- air |> mutate(mois = replace(mois, 1, NA))
tbl_groupe_manquant <- air_sans_mois |>
  tbl_summary(by = mois, include = c(Ozone, temp_c), missing = "always")
stopifnot(sum(!is.na(air_sans_mois$mois)) == 152L)
# Pour un groupe explicite, il faudrait décider de transformer le NA du facteur
# en niveau, puis expliquer le sens de ce nouveau groupe. missing concerne les
# variables décrites en lignes ; ce n'est pas un recodage de la variable by.

# Q21. Les nombres mis en forme sont souvent réunis dans des chaînes de caractères.
cellules_affichees <- as_tibble(tbl_effectifs)
print(vapply(cellules_affichees, class, character(1)))
# Une cellule « moyenne (écart-type) » n'est plus une mesure numérique.
# Repartir des données pour calculer ou modifier une statistique.

# Q22. Trois doses peuvent être décrites comme modalités d'un protocole,
# ou comme une quantité numérique. Le choix automatique n'est pas une justification.
dents <- as_tibble(datasets::ToothGrowth)
tbl_dose_categories <- dents |> tbl_summary(include = dose, type = dose ~ "categorical")
tbl_dose_continue <- dents |> tbl_summary(include = dose, type = dose ~ "continuous",
  statistic = dose ~ "{mean} ({sd})")
print(tbl_dose_categories)
print(tbl_dose_continue)

# Q23. Toutes les décisions sont réunies dans une commande lisible.
tbl_final <- air |> tbl_summary(
  by = mois, include = c(Ozone, Wind, temp_c, ozone_fort),
  type = ozone_fort ~ "categorical",
  statistic = all_continuous() ~ "{mean} ({sd})", digits = all_continuous() ~ 1,
  missing = "ifany", missing_text = "Non renseigné", percent = "column"
) |> add_overall(last = TRUE) |> add_n(statistic = "{N_nonmiss} / {N_obs}")
controle_mai <- air |> filter(mois == "Mai") |> summarise(
  n_total = n(), n_ozone = sum(!is.na(Ozone)), moyenne_ozone = mean(Ozone, na.rm = TRUE),
  pct_fort = 100 * mean(ozone_fort, na.rm = TRUE)
)
print(tbl_final)
print(controle_mai)
stopifnot(controle_mai$n_total == 31L, controle_mai$n_ozone == 26L)

# Q24. L'unité est une région. Le seuil de Catholic porte sur sa population,
# pas sur la religion de chaque individu observé dans un échantillon de personnes.
regions <- as_tibble(datasets::swiss) |>
  mutate(groupe = factor(if_else(Catholic >= 50, "Au moins 50 %", "Moins de 50 %"),
                        levels = c("Moins de 50 %", "Au moins 50 %"))) |>
  set_variable_labels(Fertility = "Indicateur de fécondité", Education = "Instruction au-delà du primaire (%)",
                      Agriculture = "Hommes dans l'agriculture (%)", groupe = "Part de catholiques")
tbl_regions <- regions |> tbl_summary(by = groupe, include = c(Fertility, Education, Agriculture),
  statistic = all_continuous() ~ "{median} [{p25} ; {p75}]", digits = all_continuous() ~ 1,
  missing = "no") |> add_overall() |> add_n()
print(tbl_regions)
stopifnot(nrow(regions) == 47L)
# La description porte sur 47 régions à la fin du XIXe siècle. Les indicateurs
# ne décrivent pas exactement les mêmes populations : Education concerne les
# conscrits, Agriculture les hommes exerçant une activité agricole. Le tableau
# résume la distribution des indicateurs régionaux, pas celle des personnes.
# Il donne le même poids à chaque région ; les populations régionales ne sont
# pas utilisées comme pondérations. Le seuil de 50 % sert ici au regroupement.
# Un autre seuil pourrait changer les effectifs et les médianes des groupes.
# Une différence entre groupes de régions ne permet pas d'affirmer une relation
# identique entre individus, ni un effet causal de la religion sur la fécondité.
# L'interprétation doit mentionner les groupes comparés, la statistique retenue,
# la période des données et les limites d'une description agrégée.
