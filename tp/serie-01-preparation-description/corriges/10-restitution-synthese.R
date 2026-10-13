# SERIE 01 - JOUR 10 - CORRIGE
library(dplyr)
library(tidyr)
library(tibble)
library(labelled)
library(gtsummary)
library(gt)
theme_gtsummary_language(language = "fr")
voitures <- as_tibble(datasets::mtcars, rownames = "modele") |> mutate(
  transmission = factor(am, levels = c(0, 1), labels = c("Automatique", "Manuelle")),
  cyl_cat = factor(cyl, levels = c(4, 6, 8))
) |> set_variable_labels(mpg = "Distance parcourue (miles/gallon US)",
                         wt = "Poids (milliers de livres)", hp = "Puissance (hp bruts)",
                         cyl_cat = "Nombre de cylindres")

# Q01. Le cylindre est une catégorie ; son code numérique ne doit pas décider du résumé.
tbl_voitures <- voitures |> tbl_summary(by = transmission, include = c(mpg, wt, hp, cyl_cat),
  statistic = all_continuous() ~ "{mean} ({sd})", digits = all_continuous() ~ 1, missing = "no")

# Q02. show_header_names donne les noms techniques à utiliser dans les modifications.
show_header_names(tbl_voitures)
tbl_entetes <- tbl_voitures |> modify_header(
  label = "**Variable**", all_stat_cols() ~ "**{level}** (N = {n})"
)

# Q03. Le titre et l'en-tête englobant n'ajoutent aucune statistique.
tbl_titre <- tbl_entetes |>
  modify_spanning_header(all_stat_cols() ~ "**Transmission**") |>
  modify_caption("**Tableau 1. Caractéristiques des 32 voitures de mtcars**")

# Q04. replace = FALSE conserve la note expliquant moyenne (écart-type) et n (%).
tbl_note <- tbl_titre |> modify_footnote_header(
  footnote = "Voitures du fichier mtcars ; description non pondérée.",
  columns = all_stat_cols(), replace = FALSE
)

# Q05. rows cible une ligne précise du corps du tableau ; label cible sa cellule de libellé.
tbl_note <- tbl_note |> modify_footnote_body(
  footnote = "Une valeur plus élevée de mpg indique davantage de distance parcourue par gallon US.",
  columns = label, rows = variable == "mpg" & row_type == "label"
)

# Q06. La mise en forme doit rester au service de la lecture.
tbl_formate <- tbl_note |> bold_labels() |> italicize_levels()
print(tbl_formate)

# Q07. Masquer une colonne ne retire ni le groupe, ni ses observations, ni ses statistiques.
tbl_masque <- tbl_formate |> modify_column_hide(columns = stat_2)
tbl_reaffiche <- tbl_masque |> modify_column_unhide(columns = stat_2)
stopifnot(identical(tbl_reaffiche$table_body, tbl_formate$table_body))

# Q08. add_stat appelle la fonction une fois par variable sélectionnée.
# ... accepte les autres arguments transmis par gtsummary sans qu'on doive les utiliser.
n_valeurs <- function(data, variable, ...) {
  tibble(n_valeurs = n_distinct(data[[variable]], na.rm = TRUE))
}
stopifnot(n_valeurs(voitures, "cyl_cat")$n_valeurs == 3L)

# Q09. Le nombre porte sur les valeurs distinctes, pas sur le nombre de voitures.
tbl_stat_perso <- tbl_formate |>
  add_stat(fns = all_continuous() ~ n_valeurs) |>
  modify_header(n_valeurs = "**Valeurs distinctes**") |>
  modify_fmt_fun(n_valeurs = label_style_number(digits = 0))
print(tbl_stat_perso)

# Q10. Les mêmes variables et le même ordre permettent un rapprochement ligne à ligne.
tbl_global_moy <- voitures |> tbl_summary(include = c(mpg, wt, hp),
  statistic = all_continuous() ~ "{mean} ({sd})", digits = all_continuous() ~ 1)
tbl_global_med <- voitures |> tbl_summary(include = c(mpg, wt, hp),
  statistic = all_continuous() ~ "{median} [{p25} ; {p75}]", digits = all_continuous() ~ 1)
tbl_fusion <- tbl_merge(tbls = list(tbl_global_moy, tbl_global_med),
                       tab_spanner = c("**Moyenne et dispersion**", "**Médiane et quartiles**"))

# Q11. L'empilement réunit des sections avec les mêmes colonnes de groupes.
tbl_conso <- voitures |> tbl_summary(by = transmission, include = c(mpg, wt),
  statistic = all_continuous() ~ "{mean} ({sd})", digits = all_continuous() ~ 1, missing = "no")
tbl_moteur <- voitures |> tbl_summary(by = transmission, include = c(hp, cyl_cat),
  statistic = all_continuous() ~ "{mean} ({sd})", digits = all_continuous() ~ 1, missing = "no")
tbl_empile <- tbl_stack(tbls = list(tbl_conso, tbl_moteur),
                        group_header = c("Rendement et poids", "Moteur"))

# Q12. HTML conserve la mise en forme et s'ouvre dans un navigateur.
dossier <- "resultats/serie-01/j10"
dir.create(dossier, recursive = TRUE, showWarnings = FALSE)
gt::gtsave(as_gt(tbl_stat_perso), filename = file.path(dossier, "voitures.html"))
gt::gtsave(as_gt(tbl_fusion), filename = file.path(dossier, "voitures_deux_resumes.html"))
gt::gtsave(as_gt(tbl_empile), filename = file.path(dossier, "voitures_sections.html"))
stopifnot(file.exists(file.path(dossier, "voitures.html")))

# Q13. CSV contient les cellules affichées, pas les attributs de présentation de gt.
write.csv2(as_tibble(tbl_stat_perso), file.path(dossier, "voitures_tableau.csv"),
           row.names = FALSE, fileEncoding = "UTF-8")
# Le CSV du tableau est un export de présentation. Il ne contient pas les
# observations de départ et ne doit pas être réutilisé pour calculer des moyennes.

# Q14. RDS conserve les objets R et permet de reprendre leur traitement.
saveRDS(list(donnees = voitures, tableau = tbl_stat_perso), file.path(dossier, "voitures.rds"))
relecture <- readRDS(file.path(dossier, "voitures.rds"))
stopifnot(identical(relecture$donnees, voitures), inherits(relecture$tableau, "gtsummary"))

# Q15. Données régionales de Suisse francophone autour de 1888, pas enquête individuelle.
regions_brutes <- rownames_to_column(datasets::swiss, var = "region") |> as_tibble()
# La définition et la population de référence de chaque indicateur sont celles
# de help("swiss", package = "datasets"). Fertility est un indicateur standardisé.

# Q16. Une clé unique et des indicateurs dans l'étendue annoncée par la documentation.
diagnostic <- regions_brutes |> summarise(across(where(is.numeric),
  list(n_na = ~ sum(is.na(.x)), min = min, max = max), .names = "{.col}_{.fn}"))
print(diagnostic, width = Inf)
stopifnot(nrow(regions_brutes) == 47L, n_distinct(regions_brutes$region) == 47L)
stopifnot(!anyNA(regions_brutes), all(as.matrix(select(regions_brutes, -region)) >= 0),
          all(as.matrix(select(regions_brutes, -region)) <= 100))

# Q17. Les deux tables ont une seule ligne par région ; le contrôle l'exige.
indicateurs <- regions_brutes |> select(region, Fertility, Education, Agriculture)
contexte <- regions_brutes |> select(region, Catholic, Infant.Mortality, Examination)
regions <- indicateurs |> left_join(contexte, by = join_by(region), relationship = "one-to-one") |>
  select(all_of(names(regions_brutes)))
stopifnot(identical(regions, regions_brutes))

# Q18. Les intervalles sont fermés à gauche. Le facteur fixe l'ordre des comparaisons.
regions <- regions |> mutate(
  catholicite = factor(if_else(Catholic < 50, "Moins de 50 %", "Au moins 50 %"),
                       levels = c("Moins de 50 %", "Au moins 50 %")),
  education_cat = cut(Education, breaks = c(-Inf, 10, 20, Inf), right = FALSE,
                       labels = c("Moins de 10 %", "10 à moins de 20 %", "20 % et plus"))
)
print(count(regions, catholicite, education_cat, .drop = FALSE))
stopifnot(!anyNA(regions$catholicite), !anyNA(regions$education_cat))
stopifnot(identical(as.integer(cut(c(9, 10, 19, 20), c(-Inf, 10, 20, Inf), right = FALSE)),
                    c(1L, 2L, 2L, 3L)))

# Q19. Les libellés courts du tableau ne remplacent pas un dictionnaire complet.
regions <- regions |> set_variable_labels(
  Fertility = "Indicateur standardisé de fécondité", Education = "Conscrits instruits au-delà du primaire (%)",
  Agriculture = "Hommes dans l'agriculture (%)", Catholic = "Part de catholiques (%)",
  catholicite = "Part de catholiques, en deux classes", education_cat = "Instruction, en trois classes"
)
dictionnaire <- tibble(
  variable = c("region", "Fertility", "Education", "Agriculture", "Catholic", "catholicite", "education_cat"),
  definition = c("Région francophone de Suisse", "Indicateur standardisé Ig",
                 "Part des conscrits avec une instruction au-delà du primaire",
                 "Part des hommes travaillant dans l'agriculture", "Part de catholiques",
                 "Catholic < 50 ; Catholic >= 50", "Education < 10 ; [10,20[ ; >= 20"),
  unite = c("Identifiant régional", "Indice sur 0-100", "Pourcentage", "Pourcentage", "Pourcentage", "Catégorie", "Catégorie")
)

# Q20. Chaque région pèse une unité ; ce n'est pas une moyenne pondérée par population.
resume_regions <- regions |> summarise(n = n(), across(c(Fertility, Education, Agriculture),
  list(moyenne = mean, et = sd), .names = "{.col}_{.fn}"), .by = catholicite)
print(resume_regions)
stopifnot(near(weighted.mean(resume_regions$Fertility_moyenne, resume_regions$n), mean(regions$Fertility)))

# Q21. Les choix de résumé, les catégories et les effectifs sont indiqués.
tbl_regions <- regions |> tbl_summary(by = catholicite,
  include = c(Fertility, Education, Agriculture, education_cat),
  statistic = all_continuous() ~ "{mean} ({sd})", digits = all_continuous() ~ 1,
  missing = "ifany", missing_text = "Non renseigné"
) |> add_overall(last = TRUE) |> add_n() |>
  modify_header(label = "**Indicateur régional**") |>
  modify_caption("**Tableau 2. Indicateurs de 47 régions suisses, autour de 1888**") |>
  modify_footnote_header(footnote = "Régions non pondérées par leur population. Source : datasets::swiss.",
                         columns = all_stat_cols(), replace = FALSE) |> bold_labels()

# Q22. Les différences sont descriptives ; les IC requièrent un cadre probabiliste
# dont la pertinence ne découle pas du seul fait que la fonction les calcule.
tbl_regions_diff <- regions |> tbl_summary(by = catholicite,
  include = c(Fertility, Education, Agriculture),
  statistic = all_continuous() ~ "{mean} ({sd})", digits = all_continuous() ~ 1
) |> add_difference(test = all_continuous() ~ "t.test", levels = c("Moins de 50 %", "Au moins 50 %")) |>
  modify_caption("**Écarts régionaux : moins de 50 % moins au moins 50 % de catholiques**")
ecart_fecondite <- with(regions,
  mean(Fertility[catholicite == "Moins de 50 %"]) - mean(Fertility[catholicite == "Au moins 50 %"]))
est_table <- tbl_regions_diff$table_body |>
  filter(variable == "Fertility", row_type == "label") |> pull(estimate)
stopifnot(near(est_table, ecart_fecondite))
# Les régions ne sont pas ici décrites comme un échantillon aléatoire indépendant
# d'une population définie. Les liens spatiaux et historiques ne sont pas modélisés.

# Q23. On pivote les données numériques, pas les cellules déjà mises en forme.
resume_long <- regions |> select(region, catholicite, Fertility, Education, Agriculture) |>
  pivot_longer(c(Fertility, Education, Agriculture), names_to = "variable", values_to = "valeur") |>
  summarise(n = n(), moyenne = mean(valeur), et = sd(valeur), .by = c(variable, catholicite))
resume_large <- resume_long |> select(variable, catholicite, moyenne) |>
  pivot_wider(names_from = catholicite, values_from = moyenne)
print(resume_large)
stopifnot(nrow(resume_long) == 6L)
stopifnot(near((resume_long |> filter(variable == "Fertility", catholicite == "Moins de 50 %"))$moyenne,
              (resume_regions |> filter(catholicite == "Moins de 50 %"))$Fertility_moyenne))

# Q24. Le changement de seuil modifie la composition des groupes, pas seulement leur nom.
regions_75 <- regions |> mutate(groupe75 = if_else(Catholic < 75, "Moins de 75 %", "Au moins 75 %"))
n_changent <- sum(regions$Catholic >= 50 & regions$Catholic < 75)
resume_75 <- regions_75 |> summarise(n = n(), across(c(Fertility, Education, Agriculture), mean), .by = groupe75)
ecart_75 <- with(regions_75, mean(Fertility[Catholic < 75]) - mean(Fertility[Catholic >= 75]))
print(resume_75)
print(tibble(seuil = c(50, 75), ecart_fecondite = c(ecart_fecondite, ecart_75)))
# Le seuil doit être justifié par la question. Le choisir après examen des p-values
# pour obtenir un résultat désiré invalide l'interprétation ordinaire du test.

# Q25. Les fichiers de données, les calculs et les tableaux sont conservés séparément.
saveRDS(regions, file.path(dossier, "regions_analyse.rds"))
write.csv2(dictionnaire, file.path(dossier, "regions_dictionnaire.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv2(resume_regions, file.path(dossier, "regions_resumes.csv"), row.names = FALSE, fileEncoding = "UTF-8")
gt::gtsave(as_gt(tbl_regions), filename = file.path(dossier, "regions_description.html"))
gt::gtsave(as_gt(tbl_regions_diff), filename = file.path(dossier, "regions_ecarts.html"))
writeLines(capture.output(sessionInfo()), file.path(dossier, "sessionInfo.txt"))
# Les chiffres de la note sont calculés, pour éviter une divergence après modification.
n_groupe1 <- sum(regions$catholicite == "Moins de 50 %")
note <- c(
  "Indicateurs des régions suisses autour de 1888",
  "",
  sprintf("La comparaison porte sur 47 régions francophones de Suisse. %s régions comptent moins de 50 %% de catholiques, et %s au moins 50 %%. Chaque région reçoit le même poids ; le tableau ne décrit donc pas un individu moyen de la population suisse.", n_groupe1, 47 - n_groupe1),
  sprintf("Dans le sens moins de 50 %% moins au moins 50 %%, l'écart moyen de l'indicateur de fécondité est de %.2f points. La moyenne globale de l'indicateur est de %.2f. Les tableaux joints donnent également les moyennes et dispersions de l'instruction et de l'activité agricole. Il faut comparer ces écarts aux variations entre régions à l'intérieur de chaque groupe.", ecart_fecondite, mean(regions$Fertility)),
  sprintf("Porter le seuil de catholicité de 50 %% à 75 %% fait changer de groupe %s régions. L'écart de fécondité devient %.2f points. Cette comparaison de sensibilité renseigne sur la dépendance du résultat au découpage choisi. Elle ne sert pas à sélectionner le seuil donnant la plus petite p-value.", n_changent, ecart_75),
  "Les indicateurs ne portent pas tous sur la même population de référence : l'instruction concerne les conscrits et l'activité agricole les hommes. Les différences sont des associations entre régions, et non des effets établis au niveau individuel. Ni une direction causale, ni l'absence de facteurs communs ne peuvent être déduites du tableau. Les intervalles issus des tests supposent un cadre probabiliste qui n'est pas établi par ce seul fichier. La période historique, les dépendances possibles entre régions et l'absence de pondération par population limitent aussi les généralisations. Les données, recodages, dictionnaire et contrôles sont enregistrés avec les résultats pour permettre leur vérification."
)
writeLines(note, file.path(dossier, "regions_note.txt"), useBytes = TRUE)

# Q26. Un script autonome recrée ses données et ses répertoires de sortie.
stopifnot(n_distinct(regions$region) == 47L, sum(resume_regions$n) == 47L)
stopifnot(!anyNA(regions$catholicite), !anyNA(regions$education_cat))
stopifnot(near(est_table, ecart_fecondite))
stopifnot(identical(readRDS(file.path(dossier, "regions_analyse.rds")), regions))
fichiers_attendus <- c("regions_description.html", "regions_ecarts.html", "regions_note.txt",
                       "regions_analyse.rds", "regions_dictionnaire.csv", "regions_resumes.csv", "sessionInfo.txt")
stopifnot(all(file.exists(file.path(dossier, fichiers_attendus))))
# Révision à refaire sans corrigé : J02 Q05, J04 Q08, J06 Q03, J07 Q16, J08 Q14.
# Ne pas confondre réussite de cette exécution et maîtrise : la seconde se vérifie
# en écrivant le code à nouveau et en expliquant les résultats obtenus.
