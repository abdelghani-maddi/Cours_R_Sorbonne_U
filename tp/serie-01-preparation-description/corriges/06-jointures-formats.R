# SERIE 01 - JOUR 06 - CORRIGE
library(dplyr)
library(tidyr)
library(tibble)
longitudinal <- as_tibble(datasets::ChickWeight) |> mutate(Chick = as.character(Chick))
animaux <- longitudinal |> distinct(Chick, Diet)
mesures <- longitudinal |> select(Chick, Time, weight)

# Q01. 50 poussins et 578 mesures ; les tables n'ont pas la même unité de ligne.
stopifnot(nrow(animaux) == 50L, nrow(mesures) == 578L)
stopifnot(n_distinct(animaux$Chick) == 50L)

# Q02. Un poussin possède plusieurs mesures, mais une seule fiche de régime.
repetitions_poussin <- mesures |> count(Chick) |> filter(n > 1)
repetitions_cle <- mesures |> count(Chick, Time) |> filter(n > 1)
stopifnot(nrow(repetitions_poussin) > 0L, nrow(repetitions_cle) == 0L)
# Schéma : animaux [Chick unique] 1 ---- n mesures [Chick, Time uniques ensemble].

# Q03. Une mesure doit rencontrer au plus une ligne de la table animaux.
reconstitue <- mesures |> left_join(animaux, by = join_by(Chick), relationship = "many-to-one")
reconstitue <- reconstitue |> select(all_of(names(longitudinal)))
stopifnot(identical(reconstitue, longitudinal))

# Q04. left_join garde les mesures orphelines ; inner_join les élimine.
animaux_incomplets <- animaux |> filter(Chick != "1")
par_gauche <- mesures |> left_join(animaux_incomplets, by = join_by(Chick), relationship = "many-to-one")
par_intersection <- mesures |> inner_join(animaux_incomplets, by = join_by(Chick), relationship = "many-to-one")
n_absentes <- sum(mesures$Chick == "1")
stopifnot(nrow(par_gauche) == 578L, sum(is.na(par_gauche$Diet)) == n_absentes)
stopifnot(nrow(par_intersection) == 578L - n_absentes)

# Q05. Les jointures filtrantes gardent seulement les colonnes de la table de gauche.
orphelines <- mesures |> anti_join(animaux_incomplets, by = join_by(Chick))
reconnues <- mesures |> semi_join(animaux_incomplets, by = join_by(Chick))
stopifnot(all(orphelines$Chick == "1"), nrow(orphelines) + nrow(reconnues) == 578L)

# Q06. La fiche fictive n'a aucune mesure : son poids et sa date restent NA.
animaux_etendus <- bind_rows(animaux,
  tibble(Chick = "hors_fichier", Diet = factor("1", levels = levels(animaux$Diet))))
par_droite <- mesures |> right_join(animaux_etendus, by = join_by(Chick), relationship = "many-to-one")
par_union <- mesures |> full_join(animaux_etendus, by = join_by(Chick), relationship = "many-to-one")
gauche_etendue <- mesures |> left_join(animaux_etendus, by = join_by(Chick), relationship = "many-to-one")
print(par_union |> filter(is.na(weight)))
stopifnot(nrow(par_droite) == 579L, nrow(par_union) == 579L, nrow(gauche_etendue) == 578L)

# Q07. La correspondance des noms est précisée dans join_by().
animaux_renommes <- animaux |> rename(id_poussin = Chick)
cles_differentes <- mesures |>
  left_join(animaux_renommes, by = join_by(Chick == id_poussin), relationship = "many-to-one")
stopifnot(nrow(cles_differentes) == 578L, !anyNA(cles_differentes$Diet))

# Q08. Même identiques, deux lignes de référence produisent deux correspondances.
animaux_double <- bind_rows(animaux, slice(animaux, 1))
erreur_cle <- tryCatch({
  mesures |> left_join(animaux_double, by = join_by(Chick), relationship = "many-to-one")
  NULL
}, error = function(e) conditionMessage(e))
print(erreur_cle)
stopifnot(is.character(erreur_cle), length(erreur_cle) == 1L)

# Q09. On autorise la multiplication uniquement pour l'observer, pas pour la corriger.
multiplie <- mesures |>
  left_join(animaux_double, by = join_by(Chick), relationship = "many-to-many")
nb_mesures_doublees <- sum(mesures$Chick == animaux$Chick[1])
stopifnot(nrow(multiplie) == 578L + nb_mesures_doublees)
animaux_sans_copie <- animaux_double |> distinct()
repare <- mesures |>
  left_join(animaux_sans_copie, by = join_by(Chick), relationship = "many-to-one")
stopifnot(nrow(repare) == 578L)
# Avec deux régimes contradictoires pour un même Chick, distinct() laisserait
# deux lignes. Il faudrait résoudre le conflit à la source, pas garder la première.

# Q10. Une homonymie de colonne n'est pas une relation entre identifiants.
fiche_poids <- animaux |> mutate(weight = 100)
suffixes_defaut <- mesures |> left_join(fiche_poids, by = join_by(Chick), relationship = "many-to-one")
suffixes_explicites <- mesures |> left_join(
  fiche_poids, by = join_by(Chick), relationship = "many-to-one", suffix = c("_mesure", "_fiche")
)
print(names(suffixes_defaut))
print(names(suffixes_explicites))
# by ne doit pas être laissé implicite ici : weight n'est pas une clé de fiche.

# Q11. bind_cols colle les positions sans comparer les identifiants.
poids_initiaux <- mesures |> filter(Time == 0) |> select(Chick, weight) |> arrange(desc(Chick))
collage_faux <- bind_cols(animaux, poids_initiaux |> select(weight))
collage_juste <- animaux |> left_join(poids_initiaux, by = join_by(Chick), relationship = "one-to-one")
controle_collage <- tibble(Chick = animaux$Chick, poids_colle = collage_faux$weight,
                          poids_joint = collage_juste$weight) |> filter(poids_colle != poids_joint)
print(controle_collage)
stopifnot(nrow(controle_collage) > 0L)
# bind_cols n'est justifié que si l'alignement des lignes est établi et contrôlé.

# Q12. La provenance permet de documenter l'empilement.
reuni <- bind_rows(
  debut = mesures |> filter(Time <= 10), fin = mesures |> filter(Time > 10), .id = "periode"
)
reuni_controle <- reuni |> select(-periode) |> arrange(Chick, Time)
stopifnot(identical(reuni_controle, mesures |> arrange(Chick, Time)))

# Q13. On compare des ENSEMBLES de clés, pas les lignes complètes de mesure.
ids_0 <- mesures |> filter(Time == 0) |> distinct(Chick)
ids_21 <- mesures |> filter(Time == 21) |> distinct(Chick)
presents_aux_deux <- dplyr::intersect(ids_0, ids_21)
presents_au_moins_une_fois <- dplyr::union(ids_0, ids_21)
absents_21 <- dplyr::setdiff(ids_0, ids_21)
print(absents_21)
print(dplyr::setequal(ids_0, ids_21))
stopifnot(nrow(presents_aux_deux) + nrow(absents_21) == nrow(ids_0))

# Q14. Rectification fictive pour apprendre la commande, pas une correction de la source.
rectificatif <- mesures |> slice(1) |> mutate(weight = weight + 1)
mesures_mises_a_jour <- rows_update(mesures, rectificatif, by = c("Chick", "Time"))
stopifnot(sum(mesures_mises_a_jour$weight != mesures$weight) == 1L)
mesures_trou <- mesures |> mutate(weight = replace(weight, 1, NA_real_))
mesures_completees <- rows_patch(mesures_trou, rectificatif, by = c("Chick", "Time"))
stopifnot(identical(mesures_completees, mesures_mises_a_jour))
# rows_patch ne remplace que les valeurs manquantes ; rows_update peut écraser
# une valeur renseignée. Dans les deux cas, la clé complète est indispensable.

# Q15. Quatre mesures par fleur donnent 150 * 4 = 600 lignes.
fleurs <- as_tibble(datasets::iris) |> mutate(id_fleur = row_number(), .before = 1)
fleurs_long <- fleurs |> pivot_longer(
  cols = c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width),
  names_to = "mesure", values_to = "valeur"
)
stopifnot(nrow(fleurs_long) == 600L, n_distinct(fleurs_long$id_fleur) == 150L)

# Q16. Le séparateur est un point littéral ; on doit l'échapper dans l'expression régulière.
fleurs_detail <- fleurs |> pivot_longer(
  cols = c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width),
  names_to = c("organe", "dimension"), names_sep = "\\.", values_to = "valeur"
)
resume_fleurs <- fleurs_detail |> summarise(moyenne = mean(valeur), .by = c(Species, organe, dimension))
stopifnot(nrow(resume_fleurs) == 12L)

# Q17. id_fleur et Species identifient les lignes du tableau large.
fleurs_retour <- fleurs_detail |> pivot_wider(
  id_cols = c(id_fleur, Species), names_from = c(organe, dimension),
  names_sep = ".", values_from = valeur
) |> select(all_of(names(fleurs))) |> arrange(id_fleur)
stopifnot(identical(fleurs_retour, fleurs))

# Q18. La clé du format long est id_fleur + mesure, pas id_fleur seul.
fleurs_double <- bind_rows(fleurs_long, slice(fleurs_long, 1))
cles_dupliquees <- fleurs_double |> count(id_fleur, mesure) |> filter(n > 1)
print(cles_dupliquees)
stopifnot(nrow(cles_dupliquees) == 1L)
# values_fn = mean agrège les doublons. C'est légitime pour des répétitions
# dont on souhaite la moyenne, mais cela masquerait ici une erreur d'empilement.

# Q19. complete matérialise le croisement des clés et dates existantes.
calendrier <- mesures |> complete(Chick, Time)
combinaisons_attendues <- n_distinct(mesures$Chick) * n_distinct(mesures$Time)
stopifnot(nrow(calendrier) == combinaisons_attendues, nrow(calendrier) == 600L)
stopifnot(sum(is.na(calendrier$weight)) == 22L)
# Absence de mesure n'est pas poids nul. complete ne dit pas pourquoi elle manque.

# Q20. names_prefix rend les noms de colonnes lisibles et utilisables.
poids_larges <- calendrier |> pivot_wider(
  id_cols = Chick, names_from = Time, names_prefix = "jour_", values_from = weight
) |> left_join(animaux, by = join_by(Chick), relationship = "one-to-one")
stopifnot(nrow(poids_larges) == 50L, sum(is.na(poids_larges)) == 22L)

# Q21. Douze plantes, plusieurs concentrations par plante : les lignes sont répétées.
co2 <- as_tibble(datasets::CO2) |> mutate(Plant = as.character(Plant))
plantes <- co2 |> distinct(Plant, Type, Treatment)
mesures_co2 <- co2 |> select(Plant, conc, uptake)
stopifnot(nrow(plantes) == 12L, nrow(mesures_co2 |> distinct(Plant, conc)) == 84L)
co2_retour <- mesures_co2 |>
  left_join(plantes, by = join_by(Plant), relationship = "many-to-one") |>
  select(all_of(names(co2)))
stopifnot(identical(co2_retour, co2))

# Q22. Huit contrôles à consigner : unité de ligne ; clé ; type des clés ;
# unicité attendue ; absences de correspondance ; cardinalité de la relation ;
# effectif avant/après ; égalité des variables qui ne devaient pas changer.
stopifnot(nrow(animaux |> count(Chick) |> filter(n > 1)) == 0L)
stopifnot(nrow(mesures |> anti_join(animaux, by = join_by(Chick))) == 0L)
# Les assertions échoueraient respectivement après duplication d'une fiche
# et après suppression de la fiche d'un poussin possédant des mesures.
