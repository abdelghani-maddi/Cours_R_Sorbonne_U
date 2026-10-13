# SERIE 01 - JOUR 02 - CORRIGE
library(dplyr)
library(tibble)
air <- as_tibble(datasets::airquality) |> mutate(id_jour = row_number())
voitures <- as_tibble(datasets::mtcars, rownames = "modele")

# Q01. id_jour est un identifiant construit pour cet exercice ; Day seul
# n'est pas unique puisqu'un numéro de jour se répète à chaque mois.
stopifnot(nrow(air) == 153L, n_distinct(air$Month) == 5L)
stopifnot(n_distinct(air$id_jour) == nrow(air))

# Q02. %in% teste l'appartenance à un ensemble de valeurs.
mai <- air |> filter(Month == 5)
mai_juin_1 <- air |> filter(Month == 5 | Month == 6)
mai_juin_2 <- air |> filter(Month %in% c(5, 6))
stopifnot(nrow(mai) == 31L, nrow(mai_juin_1) == 61L)
stopifnot(identical(mai_juin_1, mai_juin_2))

# Q03. La virgule et & expriment ET. | exprime OU : sa sélection est plus large.
et_virgule <- air |> filter(Month == 5, Temp > 75)
et_symbole <- air |> filter(Month == 5 & Temp > 75)
ou_symbole <- air |> filter(Month == 5 | Temp > 75)
stopifnot(identical(et_virgule, et_symbole), nrow(ou_symbole) >= nrow(et_virgule))

# Q04. between() inclut les deux bornes.
intervalle <- air |> filter(between(Temp, 70, 80))
stopifnot(identical(intervalle, air |> filter(Temp >= 70, Temp <= 80)))

# Q05. & est prioritaire sur | : l'écriture sans parenthèses garde tout mai,
# même lorsque Ozone est faible ou manquant. Ce n'est pas la question posée.
filtre_voulu <- air |> filter((Month == 5 | Month == 6) & Ozone > 50)
filtre_different <- air |> filter(Month == 5 | Month == 6 & Ozone > 50)
print(tibble(voulu = nrow(filtre_voulu), sans_parentheses = nrow(filtre_different)))
stopifnot(nrow(filtre_different) > nrow(filtre_voulu))

# Q06. La négation porte sur le test d'appartenance entier.
autres_mois <- air |> filter(!(Month %in% c(5, 6)))
stopifnot(nrow(autres_mois) + nrow(mai_juin_1) == nrow(air))

# Q07. Une comparaison à NA donne NA, pas TRUE. filter() ne garde que TRUE.
n_manquants <- sum(is.na(air$Ozone))
jours_manquants <- air |> filter(is.na(Ozone))
comparaison_erronee <- air |> filter(Ozone == NA)
stopifnot(n_manquants == 37L, nrow(comparaison_erronee) == 0L)

# Q08. Les lignes manquantes n'appartiennent à aucun des deux filtres numériques.
fort <- air |> filter(Ozone > 50)
faible <- air |> filter(Ozone <= 50)
bilan_ozone <- tibble(
  situation = c("Ozone > 50", "Ozone <= 50", "Ozone manquant"),
  n = c(nrow(fort), nrow(faible), nrow(jours_manquants))
)
print(bilan_ozone)
stopifnot(sum(bilan_ozone$n) == 153L)

# Q09. On garde les NA explicitement ; cela ne leur attribue aucune valeur.
fort_ou_inconnu <- air |> filter(Ozone > 50 | is.na(Ozone))
stopifnot(nrow(fort_ou_inconnu) == nrow(fort) + 37L)

# Q10. La représentation informatique des décimaux introduit de petits écarts.
print(0.1 + 0.2 == 0.3)
print(near(0.1 + 0.2, 0.3))
stopifnot(near(0.1 + 0.2, 0.3))
# Pour Month == 5, on veut une égalité de code, pas une tolérance numérique.

# Q11. Sur un tableau local, arrange() place les NA à la fin du groupe de tri
# même avec desc(). Les colonnes de tri successives départagent les égalités.
air_trie <- air |> arrange(Month, desc(Ozone), Day)
print(head(air_trie, 10))
stopifnot(nrow(air_trie) == nrow(air))

# Q12. L'exclusion explicite des NA rend les deux intentions identiques.
top_1 <- air |> filter(!is.na(Ozone)) |> arrange(desc(Ozone)) |> slice_head(n = 5)
top_2 <- air |> slice_max(Ozone, n = 5, with_ties = FALSE, na_rm = TRUE)
print(top_1 |> select(Month, Day, Ozone))
stopifnot(setequal(top_1$id_jour, top_2$id_jour))

# Q13. Ici les petites valeurs comprennent des ex aequo. Conserver les ex aequo
# peut produire plus de trois lignes. Les supprimer exige une règle de départage.
bas_avec <- air |> slice_min(Ozone, n = 3, with_ties = TRUE, na_rm = TRUE)
bas_sans <- air |> slice_min(Ozone, n = 3, with_ties = FALSE, na_rm = TRUE)
print(bas_avec |> select(id_jour, Ozone))
stopifnot(nrow(bas_sans) == 3L, nrow(bas_avec) >= 3L)
# Règle explicite de départage : à Ozone égal, prendre le jour le plus ancien.
bas_dates <- air |>
  filter(!is.na(Ozone)) |>
  arrange(Ozone, Month, Day) |>
  slice_head(n = 3)

# Q14. slice() utilise les positions dans l'ordre ACTUEL du tableau.
positions <- air |> slice(c(1, 10, 20))
debut <- air |> slice_head(n = 3)
fin <- air |> slice_tail(n = 3)
stopifnot(identical(positions$id_jour, c(1L, 10L, 20L)))

# Q15. La graine est fixée juste avant chaque tirage. Ce contrôle ne dépend
# pas de la succession de tirages effectués auparavant dans la session.
set.seed(2026)
echantillon_1 <- air |> slice_sample(n = 8)
set.seed(2026)
echantillon_2 <- air |> slice_sample(n = 8)
stopifnot(identical(echantillon_1, echantillon_2))

# Q16. Trois copies exactes sont ajoutées : 156 lignes pour 153 identifiants.
air_double <- bind_rows(air, slice_head(air, n = 3))
doublons <- air_double |> count(id_jour) |> filter(n > 1)
sans_copies <- air_double |> distinct()
print(doublons)
stopifnot(nrow(air_double) == 156L, nrow(doublons) == 3L)
stopifnot(identical(sans_copies, air))

# Q17. Le conflit sur Temp n'est pas une copie exacte.
air_conflit <- bind_rows(air_double, air |> slice(1) |> mutate(Temp = 99))
unique_lignes <- air_conflit |> distinct()
premiere_par_id <- air_conflit |> distinct(id_jour, .keep_all = TRUE)
print(air_conflit |> filter(id_jour == 1))
stopifnot(nrow(unique_lignes) == 154L, nrow(premiere_par_id) == 153L)
# .keep_all conserve la PREMIERE observation rencontrée. Changer l'ordre
# pourrait conserver 99. Il faut consulter la source avant de décider.

# Q18. Un vecteur à droite de == est recyclé ; il ne signifie pas « parmi ».
correction_mois <- air |> filter(Month %in% c(5, 6))
correction_na <- air |> filter(!is.na(Ozone))
correction_max <- air |> arrange(desc(Ozone)) |> slice_head(n = 5)

# Q19. Chaque étape repart du résultat de la précédente.
etape_cyl <- voitures |> filter(cyl %in% c(4, 6))
etape_hp <- etape_cyl |> filter(hp < 150)
etape_mpg <- etape_hp |> filter(mpg > 20)
selection_voitures <- etape_mpg |>
  select(modele, cyl, hp, mpg) |>
  arrange(desc(mpg), modele)
bilan_voitures <- tibble(
  etape = c("Départ", "4 ou 6 cylindres", "Puis hp < 150", "Puis mpg > 20"),
  n = c(nrow(voitures), nrow(etape_cyl), nrow(etape_hp), nrow(etape_mpg))
)
print(bilan_voitures)
print(selection_voitures)
stopifnot(all(diff(bilan_voitures$n) <= 0))

# Q20. Les invariants portent sur les clés, les conditions et les données brutes.
stopifnot(n_distinct(selection_voitures$modele) == nrow(selection_voitures))
stopifnot(all(selection_voitures$cyl %in% c(4, 6)),
          all(selection_voitures$hp < 150), all(selection_voitures$mpg > 20))
stopifnot(identical(voitures, as_tibble(datasets::mtcars, rownames = "modele")))
seuil_80 <- air |> filter((Month == 5 | Month == 6) & Ozone > 80)
stopifnot(nrow(seuil_80) <= nrow(filtre_voulu))
# Un NA n'est ni une valeur faible, ni une valeur forte ; un filtre numérique
# l'écarte si aucune clause ne le conserve. Les effectifs perdus se comptent.
# Un doublon exact et deux mesures contradictoires ne se traitent pas pareil.
# Un classement avec ex aequo peut dépasser l'effectif annoncé.
# Enfin, l'ordre des filtres successifs change le bilan intermédiaire,
# mais pas leur intersection finale lorsque toutes les conditions sont fixes.
