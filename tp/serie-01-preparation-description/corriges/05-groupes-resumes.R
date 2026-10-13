# SERIE 01 - JOUR 05 - CORRIGE
library(dplyr)
library(tibble)
air <- as_tibble(datasets::airquality)
voitures <- as_tibble(datasets::mtcars, rownames = "modele")
passagers <- as_tibble(as.data.frame(datasets::Titanic))

# Q01. count() regroupe temporairement et compte ; tally() compte les groupes existants.
effectifs <- air |> count(Month)
effectifs_tally <- air |> group_by(Month) |> tally() |> ungroup()
stopifnot(identical(effectifs, effectifs_tally), sum(effectifs$n) == 153L)

# Q02. n() compte les lignes ; sum(!is.na(Ozone)) compte les mesures utilisées.
resume_mois <- air |> group_by(Month) |>
  summarise(n_jours = n(), n_ozone = sum(!is.na(Ozone)),
            moyenne = mean(Ozone, na.rm = TRUE), mediane = median(Ozone, na.rm = TRUE),
            ecart_type = sd(Ozone, na.rm = TRUE), .groups = "drop")
print(resume_mois)
stopifnot(!is_grouped_df(resume_mois), sum(resume_mois$n_ozone) == 116L)

# Q03. .by ne persiste pas au-delà de l'opération ; arrange() fixe l'ordre comparé.
resume_by <- air |> summarise(
  n_jours = n(), n_ozone = sum(!is.na(Ozone)),
  moyenne = mean(Ozone, na.rm = TRUE), mediane = median(Ozone, na.rm = TRUE),
  ecart_type = sd(Ozone, na.rm = TRUE), .by = Month
)
stopifnot(isTRUE(all.equal(arrange(resume_mois, Month), arrange(resume_by, Month))))

# Q04. Les fonctions sont nommées : leur nom entre dans .names.
resume_mesures <- air |> summarise(across(
  c(Ozone, Solar.R, Wind, Temp),
  list(moyenne = ~ mean(.x, na.rm = TRUE), et = ~ sd(.x, na.rm = TRUE)),
  .names = "{.col}_{.fn}"
), .by = Month)
print(resume_mesures, width = Inf)
stopifnot(nrow(resume_mesures) == 5L, ncol(resume_mesures) == 9L)

# Q05. Le dénominateur varie selon Month. Il est calculé après le comptage.
air <- air |> mutate(temp_cat = if_else(Temp < 75, "Moins de 75", "75 et plus"))
par_mois <- air |> count(Month, temp_cat) |>
  mutate(pct_mois = 100 * n / sum(n), .by = Month)
controle_mois <- par_mois |> summarise(total = sum(pct_mois), .by = Month)
stopifnot(all(near(controle_mois$total, 100)))

# Q06. Ici le dénominateur est toujours 153, pas le nombre de jours du mois.
part_globale <- air |> count(Month, temp_cat) |> mutate(pct_total = 100 * n / sum(n))
stopifnot(near(sum(part_globale$pct_total), 100))
print(left_join(par_mois, part_globale, by = c("Month", "temp_cat", "n")))
# La jointure rassemble les deux calculs pour les comparer ; elle sera étudiée au jour 06.

# Q07. add_count répète l'effectif du groupe sur chaque ligne, sans agréger les mesures.
air_effectifs <- air |> add_count(Month, name = "n_mois")
air_tally <- air |> group_by(Month) |> add_tally(name = "n_mois") |> ungroup()
stopifnot(nrow(air_effectifs) == 153L, identical(air_effectifs, air_tally))

# Q08. .add = TRUE conserve Month en ajoutant temp_cat au groupement.
groupe_mois <- air |> group_by(Month)
groupe_deux <- groupe_mois |> group_by(temp_cat, .add = TRUE)
print(group_vars(groupe_deux))
stopifnot(identical(group_vars(groupe_deux), c("Month", "temp_cat")))
tableau_libre <- groupe_deux |> ungroup()
stopifnot(!is_grouped_df(tableau_libre))

# Q09. Les 32 lignes sont les cellules d'un tableau croisé à quatre variables.
# Freq donne l'effectif de personnes dans chaque cellule, y compris des zéros.
stopifnot(nrow(passagers) == 32L, sum(passagers$Freq) == 2201L)
print(head(passagers))

# Q10. Sans wt, on compte les cellules ; avec wt = Freq, on additionne les personnes.
cellules <- passagers |> count(Class, Survived, name = "n_cellules")
personnes <- passagers |> count(Class, Survived, wt = Freq, name = "n_personnes")
print(left_join(cellules, personnes, by = c("Class", "Survived")))
stopifnot(sum(personnes$n_personnes) == 2201L)

# Q11. Le dénominateur est la somme des personnes d'une classe.
survie_classe <- passagers |> summarise(
  n_personnes = sum(Freq), n_survivants = sum(Freq[Survived == "Yes"]), .by = Class
) |> mutate(pct_survivants = 100 * n_survivants / n_personnes)
print(survie_classe)
stopifnot(all(between(survie_classe$pct_survivants, 0, 100)))

# Q12. Freq contient des effectifs agrégés, pas des probabilités d'inclusion
# ni l'inverse de ces probabilités. Ne pas les appeler poids de sondage.
survivants_1 <- passagers |> filter(Survived == "Yes") |> summarise(n = sum(Freq))
survivants_2 <- passagers |> count(Survived, wt = Freq) |> filter(Survived == "Yes")
stopifnot(survivants_1$n == 711L, survivants_1$n == survivants_2$n)

# Q13. reframe autorise plusieurs lignes par groupe et restitue un tableau non groupé.
quartiles <- air |> reframe(
  probabilite = c(0.25, 0.50, 0.75),
  quantile = as.numeric(quantile(Ozone, probs = c(0.25, 0.50, 0.75), na.rm = TRUE, type = 2)),
  .by = Month
)
print(quartiles)
stopifnot(nrow(quartiles) == 15L, !is_grouped_df(quartiles))
# Le type de quantile doit être précisé si l'on compare différents logiciels
# ou tableaux. Le défaut de quantile() est type = 7 ; les p25/p75 de tbl_summary
# utilisent type = 2 dans la version employée par cette série.

# Q14. mutate conserve les observations, contrairement à summarise.
air_centre <- air |> mutate(ecart_mensuel = Temp - mean(Temp), .by = Month)
verification_centre <- air_centre |> summarise(ecart = mean(ecart_mensuel), .by = Month)
stopifnot(nrow(air_centre) == 153L, all(abs(verification_centre$ecart) < 1e-10))

# Q15. min_rank laisse des trous après les égalités, dense_rank non.
# row_number départage les ex aequo selon leur ordre d'apparition.
classements <- voitures |> mutate(
  rang_min = min_rank(desc(mpg)), rang_dense = dense_rank(desc(mpg)),
  rang_ligne = row_number(desc(mpg))
) |> arrange(desc(mpg), modele)
print(classements |> select(modele, mpg, starts_with("rang_")))
stopifnot(n_distinct(classements$rang_ligne) == 32L)

# Q16. ntile cherche des groupes d'effectifs proches, pas des intervalles
# séparant nécessairement les valeurs distinctes. Un ex aequo peut être coupé.
quart_voitures <- voitures |> mutate(quart_mpg = ntile(mpg, 4))
resume_quarts <- quart_voitures |>
  summarise(n = n(), min = min(mpg), max = max(mpg), .by = quart_mpg)
print(resume_quarts)
print(quart_voitures |> distinct(mpg, quart_mpg) |> count(mpg) |> filter(n > 1))
stopifnot(all(resume_quarts$n == 8L))

# Q17. Le tri précède lag. Le premier jour n'a pas de veille dans le fichier.
chronologie <- air |> arrange(Month, Day) |> mutate(
  temp_veille = lag(Temp), temp_lendemain = lead(Temp), ecart_veille = Temp - temp_veille
)
stopifnot(sum(is.na(chronologie$temp_veille)) == 1L)
# On conserve le premier NA ; lui substituer zéro créerait une variation artificielle.

# Q18. Un nouveau groupe redémarre le décalage, donc un NA par mois.
chronologie_mois <- air |> arrange(Month, Day) |> mutate(temp_veille = lag(Temp), .by = Month)
stopifnot(sum(is.na(chronologie_mois$temp_veille)) == 5L)
# La dernière journée du mois précédent n'appartient plus au groupe examiné.

# Q19. Ces calculs changent si les lignes sont permutées.
chronologie <- chronologie |> mutate(
  moyenne_cumulee = cummean(Temp), jours_chauds_cumules = cumsum(Temp >= 80)
)
stopifnot(near(last(chronologie$moyenne_cumulee), mean(air$Temp)))
stopifnot(last(chronologie$jours_chauds_cumules) == sum(air$Temp >= 80))

# Q20. Les fonctions first/last/nth ne devinent pas la chronologie.
positions_mois <- air |> arrange(Month, Day) |>
  summarise(premiere = first(Temp), derniere = last(Temp), troisieme = nth(Temp, 3), .by = Month)
print(positions_mois)
stopifnot(nrow(positions_mois) == 5L)

# Q21. Un poussin peut avoir moins de mesures qu'un autre. Une moyenne de
# toutes les lignes donne davantage de poids aux poussins mesurés plus souvent.
poussin <- as_tibble(datasets::ChickWeight)
effectifs_regime <- poussin |> summarise(n_mesures = n(), n_poussins = n_distinct(Chick), .by = Diet)
par_poussin <- poussin |> summarise(poids_moyen = mean(weight), n_mesures = n(), .by = c(Chick, Diet))
par_regime_individus <- par_poussin |> summarise(moyenne_des_poussins = mean(poids_moyen), .by = Diet)
par_regime_lignes <- poussin |> summarise(moyenne_des_mesures = mean(weight), .by = Diet)
print(effectifs_regime)
print(left_join(par_regime_individus, par_regime_lignes, by = "Diet"))
stopifnot(nrow(par_poussin) == 50L, sum(effectifs_regime$n_mesures) == 578L)
# Même la moyenne par poussin dépend de ses dates observées. Pour comparer
# les régimes à un âge donné, il faut sélectionner le même âge et étudier les absences.

# Q22. Retirer le groupement pour obtenir un dénominateur global.
pourcentage_global <- air |> count(Month, Day) |> ungroup() |> mutate(p = n / sum(n))
stopifnot(near(sum(pourcentage_global$p), 1))
# Note possible : un tableau peut contenir une ligne par journée, par individu,
# par mesure répétée ou par cellule d'effectifs. n() ne change pas de sens selon
# le contexte : il compte toujours les lignes du groupe courant. Le passage
# aux personnes exige n_distinct() pour un identifiant répété, ou sum(Freq)
# pour des effectifs agrégés. Un pourcentage doit préciser sa population de
# référence. Avec group_by(), cette référence reste le groupe jusqu'à ungroup().
# Avec .by, elle ne vaut que pour l'opération concernée. Les mesures manquantes
# imposent encore un autre dénominateur : le nombre d'observations disponibles
# pour la variable résumée. Enfin, les calculs ordonnés supposent un ordre
# explicite et une décision sur leurs éventuelles ruptures entre groupes.
