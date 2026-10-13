# SERIE 01 - JOUR 04 - CORRIGE
library(dplyr)
library(tidyr)
library(tibble)
air_brut <- as_tibble(datasets::airquality) |> mutate(id_jour = row_number())
air_saisi <- air_brut |>
  mutate(Ozone = replace(Ozone, 1, 999), Solar.R = replace(Solar.R, 2, -999),
         Wind = replace(Wind, 4, -1))
mesures <- c("Ozone", "Solar.R", "Wind", "Temp")

# Q01. On compte les NA colonne par colonne ; n() est ici 153.
na_brut <- air_brut |> summarise(across(all_of(mesures), ~ sum(is.na(.x))))
pct_brut <- air_brut |> summarise(across(all_of(mesures), ~ 100 * sum(is.na(.x)) / n()))
print(na_brut)
print(pct_brut)
stopifnot(na_brut$Ozone == 37L, na_brut$Solar.R == 7L, na_brut$Wind == 0L)

# Q02. 999 et -999 sont des nombres ordinaires pour R tant qu'on ne les recode pas.
na_saisi <- air_saisi |> summarise(across(all_of(mesures), ~ sum(is.na(.x))))
extremes_saisis <- air_saisi |> summarise(across(
  all_of(mesures), list(min = ~ min(.x, na.rm = TRUE), max = ~ max(.x, na.rm = TRUE)),
  .names = "{.col}_{.fn}"
))
print(extremes_saisis)
stopifnot(identical(na_brut, na_saisi))

# Q03. Ces règles sont connues parce que l'énoncé documente les altérations.
anomalies <- air_saisi |>
  filter(Ozone == 999 | Solar.R == -999 | Wind < 0) |>
  select(id_jour, Month, Day, all_of(mesures))
print(anomalies)
stopifnot(setequal(anomalies$id_jour, c(1L, 2L, 4L)))

# Q04. Le journal conserve la valeur rejetée et la règle appliquée.
journal <- tibble(
  id_jour = c(1L, 2L, 4L), variable = c("Ozone", "Solar.R", "Wind"),
  valeur_saisie = c(999, -999, -1),
  action = c("Sentinelle 999 -> NA", "Sentinelle -999 -> NA", "Vitesse négative -> NA")
)
print(journal)
# Une valeur extrême n'est pas nécessairement une erreur : consulter la source,
# les unités et les conditions de mesure avant de supprimer une observation.

# Q05. La suppression concerne les valeurs, pas les journées entières.
air_propre <- air_saisi |> mutate(
  Ozone = na_if(Ozone, 999), Solar.R = na_if(Solar.R, -999),
  Wind = if_else(Wind < 0, NA_real_, Wind)
)
na_propre <- air_propre |> summarise(across(all_of(mesures), ~ sum(is.na(.x))))
stopifnot(na_propre$Ozone == 38L, na_propre$Solar.R == 8L, na_propre$Wind == 1L)

# Q06. Le calcul sans na.rm reste NA. La moyenne avec 999 n'est pas une moyenne
# valide d'ozone : la valeur sentinelle y est traitée comme une mesure.
comparaison_moyenne <- tibble(
  version = c("Brut", "Saisi", "Propre"),
  moyenne = c(mean(air_brut$Ozone, na.rm = TRUE),
              mean(air_saisi$Ozone, na.rm = TRUE), mean(air_propre$Ozone, na.rm = TRUE)),
  n_disponible = c(sum(!is.na(air_brut$Ozone)), sum(!is.na(air_saisi$Ozone)),
                   sum(!is.na(air_propre$Ozone)))
)
print(comparaison_moyenne)
stopifnot(is.na(mean(air_propre$Ozone)), comparaison_moyenne$n_disponible[3] == 115L)

# Q07. TRUE est converti en 1 et FALSE en 0 dans une moyenne numérique.
air_propre <- air_propre |> mutate(ozone_renseigne = !is.na(Ozone))
proportion_renseignee <- mean(air_propre$ozone_renseigne)
stopifnot(near(proportion_renseignee, 115 / 153))

# Q08. any = au moins une ; all = toutes. Les colonnes concernées sont nommées.
incomplets <- air_propre |> filter(if_any(all_of(mesures), is.na))
complets <- air_propre |> filter(if_all(all_of(mesures), ~ !is.na(.x)))
stopifnot(nrow(incomplets) + nrow(complets) == 153L)
stopifnot(length(intersect(incomplets$id_jour, complets$id_jour)) == 0L)

# Q09. La moyenne d'Ozone ne nécessite pas de supprimer un jour manquant sur Solar.R.
avec_ozone <- air_propre |> drop_na(Ozone)
avec_toutes <- air_propre |> drop_na(all_of(mesures))
print(tibble(selection = c("Ozone disponible", "Quatre mesures disponibles"),
             n = c(nrow(avec_ozone), nrow(avec_toutes)),
             pertes = 153L - c(nrow(avec_ozone), nrow(avec_toutes))))
stopifnot(nrow(avec_toutes) <= nrow(avec_ozone))

# Q10. pick() transmet les colonnes choisies à une fonction travaillant sur un tableau.
air_propre <- air_propre |> mutate(n_na = rowSums(is.na(pick(all_of(mesures)))))
print(count(air_propre, n_na))
stopifnot(sum(air_propre$n_na) == sum(unlist(na_propre)))

# Q11. Les fonctions anonymes rendent explicite le traitement des NA pour chaque calcul.
resume_mesures <- air_propre |> summarise(across(
  all_of(mesures), list(moyenne = ~ mean(.x, na.rm = TRUE),
                        mediane = ~ median(.x, na.rm = TRUE),
                        ecart_type = ~ sd(.x, na.rm = TRUE)),
  .names = "{.col}_{.fn}"
))
print(resume_mesures, width = Inf)
stopifnot(ncol(resume_mesures) == 12L)

# Q12. La standardisation ne s'applique pas à un identifiant ni au numéro du mois.
# Le sens de chaque z dépend de la variable et des observations disponibles.
air_z <- air_propre |> mutate(across(
  all_of(mesures), ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE),
  .names = "z_{.col}"
))
controle_z <- air_z |> summarise(across(
  starts_with("z_"), list(moyenne = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE))
))
print(controle_z)
stopifnot(all(abs(unlist(select(controle_z, ends_with("moyenne")))) < 1e-10))
stopifnot(all(abs(unlist(select(controle_z, ends_with("_sd")))) - 1 < 1e-10))
# Une colonne constante aurait un écart-type nul : il faudrait la traiter à part.

# Q13. rowwise() change la manière de calculer ; ungroup() retire ce comportement.
air_ligne <- air_propre |>
  rowwise() |>
  mutate(n_na_ligne = sum(is.na(c_across(all_of(mesures))))) |>
  ungroup()
stopifnot(identical(as.numeric(air_ligne$n_na_ligne), as.numeric(air_propre$n_na)))
# Une concentration, une vitesse et une température ne s'additionnent pas pour
# former une moyenne interprétable. Standardiser ne justifie pas à lui seul un indice.

# Q14. Le capteur principal a priorité lorsqu'il est renseigné.
capteurs <- tibble(principal = c(10, NA, 30, NA), secours = c(11, 20, NA, NA))
capteurs <- capteurs |> mutate(retenu = coalesce(principal, secours))
print(capteurs)
stopifnot(identical(capteurs$retenu, c(10, 20, 30, NA_real_)))
# La première ligne garde 10, pas la moyenne de 10 et 11 ; la dernière reste NA.

# Q15. Remplacer par zéro suppose que zéro représente réellement la valeur absente.
capteurs_zero <- capteurs |> replace_na(list(principal = 0))
print(c(disponibles = mean(capteurs$principal, na.rm = TRUE),
        apres_zero = mean(capteurs_zero$principal)))
stopifnot(near(mean(capteurs$principal, na.rm = TRUE), 20),
          near(mean(capteurs_zero$principal), 10))
# Ici le code modifie artificiellement la moyenne, de 20 à 10.

# Q16. fill() convient si le document de saisie utilisait des cellules fusionnées
# et si l'appartenance au groupe est bien connue pour les lignes suivantes.
identifiants <- tibble(groupe = c("A", NA, "B", NA), ligne = 1:4)
identifiants <- identifiants |> fill(groupe, .direction = "down")
stopifnot(identical(identifiants$groupe, c("A", "A", "B", "B")))
# La valeur d'ozone de la veille n'est pas une information sur la mesure absente du jour.

# Q17. Le bilan décrit les profils observés ; il ne prouve pas pourquoi une mesure manque.
air_propre <- air_propre |> mutate(complet = n_na == 0)
profils <- air_propre |> summarise(n = n(), temperature_moyenne = mean(Temp), .by = complet)
profils_mois <- air_propre |>
  summarise(n = n(), temperature_moyenne = mean(Temp), .by = c(Month, complet))
print(profils)
print(profils_mois)

# Q18. Une correction ciblée doit conserver les règles et les effectifs utilisés.
correction_na <- air_propre |> filter(!is.na(Ozone))
correction_moyenne <- air_propre |>
  summarise(n_total = n(), n_utilise = sum(!is.na(Ozone)),
            moyenne = mean(Ozone, na.rm = TRUE))
# Aucun across(everything(), replace_na(...)) ne remplace une décision variable
# par variable. Cela toucherait aussi des identifiants, des codes et des facteurs.

# Q19. Le format long rassemble les mesures dans une colonne, avec leur nom à côté.
# Cette forme sera reprise plus en détail au jour 06.
bilan_final <- air_propre |>
  select(all_of(mesures)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "valeur") |>
  summarise(n_total = n(), n_disponible = sum(!is.na(valeur)),
            n_manquant = sum(is.na(valeur)), pct_manquant = 100 * mean(is.na(valeur)),
            min = min(valeur, na.rm = TRUE), max = max(valeur, na.rm = TRUE),
            .by = variable)
print(bilan_final)
stopifnot(all(bilan_final$n_disponible + bilan_final$n_manquant == 153L))

# Q20. Les contrôles vérifient la qualité attendue, sans reconstituer les valeurs perdues.
stopifnot(nrow(air_propre) == nrow(air_brut))
stopifnot(n_distinct(air_propre$id_jour) == 153L)
stopifnot(!any(air_propre$Ozone == 999, na.rm = TRUE),
          !any(air_propre$Solar.R == -999, na.rm = TRUE))
stopifnot(all(air_propre$Wind >= 0, na.rm = TRUE))
stopifnot(identical(air_brut, as_tibble(datasets::airquality) |> mutate(id_jour = row_number())))
# Note possible : le fichier conserve les 153 journées. Trois valeurs ont été
# remplacées par NA selon des règles documentées, sans supprimer leurs autres
# mesures. Les moyennes sont calculées sur les données disponibles ; leur
# dénominateur est donc indiqué pour chaque variable. Pour une analyse associant
# les quatre mesures, les cas complets sont identifiés séparément. Ce choix
# entraîne une perte d'observations et peut modifier le profil des journées.
# La comparaison par température et par mois décrit cette différence, sans
# identifier à elle seule le mécanisme de non-réponse. Aucun remplacement
# automatique par zéro, par la moyenne ou par la mesure précédente n'est retenu.
# Les valeurs extrêmes plausibles sont conservées. Le fichier brut et le journal
# des anomalies permettent de relire chaque décision et de reprendre les calculs.
# Les effectifs annoncés dans les résultats doivent correspondre aux observations
# réellement utilisées, et non systématiquement à l'effectif initial du fichier.
