# SERIE 01 - JOUR 03 - CORRIGE
library(dplyr)
library(tibble)
library(forcats)
library(stringr)
voitures <- as_tibble(datasets::mtcars, rownames = "modele")

# Q01. Le calcul intermédiaire poids_kg est disponible dans le même mutate().
calculs <- voitures |> mutate(
  conso_l_100 = 235.215 / mpg,
  poids_kg = wt * 453.59237,
  puissance_par_tonne = hp / (poids_kg / 1000)
)
print(calculs |> select(modele, mpg, conso_l_100, poids_kg, puissance_par_tonne))
stopifnot(all(calculs$conso_l_100 > 0), all(calculs$poids_kg > 0))

# Q02. L'arrondi est réservé à l'affichage : l'accumulation d'arrondis
# intermédiaires peut modifier moyennes, classements et tests de seuil.
affichage <- calculs |> mutate(conso_l_100 = round(conso_l_100, 1))
print(head(affichage |> select(modele, conso_l_100)))
stopifnot(identical(calculs$mpg, voitures$mpg))

# Q03. used garde les colonnes employées dans les expressions et les nouvelles.
# none garde les nouvelles colonnes (et d'éventuelles colonnes de groupe).
colonnes_utilisees <- voitures |> mutate(
  conso_l_100 = 235.215 / mpg,
  poids_kg = wt * 453.59237,
  puissance_par_tonne = hp / (poids_kg / 1000),
  .keep = "used", .before = 1
)
colonnes_nouvelles <- voitures |> mutate(
  conso_l_100 = 235.215 / mpg,
  poids_kg = wt * 453.59237,
  puissance_par_tonne = hp / (poids_kg / 1000),
  .keep = "none"
)
print(names(colonnes_utilisees))
stopifnot(ncol(colonnes_nouvelles) == 3L)

# Q04. Le contrôle est indispensable : if_else(am == 0, ..., ...)
# classerait aussi un éventuel code 9 parmi les transmissions manuelles.
stopifnot(all(voitures$am %in% c(0, 1)))
calculs <- calculs |> mutate(transmission = if_else(am == 0, "Automatique", "Manuelle"))
print(count(calculs, am, transmission))

# Q05. NA reste manquant sauf si missing est fourni. Une modalité explicite
# devient une catégorie comptée, avec un dénominateur potentiellement différent.
x <- c(0, 1, NA)
sans_reponse <- if_else(x == 1, "Oui", "Non")
modalite_reponse <- if_else(x == 1, "Oui", "Non", missing = "Non renseigné")
print(tibble(x, sans_reponse, modalite_reponse))
stopifnot(is.na(sans_reponse[3]), !is.na(modalite_reponse[3]))

# Q06. On traite d'abord les NA, puis les intervalles du plus petit au plus grand.
modalites_hp <- c("Moins de 100", "100 à moins de 200", "200 et plus")
calculs <- calculs |> mutate(
  puissance_cat = case_when(
    is.na(hp) ~ NA_character_,
    hp < 100 ~ modalites_hp[1],
    hp < 200 ~ modalites_hp[2],
    TRUE ~ modalites_hp[3]
  ),
  puissance_cat = factor(puissance_cat, levels = modalites_hp)
)

# Q07. 100 appartient au deuxième intervalle ; 200 au troisième.
# La première condition vraie gagne : hp < 200 placé en tête absorbe hp < 100.
cas_limites <- tibble(hp = c(99, 100, 199, 200, NA_real_)) |>
  mutate(categorie = case_when(
    is.na(hp) ~ NA_character_, hp < 100 ~ modalites_hp[1],
    hp < 200 ~ modalites_hp[2], TRUE ~ modalites_hp[3]
  ))
print(cas_limites)
stopifnot(identical(cas_limites$categorie,
                    c(modalites_hp[1], modalites_hp[2], modalites_hp[2], modalites_hp[3], NA)))

# Q08. right = FALSE donne des intervalles fermés à gauche et ouverts à droite.
calculs <- calculs |> mutate(puissance_cut = cut(
  hp, breaks = c(-Inf, 100, 200, Inf), right = FALSE, labels = modalites_hp
))
stopifnot(identical(calculs$puissance_cat, calculs$puissance_cut))

# Q09. Une priorité est formulée explicitement pour faciliter sa relecture.
calculs <- calculs |> mutate(profil = case_when(
  is.na(hp) | is.na(mpg) ~ NA_character_,
  hp >= 200 ~ "Puissance >= 200",
  hp < 200 & mpg >= 25 ~ "Puissance < 200 et mpg >= 25",
  TRUE ~ "Autres"
))
# Ici les deux conditions explicites sont disjointes : les inverser ne change
# rien. En revanche, retirer hp < 200 de la deuxième peut créer un chevauchement
# dans un autre fichier. Il faut distinguer règle générale et données observées.

# Q10. Un seuil de travail n'est pas une norme technique. Le découpage perd
# les écarts à l'intérieur des classes et dépend de bornes choisies ici.
controle_hp <- calculs |> count(cyl, puissance_cat)
print(controle_hp)
stopifnot(sum(controle_hp$n) == 32L)

# Q11. as.numeric(facteur) restitue les positions internes, pas les codes 4,6,8.
calculs <- calculs |> mutate(
  cyl_facteur = factor(cyl, levels = c(4, 6, 8),
                       labels = c("4 cylindres", "6 cylindres", "8 cylindres")),
  transmission = factor(transmission, levels = c("Automatique", "Manuelle"))
)
print(calculs |> distinct(cyl, cyl_facteur) |> mutate(code_interne = as.numeric(cyl_facteur)))

# Q12. L'ordre des niveaux change ; les voitures restent les mêmes.
calculs <- calculs |> mutate(transmission_ref = fct_relevel(transmission, "Manuelle"))
print(levels(calculs$transmission_ref))
stopifnot(identical(as.character(calculs$transmission), as.character(calculs$transmission_ref)))

# Q13. fct_recode : nouveau = ancien. fct_collapse réunit plusieurs anciens niveaux.
calculs <- calculs |> mutate(
  transmission_courte = fct_recode(transmission, Auto = "Automatique", Manu = "Manuelle"),
  cyl_regroupe = fct_collapse(cyl_facteur, "6 ou 8 cylindres" = c("6 cylindres", "8 cylindres"))
)
print(count(calculs, cyl_facteur, cyl_regroupe))

# Q14. La fréquence décrit combien de voitures ; la moyenne décrit leur consommation.
calculs <- calculs |> mutate(
  cyl_frequence = fct_infreq(cyl_facteur),
  cyl_consommation = fct_reorder(cyl_facteur, conso_l_100, .fun = mean)
)
print(levels(calculs$cyl_frequence))
print(levels(calculs$cyl_consommation))

# Q15. Le filtrage ne retire pas automatiquement les niveaux vides.
quatre <- calculs |> filter(cyl == 4)
print(levels(quatre$cyl_facteur))
quatre <- quatre |> mutate(cyl_facteur = fct_drop(cyl_facteur))
stopifnot(nlevels(quatre$cyl_facteur) == 1L)
# Garder des niveaux vides permet de comparer des tableaux ayant la même nomenclature.

# Q16. Ici 8 et 4 cylindres sont les deux modalités les plus fréquentes.
calculs <- calculs |> mutate(cyl_deux = fct_lump_n(cyl_facteur, n = 2, other_level = "Autres"))
print(count(calculs, cyl_facteur, cyl_deux))
# Avec les réglages par défaut, des fréquences égales à la frontière peuvent
# conduire à conserver plus de n niveaux. Toujours regarder le résultat.

# Q17. str_squish retire les espaces aux extrémités et réduit les espaces répétés.
texte_sale <- c("  Automatique  ", "MANUELLE", " automatique", "Manuelle   ")
texte_propre <- str_to_lower(str_squish(texte_sale))
stopifnot(n_distinct(texte_propre) == 2L)

# Q18. Le texte du nom de modèle ne constitue pas une nomenclature des marques.
reperage <- voitures |>
  mutate(mercedes_ou_mazda = str_detect(modele, "^(Merc|Mazda)"),
         premier_mot = str_extract(modele, "^[^ ]+"))
print(reperage |> select(modele, premier_mot, mercedes_ou_mazda))
# Merc est une abréviation ; Hornet et Valiant montrent aussi les limites
# d'une identification du constructeur par le premier mot. Ne pas inventer
# une marque à partir de cette extraction seule.

# Q19. On garde modele pour pouvoir retrouver et contrôler la transformation.
reperage <- reperage |> mutate(modele_cle = str_replace_all(str_to_lower(modele), "\\s+", "_"))
stopifnot(n_distinct(reperage$modele_cle) == 32L)

# Q20. Les deux dimensions des pétales ont la même unité : leur rapport est sans unité.
fleurs <- as_tibble(datasets::iris) |>
  rename(espece = Species) |>
  mutate(
    rapport_petale = Petal.Length / Petal.Width,
    rapport_cat = cut(rapport_petale, breaks = c(-Inf, 3, 5, Inf),
                     right = FALSE, labels = c("Moins de 3", "3 à moins de 5", "5 et plus"))
  )
controle_fleurs <- fleurs |> count(espece, rapport_cat, .drop = FALSE)
print(controle_fleurs)
stopifnot(sum(controle_fleurs$n) == 150L, !anyNA(fleurs$rapport_cat))
controle_bornes <- cut(c(2.99, 3, 4.99, 5), c(-Inf, 3, 5, Inf), right = FALSE)
stopifnot(identical(as.integer(controle_bornes), c(1L, 2L, 2L, 3L)))

# Q21. Le dictionnaire porte sur les variables demandées et la source de calcul.
dictionnaire <- tibble(
  variable = c("conso_l_100", "poids_kg", "puissance_par_tonne", "puissance_cat", "rapport_petale", "rapport_cat"),
  regle = c("235.215 / mpg", "wt * 453.59237", "hp / (poids_kg / 1000)",
            "hp < 100 ; 100 <= hp < 200 ; hp >= 200", "Petal.Length / Petal.Width", "rapport < 3 ; 3 <= rapport < 5 ; rapport >= 5"),
  unite = c("L/100 km", "kg", "hp/tonne", "Catégorie", "Sans unité", "Catégorie"),
  source = c(rep("datasets::mtcars", 4), rep("datasets::iris", 2))
)
print(dictionnaire, width = Inf)

# Q22. Les deux sorties de if_else doivent avoir un type compatible.
correction_type <- if_else(voitures$am == 1, 1L, 0L)
correction_ordre <- case_when(
  is.na(voitures$hp) ~ NA_character_, voitures$hp < 100 ~ "Faible",
  voitures$hp < 200 ~ "Moyen", TRUE ~ "Fort"
)
stopifnot(identical(as.integer(voitures$am), correction_type))
# Consigner les NA, tester les valeurs aux bornes et vérifier les correspondances
# ancien/nouveau codage. Une catégorie manquante ne doit pas être assimilée à
# une réponse négative. Justifier les seuils par la question et leur définition,
# puis vérifier la sensibilité des résultats à un autre découpage raisonnable.
