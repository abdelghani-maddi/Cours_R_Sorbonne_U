# SERIE 01 - JOUR 01 - CORRIGE
# Exécuter les blocs successivement. Les commentaires répondent aussi aux
# questions de lecture : une commande correcte ne suffit pas à les traiter.

library(dplyr)
library(tibble)
fleurs <- as_tibble(datasets::iris)
regions <- rownames_to_column(datasets::swiss, var = "region") |> as_tibble()

# Q01. 150 fleurs et cinq variables ; une mesure porte sur une fleur.
dim(fleurs)
names(fleurs)
head(fleurs)
stopifnot(identical(dim(fleurs), c(150L, 5L)))

# Q02. Les quatre mesures sont numériques ; Species est un facteur.
# Un facteur stocke des catégories et leur ordre, pas une mesure quantitative.
glimpse(fleurs)
class(fleurs)
class(fleurs$Species)

# Q03. L'aide décrit 47 régions francophones de Suisse, en 1888.
# Fertility est un indicateur standardisé de fécondité, pas un nombre d'enfants
# directement observé pour une personne. Les autres mesures sont des pourcentages.
# Les définitions exactes de leurs populations de référence figurent dans l'aide.
# Une association entre régions n'établit pas une relation au niveau individuel.
# Le dictionnaire complet est construit à Q20.

# Q04. On change les colonnes ; toutes les observations sont conservées.
sepales <- fleurs |> select(Species, Sepal.Length, Sepal.Width)
head(sepales)
stopifnot(nrow(sepales) == nrow(fleurs), ncol(sepales) == 3L)

# Q05. starts_with() impose le début du nom ; contains() cherche partout.
petales_1 <- fleurs |> select(starts_with("Petal"), Species)
petales_2 <- fleurs |> select(contains("Petal"), Species)
stopifnot(identical(petales_1, petales_2))

# Q06. $ signifie la fin du nom. \\. désigne un point littéral dans une
# expression régulière R ; un point non échappé désigne n'importe quel caractère.
longueurs_1 <- fleurs |> select(ends_with("Length"))
longueurs_2 <- fleurs |> select(matches("\\.Length$"))
stopifnot(identical(longueurs_1, longueurs_2), ncol(longueurs_1) == 2L)

# Q07. where() applique un test de type à chaque colonne.
numeriques <- fleurs |> select(where(is.numeric))
trois_mesures <- numeriques |> select(-Sepal.Width)
stopifnot(ncol(numeriques) == 4L, ncol(trois_mesures) == 3L)

# Q08. all_of() exige tous les noms ; any_of() ignore les noms absents.
choix <- c("Species", "Petal.Length")
selection_stricte <- fleurs |> select(all_of(choix))
choix <- c(choix, "absente")
selection_tolérante <- fleurs |> select(any_of(choix))
# Cette erreur est capturée : elle ne bloque pas l'exécution du corrigé.
erreur_all_of <- tryCatch(
  fleurs |> select(all_of(choix)),
  error = function(e) conditionMessage(e)
)
print(erreur_all_of)
stopifnot(identical(selection_stricte, selection_tolérante))
# any_of() n'est pas une correction universelle : il peut masquer une faute
# de frappe ou l'absence d'une variable indispensable à une analyse.

# Q09. : retient toutes les colonnes situées entre deux positions incluses.
par_intervalle <- fleurs |> select(Sepal.Width:Petal.Width)
par_noms <- fleurs |> select(Sepal.Width, Petal.Length, Petal.Width)
stopifnot(identical(par_intervalle, par_noms))
# L'ajout d'une colonne au milieu modifierait la sélection par intervalle.

# Q10. Dans iris, Species est la dernière colonne, mais son nom est plus stable.
derniere <- fleurs |> select(last_col())
espece_devant <- fleurs |> select(Species, everything())
stopifnot(identical(names(derniere), "Species"))

# Q11. Syntaxe : nouveau_nom = ancien_nom. Les valeurs ne sont pas traduites.
fleurs_fr <- fleurs |> rename(
  longueur_sepale = Sepal.Length, largeur_sepale = Sepal.Width,
  longueur_petale = Petal.Length, largeur_petale = Petal.Width
)
print(names(fleurs_fr))

# Q12. Une fonction anonyme reçoit ici un vecteur de noms.
fleurs_noms <- fleurs |>
  rename_with(tolower) |>
  rename_with(~ gsub(".", "_", .x, fixed = TRUE))
# fixed = TRUE évite de traiter le point comme une expression régulière.
stopifnot("sepal_length" %in% names(fleurs_noms))

# Q13. relocate() ne trie pas les lignes.
fleurs_ordre <- fleurs |>
  relocate(Species) |>
  relocate(Petal.Width, .after = Species)
stopifnot(identical(fleurs_ordre$Petal.Width, fleurs$Petal.Width))

# Q14. mean() attend un vecteur numérique, pas un tableau à une colonne.
longueur <- fleurs |> pull(Sepal.Length)
mean(longueur)
class(fleurs |> select(Sepal.Length))
class(longueur)
stopifnot(near(mean(longueur), 5.84333333333333))

# Q15. Les noms rendent le vecteur identifiable hors du tableau d'origine.
fecondite <- regions |> pull(Fertility, name = region)
head(fecondite, 5)
stopifnot(length(fecondite) == 47L, identical(names(fecondite), regions$region))

# Q16. Un tableau des modalités, un nombre, puis un tableau des effectifs.
modalites <- fleurs |> distinct(Species)
n_modalites <- n_distinct(fleurs$Species)
effectifs <- fleurs |> count(Species)
print(modalites)
print(effectifs)
stopifnot(n_modalites == 3L, all(effectifs$n == 50L))

# Q17. Trois corrections distinctes : sens du renommage, nom inexistant,
# et confusion tableau/vecteur. Aucune ne nécessite de modifier les données.
correction_1 <- fleurs |> rename(longueur_sepale = Sepal.Length)
correction_2 <- fleurs |> select(Species, Petal.Length)
correction_3 <- fleurs |> pull(Sepal.Length) |> mean()

# Q18. On vérifie un invariant de préparation, pas seulement l'absence d'erreur.
stopifnot(identical(fleurs, as_tibble(datasets::iris)))
stopifnot(nrow(sepales) == 150L, ncol(sepales) == 3L)

# Q19. Le travail est reproduit sur des données dont les noms sont différents.
regions_courtes <- regions |>
  select(region, Fertility, Education, Catholic) |>
  rename(fecondite = Fertility, education = Education, catholiques = Catholic) |>
  relocate(region)
education <- regions_courtes |> pull(education, name = region)
stopifnot(identical(dim(regions_courtes), c(47L, 4L)))
stopifnot(identical(names(regions_courtes),
                    c("region", "fecondite", "education", "catholiques")))
stopifnot(identical(unname(education), regions$Education))

# Q20. Dictionnaire fondé sur help("swiss", package = "datasets").
dictionnaire <- tibble(
  variable = names(datasets::swiss),
  definition = c(
    "Indicateur standardisé de fécondité Ig",
    "Part des hommes travaillant dans l'agriculture",
    "Part des conscrits obtenant la meilleure note à l'examen militaire",
    "Part des conscrits ayant une instruction au-delà du primaire",
    "Part de la population catholique",
    "Part des naissances vivantes décédées avant un an"
  ),
  unite = c("Indice", rep("Pourcentage", 5)),
  type_r = vapply(datasets::swiss, function(x) class(x)[1], character(1))
)
print(dictionnaire, width = Inf)
stopifnot(nrow(dictionnaire) == 6L)
# select() choisit les colonnes, rename() change leurs noms et pull() extrait
# un vecteur. Le contrôle des dimensions repère une sélection incomplète.
# Les contrôles d'égalité vérifient que les observations sont conservées.
# Renommer Education en education ne change ni ses valeurs ni sa définition.
