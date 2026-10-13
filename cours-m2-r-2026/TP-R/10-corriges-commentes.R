# TP R - Préparation des données et tableaux descriptifs
# Sorbonne Université - M2 Sociologie
#
# Utilisation : ouvrir ce fichier dans RStudio, puis exécuter les lignes
# progressivement avec Ctrl + Entrée (Cmd + Entrée sur Mac).
# Aucun fichier externe, aucun document Quarto n'est nécessaire.
# Installation (une seule fois) :
# install.packages(c("dplyr", "tibble", "labelled", "gtsummary"))
# Charger les bibliothèques au début de chaque séance :
library(dplyr)
library(tibble)
library(labelled)
library(gtsummary)

# Corrigés : exécuter un exercice à la fois. Chaque exercice recrée df.

# ====================================================================
# EXERCICE 01 - Explorer un tableau et sélectionner des colonnes
# Données : iris | Fonctions : glimpse(), select(), starts_with()
# ====================================================================
# 1. Afficher les six premières lignes et la structure des variables.
# 2. Conserver Sepal.Length, Sepal.Width et Species.
# 3. Sélectionner toutes les mesures des pétales à l'aide de starts_with().

data(iris)
df <- tibble::as_tibble(iris)
head(df)                         # Premières observations
glimpse(df)                       # Types des cinq variables
mesures_sepales <- df |> select(Sepal.Length, Sepal.Width, Species)
mesures_petales <- df |> select(starts_with("Petal"), Species)
head(mesures_sepales)
head(mesures_petales)
# select() retient des colonnes ; il ne supprime aucune observation.

# ====================================================================
# EXERCICE 02 - Filtrer des observations
# Données : mtcars | Fonctions : filter(), select(), %in%
# ====================================================================
# 1. Repérer les voitures comportant quatre cylindres.
# 2. Parmi elles, conserver celles dont la consommation mpg dépasse 25.
# 3. Retenir les voitures à quatre ou six cylindres puis afficher mpg, cyl et hp.

data(mtcars)
df <- tibble::as_tibble(mtcars, rownames = "modele")
quatre_cylindres <- df |> filter(cyl == 4)
economes <- df |> filter(cyl == 4, mpg > 25)
quatre_ou_six <- df |>
  filter(cyl %in% c(4, 6)) |>
  select(modele, mpg, cyl, hp)
nrow(quatre_cylindres)
nrow(economes)
head(quatre_ou_six)
# Une virgule dans filter() signifie ET ; %in% permet de tester plusieurs valeurs.

# ====================================================================
# EXERCICE 03 - Renommer et déplacer des colonnes
# Données : iris | Fonctions : rename(), relocate()
# ====================================================================
# 1. Renommer Sepal.Length en longueur_sepale et Sepal.Width en largeur_sepale.
# 2. Placer Species en première position.
# 3. Vérifier les noms des colonnes sans modifier le tableau iris d'origine.

data(iris)
df <- tibble::as_tibble(iris)
df_prepare <- df |>
  rename(longueur_sepale = Sepal.Length,
         largeur_sepale = Sepal.Width) |>
  relocate(Species)
names(df_prepare)
names(df)
# Dans rename(), le nouveau nom se place à gauche du signe =.

# ====================================================================
# EXERCICE 04 - Créer et recoder des variables
# Données : mtcars | Fonctions : mutate(), if_else(), case_when()
# ====================================================================
# 1. Créer consommation_l_100km à partir de mpg avec la formule 235.215 / mpg.
# 2. Créer une variable automatique indiquant si am vaut 0.
# 3. Classer la puissance hp en trois catégories : moins de 100, 100 à 199, 200 et plus.

data(mtcars)
df <- tibble::as_tibble(mtcars, rownames = "modele")
df_prepare <- df |>
  mutate(
    consommation_l_100km = 235.215 / mpg,
    automatique = if_else(am == 0, "Oui", "Non"),
    puissance = case_when(
      hp < 100 ~ "Moins de 100",
      hp < 200 ~ "100 à 199",
      TRUE ~ "200 et plus"
    )
  )
count(df_prepare, puissance)
head(select(df_prepare, modele, mpg, consommation_l_100km, automatique))
# L'ordre des conditions dans case_when() compte : on teste d'abord hp < 100.

# ====================================================================
# EXERCICE 05 - Valeurs manquantes, tri et résumé
# Données : airquality | Fonctions : is.na(), filter(), arrange(), summarise()
# ====================================================================
# 1. Compter les valeurs manquantes de Ozone.
# 2. Écarter les observations sans Ozone.
# 3. Trouver les cinq journées aux concentrations d'ozone les plus élevées et calculer moyenne et médiane.

data(airquality)
df <- tibble::as_tibble(airquality)
sum(is.na(df$Ozone))
avec_ozone <- df |> filter(!is.na(Ozone))
top_cinq <- avec_ozone |> arrange(desc(Ozone)) |> slice_head(n = 5)
resume <- df |>
  summarise(n_total = n(),
            n_ozone = sum(!is.na(Ozone)),
            moyenne_ozone = mean(Ozone, na.rm = TRUE),
            mediane_ozone = median(Ozone, na.rm = TRUE))
top_cinq
resume
# na.rm = TRUE retire les NA du calcul, pas du tableau initial.

# ====================================================================
# EXERCICE 06 - Résumer des données par groupe
# Données : ToothGrowth | Fonctions : group_by(), summarise(), count()
# ====================================================================
# 1. Compter les observations par type de supplément et par dose.
# 2. Calculer la longueur moyenne des dents pour chaque supplément.
# 3. Calculer longueur moyenne et écart-type pour chaque couple supplément-dose.

data(ToothGrowth)
df <- tibble::as_tibble(ToothGrowth)
count(df, supp, dose)
par_supplement <- df |>
  group_by(supp) |>
  summarise(n = n(), moyenne = mean(len), .groups = "drop")
par_supplement_dose <- df |>
  group_by(supp, dose) |>
  summarise(n = n(), moyenne = mean(len), ecart_type = sd(len), .groups = "drop")
par_supplement
par_supplement_dose
# .groups = "drop" restitue un tableau non groupé.

# ====================================================================
# EXERCICE 07 - Libellés de variables et modalités
# Données : mtcars | Fonctions : labelled::var_label(), labelled::set_variable_labels(), labelled::val_labels()
# ====================================================================
# 1. Créer une variable facteur transmission à deux modalités explicites.
# 2. Attribuer un libellé à mpg et hp.
# 3. Créer une variable étiquetée pour am, puis afficher ses codes et leurs libellés.

data(mtcars)
df <- tibble::as_tibble(mtcars, rownames = "modele") |>
  mutate(transmission = factor(am, levels = c(0, 1),
                               labels = c("Automatique", "Manuelle")),
         am_etiquette = labelled::labelled(am,
           labels = c(Automatique = 0, Manuelle = 1))) |>
  labelled::set_variable_labels(
    mpg = "Distance parcourue (miles par gallon)",
    hp = "Puissance du moteur (chevaux)",
    am_etiquette = "Type de transmission"
  )
labelled::var_label(df$mpg)
labelled::var_label(df$hp)
labelled::val_labels(df$am_etiquette)
count(df, transmission)
# Un libellé décrit une variable ; les value labels décrivent ses codes.
# Les codes 0 et 1 sont conservés dans am_etiquette.

# ====================================================================
# EXERCICE 08 - Premier tableau descriptif
# Données : iris | Fonctions : tbl_summary(), statistic, digits, missing
# ====================================================================
# 1. Construire un tbl_summary() avec Species et les quatre mesures.
# 2. Présenter les variables numériques avec moyenne et écart-type.
# 3. Arrondir à une décimale et supprimer la ligne des manquants si elle est inutile.

data(iris)
df <- tibble::as_tibble(iris)
tableau <- df |>
  gtsummary::tbl_summary(
    statistic = list(gtsummary::all_continuous() ~ "{mean} ({sd})"),
    digits = list(gtsummary::all_continuous() ~ 1),
    missing = "no"
  )
tableau
# tbl_summary() affiche les effectifs et pourcentages des catégories,
# et la moyenne (écart-type) des variables quantitatives.
# Ici iris ne comporte pas de données manquantes.

# ====================================================================
# EXERCICE 09 - Comparer deux groupes
# Données : ToothGrowth | Fonctions : tbl_summary(), add_p(), add_overall(), add_n()
# ====================================================================
# 1. Décrire len et dose selon supp (OJ ou VC).
# 2. Ajouter une colonne globale puis une colonne de p-values.
# 3. Ajouter le nombre d'observations renseignées pour chaque variable et commenter une différence descriptive sans lui donner un sens causal.

data(ToothGrowth)
df <- tibble::as_tibble(ToothGrowth) |>
  mutate(supp = factor(supp, levels = c("OJ", "VC"),
                       labels = c("Jus d'orange", "Vitamine C")))
tableau <- df |>
  select(supp, len, dose) |>
  gtsummary::tbl_summary(
    by = supp,
    statistic = list(gtsummary::all_continuous() ~ "{mean} ({sd})"),
    digits = list(gtsummary::all_continuous() ~ 1),
    missing = "no"
  ) |>
  gtsummary::add_overall() |>
  gtsummary::add_n() |>
  gtsummary::add_p()
tableau
# La p-value teste une hypothèse d'association ; elle ne prouve pas
# que le supplément est la cause de la différence observée.

# ====================================================================
# EXERCICE 10 - Tableau de synthèse d'une enquête fictive
# Données : mtcars | Fonctions : labelled, tbl_summary(), add_difference(), modify_header()
# ====================================================================
# 1. Créer une catégorie de consommation forte (mpg < 20) et une variable puissance en deux catégories.
# 2. Attribuer des libellés à mpg, hp et puissance_cat.
# 3. Comparer mpg et hp selon am avec tbl_summary(), add_difference(), add_n(), modify_header() ; interpréter les différences de moyennes.

data(mtcars)
df <- tibble::as_tibble(mtcars) |>
  mutate(
    transmission = factor(am, levels = c(0, 1),
                          labels = c("Automatique", "Manuelle")),
    consommation_forte = factor(if_else(mpg < 20, "Oui", "Non"),
                                levels = c("Non", "Oui")),
    puissance_cat = factor(if_else(hp >= 150, "150 et plus", "Moins de 150"),
                           levels = c("Moins de 150", "150 et plus"))
  ) |>
  labelled::set_variable_labels(
    mpg = "Distance parcourue (miles par gallon)",
    hp = "Puissance (chevaux)",
    puissance_cat = "Catégorie de puissance"
  )
count(df, transmission, consommation_forte)
tableau <- df |>
  select(transmission, mpg, hp) |>
  gtsummary::tbl_summary(
    by = transmission,
    statistic = list(gtsummary::all_continuous() ~ "{mean} ({sd})"),
    digits = list(gtsummary::all_continuous() ~ 1),
    missing = "no"
  ) |>
  gtsummary::add_n() |>
  gtsummary::add_difference() |>
  gtsummary::modify_header(label = "**Variable**")
tableau
# add_difference() estime ici la différence entre deux moyennes.
# Vérifier le sens de la soustraction indiqué par les en-têtes du tableau.
# Les 32 voitures de mtcars ne forment pas un échantillon représentatif.
