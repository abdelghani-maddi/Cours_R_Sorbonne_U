# SERIE 01 - JOUR 03 - Calculs, recodages, facteurs et texte
# Travail prévu : 4 à 5 h. À rendre : j03_nom.R, dictionnaire des variables
# créées et deux tables de contrôle ancien codage / nouveau codage.
# Aides : ?mutate, ?case_when, ?cut, ?forcats::fct_collapse, ?stringr::str_squish.

library(dplyr)
library(tibble)
library(forcats)
library(stringr)
voitures <- as_tibble(datasets::mtcars, rownames = "modele")

# Exemple : mutate() conserve les anciennes variables sauf suppression explicite.
exemple <- voitures |> mutate(poids_kg = wt * 453.59237)
# wt est exprimé en milliers de livres, pas en kilogrammes.

# A. Calculer sans perdre la variable source -------------------------------
# Q01. Ajouter conso_l_100 = 235.215 / mpg, poids_kg = wt * 453.59237,
#      puis puissance_par_tonne = hp / (poids_kg / 1000).
#      La variable créée au début de mutate() peut-elle être réutilisée ensuite ?
# Q02. Comparer les calculs non arrondis et une copie arrondie à une décimale.
#      Pourquoi garder les calculs non arrondis pour les analyses suivantes ?
# Q03. Refaire Q01 avec .keep = "used" puis .keep = "none".
#      Ajouter .before = 1. Décrire ce que chacun de ces arguments conserve.

# B. Recoder et tester les bornes ------------------------------------------
# Q04. Créer transmission : am = 0 -> Automatique, am = 1 -> Manuelle,
#      avec if_else(). Puis vérifier que am ne contient que les codes prévus.
# Q05. Sur le vecteur c(0, 1, NA), comparer if_else(x == 1, "Oui", "Non")
#      avec la même commande et missing = "Non renseigné". Quel est le coût
#      d'une catégorie explicite de non-réponse pour le calcul des pourcentages ?
# Q06. Classer hp : moins de 100 ; de 100 à moins de 200 ; 200 et plus.
#      Utiliser case_when(), puis un facteur dans cet ordre.
# Q07. Tester ce recodage sur c(99, 100, 199, 200, NA).
#      Corriger une version où hp < 200 serait placé avant hp < 100.
# Q08. Refaire le classement avec cut(), breaks = c(-Inf, 100, 200, Inf),
#      right = FALSE. Vérifier la correspondance exacte avec Q06.
# Q09. Créer un profil qui distingue d'abord les voitures hp >= 200,
#      puis celles hp < 200 et mpg >= 25, puis les autres.
#      Expliquer pourquoi inverser les conditions pourrait changer le résultat.
# Q10. Croiser cyl et la catégorie de puissance avec count(). Vérifier que
#      la somme des effectifs retrouve 32. Donner deux limites des seuils choisis.

# C. Manipuler les facteurs ------------------------------------------------
# Q11. Convertir cyl en facteur : 4, 6, 8 -> "4 cylindres", "6 cylindres",
#      "8 cylindres". Comparer as.numeric(cyl_facteur) aux codes de départ.
# Q12. Placer Manuelle en première modalité avec fct_relevel().
#      Comparer l'ordre des niveaux avant/après, sans changer les effectifs.
# Q13. Avec fct_recode(), raccourcir les noms de transmission en Auto / Manu.
#      Avec fct_collapse(), regrouper 6 et 8 cylindres en "6 ou 8 cylindres".
# Q14. Ordonner les catégories de cylindres par fréquence avec fct_infreq(),
#      puis par consommation moyenne avec fct_reorder(). Expliquer la différence.
# Q15. Filtrer les voitures à quatre cylindres. Comparer les niveaux conservés
#      avant et après fct_drop(). Quand garder un niveau absent est-il utile ?
# Q16. Appliquer fct_lump_n(n = 2) à cyl_facteur. Examiner les effectifs et
#      expliquer le traitement des ex aequo ; ne pas supposer le résultat.

# D. Nettoyer des chaînes --------------------------------------------------
# Ce petit défaut de saisie est introduit pour l'exercice, pas présent dans mtcars.
texte_sale <- c("  Automatique  ", "MANUELLE", " automatique", "Manuelle   ")
# Q17. Nettoyer texte_sale avec str_squish() et str_to_lower().
#      Vérifier qu'il ne reste que deux modalités distinctes.
# Q18. Dans modele, repérer Mercedes et Mazda avec str_detect(), puis extraire
#      le premier mot avec str_extract(). Pourquoi ce mot n'est-il pas toujours
#      un identifiant fiable du constructeur ? Consulter les modèles obtenus.
# Q19. Créer modele_cle : minuscules et espaces remplacés par _. Utiliser
#      str_replace_all(). Vérifier l'unicité, sans supprimer modele d'origine.

# E. Réemploi --------------------------------------------------------------
# Q20. Sur iris, renommer Species en espece, créer un rapport longueur/largeur
#      des pétales, le classer en < 3, [3, 5[, >= 5. Ordonner les catégories.
#      Vérifier les bornes et produire les effectifs par espèce et catégorie.
# Q21. Construire une table donnant, pour chaque variable créée dans Q01,
#      Q06 et Q20, son nom, sa formule ou règle, son unité et sa source.
# Q22. Corriger : if_else(am == 1, 1, "0") ; case_when(hp < 200 ~ "Moyen",
#      hp < 100 ~ "Faible", TRUE ~ "Fort"). Rédiger les règles de traitement
#      des NA et de choix des seuils que vous appliqueriez à un fichier d'enquête.
