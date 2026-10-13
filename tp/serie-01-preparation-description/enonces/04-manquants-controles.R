# SERIE 01 - JOUR 04 - Valeurs manquantes et contrôle d'un fichier
# Travail prévu : 4 à 5 h. À rendre : j04_nom.R, bilan de qualité et journal
# des trois anomalies introduites ci-dessous. Ne pas modifier air_brut.

library(dplyr)
library(tidyr)
library(tibble)
air_brut <- as_tibble(datasets::airquality) |> mutate(id_jour = row_number())
# Altérations de saisie créées uniquement pour ce TP : deux sentinelles et un vent négatif.
air_saisi <- air_brut |>
  mutate(Ozone = replace(Ozone, 1, 999), Solar.R = replace(Solar.R, 2, -999),
         Wind = replace(Wind, 4, -1))
mesures <- c("Ozone", "Solar.R", "Wind", "Temp")

# A. Mesurer avant de nettoyer --------------------------------------------
# Q01. Avec summarise(across()), compter les NA de chaque mesure de air_brut.
#      Obtenir ensuite un pourcentage, en utilisant n() comme dénominateur.
# Q02. Refaire ce bilan sur air_saisi. Pourquoi les défauts introduits ne sont-ils
#      pas tous détectés par is.na() ? Inspecter minimum et maximum de chaque mesure.
# Q03. Retrouver les trois lignes altérées à partir des règles : Ozone == 999,
#      Solar.R == -999, Wind < 0. Conserver id_jour, Month et Day.
# Q04. Construire un journal avec id_jour, variable, valeur_saisie, action.
#      Indiquer que les sentinelles deviennent NA et qu'un vent négatif devient NA.
#      Une valeur très élevée mais plausible doit-elle être supprimée automatiquement ?

# B. Recoder sans inventer des observations --------------------------------
# Q05. Remplacer 999 et -999 par NA avec na_if(), et Wind < 0 par NA avec if_else().
#      Créer air_propre. Vérifier le nombre de NA ajouté à chaque variable.
# Q06. Comparer mean(Ozone), mean(Ozone, na.rm = TRUE), et le résultat avant
#      nettoyage. Donner l'effectif réellement utilisé pour chaque moyenne.
# Q07. Créer un indicateur ozone_renseigne et calculer sa proportion.
#      Pourquoi mean(!is.na(Ozone)) est-il une proportion et pas un effectif ?
# Q08. Retenir les lignes incomplètes sur au moins une mesure avec if_any().
#      Retenir les lignes complètes sur toutes les mesures avec if_all().
#      Vérifier que les deux ensembles partitionnent le tableau.
# Q09. Comparer drop_na(Ozone) et drop_na(all_of(mesures)). Quantifier les pertes
#      et expliquer à quelle analyse chaque sélection pourrait correspondre.
# Q10. Calculer le nombre de mesures manquantes par ligne avec rowSums(is.na(pick())).
#      Compter ensuite les lignes ayant 0, 1, 2... mesures manquantes.

# C. Plusieurs colonnes, plusieurs fonctions -------------------------------
# Q11. Utiliser across() pour calculer moyenne, médiane et écart-type des quatre
#      mesures. Nommer les sorties avec .names = "{.col}_{.fn}".
# Q12. Créer quatre copies centrées-réduites avec across() et .names = "z_{.col}".
#      Vérifier leur moyenne et leur écart-type en excluant les NA.
#      Expliquer pourquoi il faut exclure id_jour, Month et Day de ce calcul.
# Q13. Utiliser rowwise() et c_across() pour compter les mesures manquantes
#      ligne à ligne, puis ungroup(). Vérifier l'égalité avec Q10.
#      Pourquoi la moyenne brute de Ozone, Wind et Temp n'a-t-elle pas de sens ?

# D. Compléter : dans quelles circonstances ? ------------------------------
# Q14. Dans cet exemple séparé, la seconde colonne est une mesure de secours :
capteurs <- tibble(principal = c(10, NA, 30, NA), secours = c(11, 20, NA, NA))
#      Construire retenu avec coalesce(). Qu'arrive-t-il quand les deux valeurs
#      sont renseignées mais différentes, ou quand les deux sont manquantes ?
# Q15. Appliquer replace_na(list(principal = 0)) à une copie de capteurs.
#      Comparer les moyennes avant/après. Expliquer pourquoi ce code exécutable
#      n'est pas, à lui seul, une méthode de traitement de la non-réponse.
# Q16. Les identifiants suivants ont été omis dans un tableau de saisie :
identifiants <- tibble(groupe = c("A", NA, "B", NA), ligne = 1:4)
#      Utiliser fill(groupe, .direction = "down"). Quelle information externe
#      justifie cette opération ? Pourquoi ne pas remplir ainsi Ozone ?

# E. Diagnostic complet ---------------------------------------------------
# Q17. Comparer les moyennes de Temp entre cas complets et incomplets.
#      Faire aussi ce bilan par mois. Une différence prouve-t-elle le mécanisme des NA ?
# Q18. Corriger : filter(Ozone != NA) ; mean(Ozone, na.rm = TRUE) sans effectif ;
#      across(everything(), ~ replace_na(.x, 0)). Écrire les limites de chaque solution.
# Q19. Produire un bilan final : variable, n_total, n_disponible, n_manquant,
#      pct_manquant, min, max. Utiliser pivot_longer() si nécessaire et comparer
#      le bilan à celui de air_brut. Ne pas effacer les traces des anomalies.
# Q20. Écrire cinq assertions sur air_propre : lignes conservées, identifiants
#      uniques, sentinelles absentes, vent non négatif, données brutes intactes.
#      Terminer par une note de 150 mots sur les choix et leurs conséquences.
