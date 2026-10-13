# SERIE 01 - JOUR 09 - Comparaisons, intervalles et tests dans les tableaux
# Travail prévu : 4 à 5 h. À rendre : j09_nom.R, deux tableaux de comparaisons
# et une note distinguant différence observée, intervalle de confiance et p-value.

library(dplyr)
library(tibble)
library(forcats)
library(labelled)
library(gtsummary)
dents <- as_tibble(datasets::ToothGrowth) |> mutate(
  supp = factor(supp, levels = c("OJ", "VC"), labels = c("Jus d'orange", "Acide ascorbique")),
  dose_cat = factor(dose, levels = c(0.5, 1, 2))
) |> set_variable_labels(len = "Longueur des odontoblastes (len)",
                         dose_cat = "Dose (mg/jour)", supp = "Mode d'administration")
# Lire l'aide de ToothGrowth. Les comparaisons portent sur les animaux de cette
# expérience ; len ne doit pas recevoir une unité inventée.

# A. Décrire avant de tester -----------------------------------------------
# Q01. Compter les animaux pour chaque couple supp-dose. Vérifier l'équilibre
#      du protocole et identifier l'unité d'observation.
# Q02. Décrire len et dose_cat selon supp, avec moyenne (écart-type), total et n.
#      Quels éléments faut-il lire avant d'ajouter une colonne de tests ?
# Q03. Sur un tableau distinct, ajouter add_p() en imposant t.test pour len
#      et fisher.test pour dose_cat. Lire les notes de méthode sous le tableau.
#      Pourquoi le test de dose_cat n'est-il pas une question scientifique utile ici ?
# Q04. Refaire le test de len avec t.test(len ~ supp, data = dents).
#      Comparer la p-value non arrondie à celle contenue dans le tableau.
# Q05. Refaire add_p() avec wilcox.test pour len, exact = FALSE.
#      Expliquer pourquoi une comparaison de rangs ne teste pas en général
#      exactement la même hypothèse qu'une comparaison de moyennes.

# B. Comparer à dose fixée -------------------------------------------------
# Q06. Retenir dose == 1. Décrire len par supp, puis justifier l'intérêt
#      de cette restriction par rapport à la comparaison toutes doses confondues.
# Q07. Ajouter add_difference(test = len ~ "t.test", conf.level = 0.95).
#      Fixer levels = c("Jus d'orange", "Acide ascorbique"). Vérifier le sens
#      de la différence avec un calcul direct de moyennes.
# Q08. Inverser levels. Vérifier le signe de l'estimation et expliquer ce qui
#      arrive aux bornes de l'intervalle et à la p-value.
# Q09. Créer long_20 : len >= 20. Ce seuil est un choix de TP. Décrire la modalité 1
#      et ajouter une différence de proportions avec prop.test. Préciser l'événement.
# Q10. Calculer directement les deux proportions et leur différence. Distinguer
#      une différence de 0.20, de 20 points et une augmentation relative de 20 %.

# C. Intervalles et comparaisons multiples ----------------------------------
# Q11. Sur les animaux dose == 1, ajouter add_ci() à un tableau global de len
#      et long_20. Choisir t.test pour la moyenne et wilson pour la proportion.
#      Distinguer IC d'une moyenne et IC d'une différence entre deux moyennes.
# Q12. Comparer les IC à 90 % et 95 % pour la moyenne globale de len avec t.test().
#      Quel intervalle est plus large, et pourquoi ?
# Q13. Dans iris, comparer les quatre mesures selon Species avec add_p(),
#      en imposant kruskal.test. Combien d'hypothèses sont testées ?
# Q14. Ajouter add_q(method = "holm"). Vérifier les résultats avec p.adjust().
#      Dire quelle famille de tests est corrigée et ce que signifie cet ajustement.

# D. Présenter plusieurs sous-groupes --------------------------------------
# Q15. Construire un tableau par dose avec tbl_strata(), puis, dans chaque dose,
#      comparer len selon supp avec add_difference(). Fixer types et arrondis.
# Q16. Présenter le plan supp-dose avec tbl_cross(), avec pourcentages en ligne.
#      Vérifier une ligne à la main et comparer à count().
# Q17. Dans une copie de la dose 1, rendre la première valeur len manquante.
#      Refaire tableau, add_n() et add_difference(). Quelles observations
#      entrent dans les moyennes et dans le test ? Les groupes restent-ils équilibrés ?
# Q18. Formater les p-values avec label_style_pvalue(digits = 2), puis utiliser
#      bold_p(t = 0.05). Pourquoi le gras ne doit-il pas devenir un classement
#      de l'importance scientifique des résultats ?

# E. Corriger et transposer ------------------------------------------------
# Q19. Corriger ce raisonnement : « p > 0.05 prouve qu'il n'y a pas de différence ».
#      Corriger aussi l'ajout successif de add_p() et add_difference() quand
#      il ne sert qu'à produire deux fois la même information de test.
# Q20. Sur PlantGrowth, comparer ctrl et trt2 : retirer trt1, supprimer son niveau
#      inutilisé, décrire weight et estimer la différence de moyennes avec son IC.
# Q21. Expliquer pourquoi un test analogue appliqué aux 578 lignes de ChickWeight
#      comme à 578 individus indépendants serait inadapté. Se référer au jour 06.
# Q22. Écrire trois contrôles numériques reliant tableau et calculs directs,
#      puis une interprétation de 150 à 200 mots de la comparaison à dose 1.
#      Citer les groupes, le sens de la soustraction, la dose, l'effectif et les limites.
