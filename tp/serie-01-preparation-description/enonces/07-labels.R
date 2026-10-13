# SERIE 01 - JOUR 07 - Codes, libellés et valeurs manquantes déclarées
# Travail prévu : 4 h. À rendre : j07_nom.R, dictionnaire de variables et
# une table comparant code brut, libellé de modalité et facteur d'analyse.

library(dplyr)
library(tibble)
library(labelled)
library(haven)
voitures <- as_tibble(datasets::mtcars, rownames = "modele")
# mtcars contient des caractéristiques de voitures. Les étiquettes ajoutées
# ci-dessous servent à travailler les mêmes opérations que sur un fichier d'enquête.

# A. Trois notions différentes --------------------------------------------
# Q01. Sur am, identifier le nom de variable, ses deux codes et leur sens dans
#      l'aide de mtcars. Distinguer nom, libellé de variable et libellé de modalité.
# Q02. Avec var_label(), attribuer à mpg "Distance parcourue (miles/gallon US)".
#      Afficher le libellé puis names(voitures). Qu'est-ce qui a changé ?
# Q03. Avec set_variable_labels(), étiqueter hp, wt, am, cyl et modele.
#      Récupérer les libellés avec var_label() et produire un dictionnaire.
# Q04. Renommer mpg en rendement dans une copie. Vérifier le nom et le libellé.
#      Le libellé doit-il nécessairement être identique au nom informatique ?

# B. Étiqueter les codes ---------------------------------------------------
# Q05. Attribuer à am les value labels Automatique = 0 et Manuelle = 1 avec val_labels().
#      Afficher les codes, les étiquettes et la classe de la colonne.
# Q06. Dans une autre copie, employer set_value_labels() pour am et vs
#      (Moteur en V = 0, Moteur en ligne = 1). Comparer le résultat pour am à Q05.
# Q07. Recréer la variable avec labelled(). Contrôler les codes sous-jacents
#      avec as.numeric(), puis les modalités avec val_labels().
# Q08. Utiliser val_label(x, 1) pour lire puis modifier le libellé du seul code 1.
#      Les valeurs observées ont-elles été modifiées ?
# Q09. Ajouter à une copie une étiquette "Non renseigné" = 9 avec add_value_labels().
#      Le fait d'ajouter cette étiquette crée-t-il une observation de code 9 ?

# C. Préparer les variables pour l'analyse ---------------------------------
# Q10. Convertir am étiqueté avec to_factor(). Comparer as.character() du facteur
#      et as.numeric() du vecteur étiqueté. Ne pas confondre les deux opérations.
# Q11. Convertir avec levels = "prefixed", puis avec sort_levels = "values".
#      Examiner les niveaux. À quoi sert un facteur préfixé dans un contrôle de codage ?
# Q12. Employer remove_val_labels() sur une copie de am. Comparer valeurs,
#      classe, libellé de variable et étiquettes de modalités avant/après.
# Q13. Appliquer to_factor() à un tableau contenant am, vs, mpg et hp.
#      Vérifier que seules les colonnes étiquetées en modalités deviennent facteurs.

# D. Les valeurs non documentées et les non-réponses ------------------------
# Vecteurs créés pour ces questions : les codes 2 et 9 ne viennent pas de mtcars.
x_incomplet <- labelled(c(0, 1, 2, NA), labels = c(Automatique = 0, Manuelle = 1))
# Q14. Comparer to_factor(x_incomplet), to_factor(..., nolabel_to_na = TRUE)
#      et to_factor(..., strict = TRUE). Quelle opération risque de perdre de l'information ?

x_spss <- haven::labelled_spss(c(0, 1, 9, NA),
  labels = c(Automatique = 0, Manuelle = 1, "Non renseigné" = 9),
  na_values = 9, label = "Transmission")
# Q15. Examiner na_values(), val_labels(), is.na() et as.numeric() sur x_spss.
#      Expliquer la différence entre NA de R et code de non-réponse déclaré.
# Q16. Nettoyer avec haven::zap_missing(), puis convertir en facteur en retirant
#      les niveaux inutilisés. Vérifier le nombre de NA et les modalités restantes.
# Q17. Comparer mean(as.numeric(x_spss), na.rm=TRUE) au résultat après zap_missing().
#      Pourquoi ces moyennes ne décrivent-elles de toute façon pas une transmission ?

# E. Conserver et contrôler la documentation -------------------------------
# Q18. Utiliser look_for() pour retrouver les variables liées au moteur dans
#      voitures. Créer hp_double = 2 * hp ; vérifier le libellé hérité et le corriger.
# Q19. Enregistrer le tableau étiqueté avec saveRDS(), le relire et vérifier
#      l'identité des objets. Écrire dans resultats/serie-01/j07/.
# Q20. Exporter les mêmes données en CSV, puis les relire. Vérifier les effectifs
#      et constater ce que deviennent les libellés. Garder le dictionnaire à part.

# F. Réemploi --------------------------------------------------------------
# Q21. Sur ToothGrowth, conserver supp, créer supp_code (OJ = 1, VC = 2), ajouter
#      les libellés "Jus d'orange" et "Acide ascorbique", puis un facteur supp_fr.
#      Étiqueter len et dose à partir de l'aide, sans inventer une unité absente.
#      Vérifier la correspondance entre supp, supp_code et supp_fr.
# Q22. Préparer une table d'analyse contenant len, dose et supp_fr et un
#      dictionnaire séparé. Rédiger dix lignes sur le moment où convertir les
#      variables étiquetées et sur la conservation des codes et des non-réponses.
