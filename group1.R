
library(questionr)
data(hdv2003)

## Recodage de hdv2003$age en hdv2003$age_rec
hdv2003$age_rec <- cut(hdv2003$age,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(18, 35, 45, 55, 65, 97)
)


library(gtsummary)


hdv2003 %>%
  tbl_summary(
    include = c("age_rec", "sexe", "sport"),
    by = sport,
    label = age_rec ~ "Age recodé",
    percent = "row"
    
  ) %>%
  add_p() %>%
  separate_p_footnotes() 




## Recodage de hdv2003$heures.tv en hdv2003$heures.tv_rec
hdv2003$heures.tv_rec <- cut(hdv2003$heures.tv,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(0, 2, 5, 12),
  labels = c("Peu","Moyen", "Beaucoup")
)



hdv2003 %>%
  tbl_summary(
    include = c("age_rec", "sexe", "heures.tv_rec"),
    by = heures.tv_rec,
    label = age_rec ~ "Age recodé",
    percent = "row"
    
  ) %>%
  add_p() %>%
  separate_p_footnotes() %>%
  add_overall()











# Ajustement d'un modèle de régression logistique pour prédire 'response' en fonction d'autres variables
mod <- glm(sport ~ sexe + heures.tv_rec + age_rec + cinema, data = hdv2003, family = binomial)

# Création du tableau récapitulatif des coefficients du modèle
regression_table <- tbl_regression(mod)

# Affichage du tableau récapitulatif
regression_table

# Transformation des coefficients en odds ratios et ajout des intervalles de confiance à 95%
regression_table_exp <- tbl_regression(mod, exponentiate = TRUE)

# Affichage du tableau avec les odds ratios
regression_table_exp

# Ajout de labels personnalisés aux variables
regression_table_labels <- regression_table_exp %>%
  modify_header(label ~ "**Variable**")

# Affichage du tableau avec des labels personnalisés
regression_table_labels

# Ajout des symboles de significativité pour les p-values
regression_table_significance <- regression_table_exp %>%
  modify_column_hide(columns = p.value) %>%
  add_significance_stars()

# Affichage du tableau avec les p-values sous forme de symboles
regression_table_significance





