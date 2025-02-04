# Packages ----
library(questionr)
library(tidyverse)
library(questionr)
library(gtsummary)
library(labelled)
library(ade4)
library(explor)
library(factoextra)
library(FactoMineR)


# Importation des données ----
### Q1
data(happy)
d <- happy

# extraire les 10 premieres lignes
top10lignes <- d %>%
  slice(1:10)
top10lignes

# combien d'observations et de variables contient le jeu de données : 
str(d)
# 51020 observations et 10 variables


### Q2
# pour voir les types de chaque variable :
describe(d)

# modalités de la variable "happy" :
# not too happy | pretty happy | very happy

# modalités de la variable "degree" :
# lt high school | high school | junior college | bachelor | graduate

# modalités de la variable "health" :
# poor | fair | good | excellent


### Q3
# on peut voir grâce aux résultats de la fonction describe que les variables contenant des valeurs manquantes sont :
# happy (9.2%) ; age (0.4%) ; marital (0.03%) ; degree (0.3%) ; finrela (9.6%) ; health (24.6%)


### Q4
## Recodage de d$age en d$age_group
d$age_group <- cut(d$age,
  include.lowest = TRUE,
  right = TRUE,
  dig.lab = 4,
  breaks = c(18, 29, 49, 64, 89)
)
# verification
table(d$age_group)

## Recodage de d$happy en d$happiness_score
d$happiness_score <- d$happy %>%
  fct_recode(
    "1" = "not too happy",
    "2" = "pretty happy",
    "3" = "very happy"
  )
# verification
table(d$happy)
table(d$happiness_score)


### Q5
# stats descriptives variable age
summary(d$age)

# répartition répondants selon degree et age_group
d %>%
  tbl_summary(
    include = c("age_group", "degree"),
    by = age_group,
  ) %>%
  add_overall(last = T) %>%
  add_p() %>%
  separate_p_footnotes()
# La majorité (61 %) des individus entre 18 et 29 ans, la majorité (53 %) des individus entre 30 et 49 ans et la 
# majorité (49 %) des individus entre 50 et 64 ans ont un niveau d'éducation correspondant au lycée
# tandis que la majorité (45 %) des individus entre 65 et 89 ans ont un niveau d'éducation plus bas que le lycée

d %>%
  tbl_summary(
    include = c("age_group", "degree"),
    by = age_group,
    percent = "row"
  ) %>%
  add_overall(last = T) %>%
  add_p() %>%
  separate_p_footnotes()
# La majorité des individus ayant un niveau plus bas que lycée sont des individus âgés entre 65 et 89 ans (34 %)
# La majorité des individus ayant un niveau lycée sont des individus âgés entre 30 et 49 ans (41 %)
# La majorité des individus ayant un niveau post-bac (junior college) sont des individus âgés entre 30 et 49 ans (54 %)
# La majorité des individus ayant un niveau licence sont des individus âgés entre 30 et 49 ans (50 %)
# La majorité des individus ayant un niveau diplôme d'études supérieures sont des individus âgés entre 30 et 49 ans (53 %)

# On constate donc un écart avec les plus vieilles générations (à partir de 65 ans) qui ont des niveaux d'éducation
# plus faibles.


### Q6
d %>%
  tbl_summary(
    include = c("happy", "sex"),
    by = sex
  ) %>%
  add_overall(last = T) %>%
  add_p() %>%
  separate_p_footnotes()


# Graphique
ggplot(d) +
 aes(x = sex, fill = happy) +
 geom_bar(position = "fill") +
 scale_fill_brewer(palette = "YlOrRd", 
 direction = 1) +
 labs(x = "Sexe", y = "Fréquences", title = "Bonheur en fonction du sexe", fill = "Légende") +
 theme_bw() +
 theme(axis.text.y = element_text(size = 11L), axis.text.x = element_text(size = 11L), 
 legend.text = element_text(size = 11L), legend.title = element_text(size = 11L))

# Ce graphique permet de mettre en comparaison les résultats donnés par chaque sexe tout en prenant en compte les différences
# en termes de nombre de répondants.
# On voit ainsi qu'il ne semble pas y avoir de réelle différence entre les hommes et les femmes sur la question du bonheur
# car les proportions pour chaque réponse sont extrêmement similaires.


### Q7
d %>%
  tbl_summary(
    include = c("happy", "marital"),
    by = marital
  ) %>%
  add_overall(last = T) %>%
  add_p() %>%
  separate_p_footnotes()

# Graphique
ggplot(d) +
 aes(x = happy, fill = marital) +
 geom_bar(position = "fill") +
 scale_fill_brewer(palette = "Spectral", 
 direction = 1) +
 labs(x = "Niveau de bonheur", y = "Fréquences", title = "Bonheur en fonction du statut marital", 
 fill = "Légende") +
 theme_bw() +
 theme(axis.text.y = element_text(size = 11L), axis.text.x = element_text(size = 11L), 
 legend.text = element_text(size = 11L), legend.title = element_text(size = 11L))

# Même graphique que précédemment pour pouvoir comparer les groupes aux proportions différentes.
# On constate cette fois-ci que les individus marriés semblent déclarer de plus hauts niveaux de bonheur que les autres
# groupes. Les 4 autres groupes tendent majoritairement déclarer ne pas être très heureux, bien que presque tout autant
# de personnes n'ayant jamais été mariées déclarent être assez heureuses.


### Q8
d %>%
  tbl_summary(
    include = c("happy", "health"),
    by = health
  ) %>%
  add_overall(last = T) %>%
  add_p() %>%
  separate_p_footnotes()

# Graphique
ggplot(d) +
 aes(x = happiness_score, fill = health) +
 geom_bar(position = "dodge") +
 scale_fill_brewer(palette = "RdBu", 
 direction = 1) +
 labs(x = "Niveau de bonheur", y = "Effectifs", title = "Bonheur selon l'état de santé", 
 fill = "Légende") +
 theme_minimal() +
 theme(axis.text.y = element_text(size = 11L), axis.text.x = element_text(size = 11L), 
 legend.text = element_text(size = 11L), legend.title = element_text(size = 11L))

# On constate qu'indépendemment de l'état de santé, la plupart des individus déclarent un niveau de bonheur moyen.


### Q9
## Recodage de d$finrela en d$financial_status
d$financial_status <- d$finrela %>%
  fct_recode(
    "low" = "far below average",
    "low" = "below average",
    "high" = "above average",
    "high" = "far above average"
  )

# Selection des variables d'intérêt
d2 <- d %>%
  select(happy, marital, degree, health, financial_status)


### Q10
# Réaliser l'ACM
acm <- dudi.acm(d2, scannf = F, nf = Inf)

explor::explor(acm)
res <- explor::prepare_results(acm)
explor::MCA_var_plot(res, xax = 1, yax = 2, var_sup = FALSE, var_sup_choice = ,
                     var_lab_min_contrib = 0, col_var = "Variable", symbol_var = NULL, size_var = "Contrib",
                     size_range = c(52.5, 700), labels_size = 10, point_size = 56, transitions = TRUE,
                     labels_positions = "auto", labels_prepend_var = FALSE, xlim = c(-1.68, 3.55),
                     ylim = c(-2.47, 2.76))


par(mfrow = c(1, 2))
for (i in 1:2) 
  barplot(acm$cr[, i], names.arg = row.names(acm$cr), las = 2, main = paste("Axe", i))
# On peut ainsi dire que le premier axe, qui explique 11,43 % de l'information totale, semble dépendre du statut financier
# tandis que le deuxième axe, qui explique 7,97 % de l'information totale, semble surtout dépendre du diplôme.
fviz_contrib(acm, choice = "var", axes = 1)
fviz_contrib(acm, choice = "var", axes = 2)
# Les modalités qui contribuent le plus à l'axe 1 sont le niveau de diplôme plus bas que lycée, le niveau de revenus bas,
# un niveau de bonheur faible et un niveau de revenus haut
# Les modalités qui contribuent le plus à l'axe 2 sont un niveau de bonheur élevé principalement, un niveau de bonheur moyen,
# un niveau de diplôme plus bas que lycée et une bonne santé


### Q11
# Représentation des modalités des variables sur le plan factoriel effectuée à la Q10 :
explor::MCA_var_plot(res, xax = 1, yax = 2, var_sup = FALSE, var_sup_choice = ,
                     var_lab_min_contrib = 0, col_var = "Variable", symbol_var = NULL, size_var = "Contrib",
                     size_range = c(52.5, 700), labels_size = 10, point_size = 56, transitions = TRUE,
                     labels_positions = "auto", labels_prepend_var = FALSE, xlim = c(-1.68, 3.55),
                     ylim = c(-2.47, 2.76))


# On semble constater 3 grands groupements.
# D'une part, il semblerait que les individus ayant un statut financier élevé, ont tendance à avoir un niveau d'éducation
# allant de la licence au diplôme supérieur, un niveau de santé excellent, à être mariés et à déclarer un niveau de bonheur
# très élevé.

# Ensuite, les individus ayant un statut financier moyen ont tendance à avoir un niveau d'éducation allant du lycée au
# début d'études supérieures, un bon niveau de santé, à ne s'être jamais mariés ou bien à être divorcés et à déclarer
# un niveau de bonheur moyen.

# Enfin, les individus ayant un statut financier faible ont plutôt tendance à avoir un niveau d'éducation inférieur au lycée,
# un niveau de santé moyen à mauvais, à être veufs ou séparés de leur conjoint, et à ne pas être très heureux dans leur vie.


### Synthèse et recommandations
# Ci-après visualisations supplémentaires pour mieux commenter l'ACM.
par(mfrow = c(1, 1))
s.corcircle(acm$co, clabel = .7)
boxplot(acm)
scatter(acm, col = RColorBrewer::brewer.pal(n = 8, name = "Dark2"))

# Les principaux facteurs associés à un haut niveau de bonheur sont d'avoir un diplôme de licence 
# et au-delà, d'avoir
# un excellent état de santé, d'avoir un niveau de revenus élevé et d'être marié.

# Il est difficile de donner des recommandations pour améliorer le bonheur des 
# individus car cela reviendrait globalement
# à leur dire d'obtenir un diplôme plus élevé, ce qui est souvent compliqué si 
# ça n'a pas déjà été accompli jusqu'à présent.
# Un diplôme plus élevé permettrait ainsi sans doute d'avoir des revenus 
# plus élevés et un meilleur état de santé car
# cela voudrait probablement dire un travail moins physique et plus de moyens 
# pour se soigner. Il faudrait idéalement aussi
# se marier, mais là encore, il s'agit d'une recommandation plus facile à dire qu'à faire.
