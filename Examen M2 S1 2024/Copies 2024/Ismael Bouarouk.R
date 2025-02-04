
# charger les librairies --------------------------------------------------

library(questionr)
library(dplyr)
library(gtsummary)
library(ggplot2)
library(ade4)
library(explor)
library(factoextra)

# charger la base ---------------------------------------------------------

data("happy")
d <- happy


# Partie 1 ----------------------------------------------------------------


# informations générales de la bases --------------------------------------

dim(d) #La base contient 10 variables et 51020 observations
head(d)
top_10lignes <- d %>%
  slice(1:10)
top_10lignes

#> top_10lignes
#id         happy year age    sex       marital
#1   1 not too happy 1972  23 female never married
#2   2 not too happy 1972  70   male       married
#3   3  pretty happy 1972  48 female       married
#4   4 not too happy 1972  27 female       married
#5   5  pretty happy 1972  61 female       married
#6   6  pretty happy 1972  26   male never married
#7   7 not too happy 1972  28   male      divorced
#8   8 not too happy 1972  27   male never married
#9   9  pretty happy 1972  21 female never married
#10 10  pretty happy 1972  30 female       married

describe(d)

# $id: numeric
#$happy: nominal factor
#$year: numeric
#$age: numeric
#$sex: nominal factor
#$marital: nominal factor
#$degree: nominal factor
#$finrela:nominal factor
#$health: nominal factor
#$wtssall: numeric

unique(d$happy) # Levels: 3 : not too happy, pretty happy, very happy
unique(d$degree) # Levels: 5 : bachelor, lt high school, high school, graduate, junior college
unique(d$health) # Levels : 4 : poor, fair, good, excellent

sum(is.na(d)) #Il y a 22484 NA indiqué
sum(is.na(d$id))
sum(is.na(d$happy)) # 4717 NA
sum(is.na(d$year)) # aucun
sum(is.na(d$age)) # 184 NA
sum(is.na(d$sex)) # aucun
sum(is.na(d$marital)) # 14 NA
sum(is.na(d$degree)) # 164 NA
sum(is.na(d$finrela)) # 4876 NA
sum(is.na(d$health)) # 12529 NA
sum(is.na(d$wtssall)) # aucun

# Calcul de la moyenne de na par variable

moy_na1 <- d %>%
  group_by(happy) %>%
  summarise(nbr = n()) %>%
  mutate(part = nbr / sum(nbr) * 100)
moy_na1 # La variable happy à 9,75% de valeurs na

moy_na2 <- d %>%
  group_by(age) %>%
  summarise(nbr = n()) %>%
  mutate(part = nbr / sum(nbr) * 100)
moy_na2 # La variable happy à 0,32 % de valeurs manquantes

moy_na3 <- d %>%
  group_by(marital) %>%
  summarise(nbr = n()) %>%
  mutate(part = nbr / sum(nbr) * 100)
moy_na3 # La variable marital à 0,02 % de valeurs manquantes

moy_na4 <- d %>%
  group_by(degree) %>%
  summarise(nbr = n()) %>%
  mutate(part = nbr / sum(nbr) * 100)
moy_na4 # La variable degree à 0,32 % de valeurs manquantes

moy_na5 <- d %>%
  group_by(finrela) %>%
  summarise(nbr = n()) %>%
  mutate(part = nbr / sum(nbr) * 100)
moy_na5 # la variable finrela à 9,56 % de valeurs manquantes

moy_na6 <- d %>%
  group_by(health) %>%
  summarise(nbr = n()) %>%
  mutate(part = nbr / sum(nbr) * 100)
moy_na6 # La variable health à 24,6 % de valeurs manquantes

# On associe la colonne id aux lignes 

d <- data.frame(d)
d <- d[,-1]


# Recodage des variables --------------------------------------------------

## Recodage de d$age en d$age_group
d$age_group <- cut(d$age,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(18, 29, 49, 64, 89)
)
## Recodage de d$age_group
d$age_group <- as.character(d$age_group)
d$age_group[d$age_group == "[18,29)"] <- "jeunes adultes"
d$age_group[d$age_group == "[29,49)"] <- "adultes"
d$age_group[d$age_group == "[49,64)"] <- "seniors"
d$age_group[d$age_group == "[64,89]"] <- "âgés"
d$age_group <- factor(d$age_group)

unique(d$age_group)



d <- d %>%
  mutate(happiness_score = case_when(
    happy == "very happy" ~ "3",
    happy == "pretty happy" ~ "2",
    happy == "not too happy" ~ "1"))



# Partie 2 ----------------------------------------------------------------


#5. Résumé statistique
describe(d$age)
mean(d$age, na.rm = TRUE) # Moyenne d'âge à 45 ans.
median(d$age, na.rm = TRUE) # Médiane de 43 ans
sd(d$age, na.rm = TRUE) # Ecart-type de 17,44
max(d$age, na.rm = TRUE ) 
min(d$age, na.rm = TRUE) 

# Analyser la répartition de l'age selon le degree

tab1<-table(d$age_group,d$degree)
prop.table(tab1)*100

#6. Croisements et visualisations 

d_happy_sex <- d %>% group_by(happy,sex) %>% 
  summarise(nbr = n()) %>%
  mutate(part = nbr / sum(nbr) * 100)
d_happy_sex

tab2<-table(d$happy,d$sex)
prop.table(tab2)*100

d %>% 
  tbl_summary(include = c("happy","sex"),
                     by = sex) %>%
  add_overall()

# Réaliser un graph

ggplot(d) +
  aes(x = sex, fill = happy) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")       # Palette de couleurs
  

# Partie 3  ---------------------------------------------------------------

tab3<-table(d$happy,d$marital)
tab3
prop.table(tab3)*100

ggplot(d) +
  aes(x = marital, fill = happy) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1") 

# Bien régler les données

data_summary <- d %>%
  group_by(marital, happy) %>%
  summarise(count = n(), .groups = "drop") # Compte des observations par catégorie

# Création du diagramme en bar
plot1 <- data_summary %>%
  ggplot( aes(x = factor(marital), y = count, fill = happy)) +
  geom_bar(stat = "identity", position = "dodge") + # Barres côte à côte
  scale_fill_brewer(palette = "Pastel1") +         # Palette de couleurs
  labs(
    title = "Niveau de bonheur selon la situation maritale",
    x = "situation maritale",
    y = "Nombre d'individus",
    fill = "niveau de bonheur"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotation des labels
  )
print(plot1)

# 8. 

d %>% 
  tbl_summary(include = c("happy","health"),
              by = health) %>%
  add_overall()

# On observe une surreprésentation de la modalité not too happy pour les personnes pauvres
# Et une nette surreprésentation de la modalité very happy pour les personnes ayant répondu "excellent".
# L'argent fait le bonheur ?


# Partie 4 : ACM ----------------------------------------------------------

bg <- d %>%
  select(happy, marital, degree, health, finrela )

## Recodage de bg$finrela en bg$finrela_rec
bg$financial_status <- as.character(bg$finrela)
bg$financial_status[bg$finrela == "far below average"] <- "low"
bg$financial_status[bg$finrela == "below average"] <- "low"
bg$financial_status[bg$finrela == "above average"] <- "high"
bg$financial_status[bg$finrela == "far above average"] <- "high"
bg$financial_status <- as.factor(bg$financial_status)
describe(bg)
bg <- select(bg, -finrela)

acm <- dudi.acm(bg, scannf = FALSE, nf = Inf)

explor::explor(acm)

# L'axe 1 explique 11% de l'ensemble des dimensions, les variables qui y contribuent sont : 
# health, marital, happy et dregree

# L'axe 2 explique 8% de l'ensemble des dimensions et les variables qui y contribuent le plus sont : 
# health, degree, financial_status

boxplot(acm)
fviz_contrib(acm, choice = "var", axes = 1)
fviz_contrib(acm, choice = "var", axes = 2)
fviz_contrib(acm, choice = "var", axes = 3)

# Il semble qu'on distingue 3 grande catégories, les personnes issues de classes aisées, 
# mariées, heureuses et avec des longues études.
# Les personnes de classes moyennes, divorcées, des études courtes et plutôt heureuses.
# les personnes pauvres et malheureuses et veufs ou veuves.

# Il semble que le niveau de bonheur soit avant tout corrélé à la richesse mais aussi au statut marital, 
# Le divorce n'est pas synonyme de niveau de bonheur bas. Par contre la mort du conjoint ou de la conjointe oui. 
# Je recommande d'améliorer les conditions de vie et les salaires pour que plus de monde soit heureux. 