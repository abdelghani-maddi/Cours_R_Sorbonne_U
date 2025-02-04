library(tidyverse)
library(questionr)
library(openxlsx)
library(labelled)
library(codebook)
library(DT)
library(readxl)
library(esquisse)
library(gtsummary)
library(glue)


# PARTIE I -----
# Quesrion 1 
data("happy")
top_10lignes <- happy %>%
  slice(1:10)
dim(happy)
# Le jeu de données contient 51 020 observations et 9 variables (10 avec l'identifiant) 

#Question 2 
str(happy)
# happy : variable type factor 
# year : variable type numérique
# AGE : Variable type numérique
# sex : variable type factor 
# marital : variable type factor 
# degree : variable type factor 
# finrela : variable type factor 
# health : variable type factor 
# wtssall : variable type numérique 

table(happy$happy)
# les modalités pour la variable happy sont les suivantes : "not too happy", "pretty happy" et "very happy"
table(happy$degree)
# les modalités pour la variable degree sont les suivantes : "it high school", "high school", "junior college", "bachelor" et "graduate"
table(happy$health)
# les modalités pour la variable health sont les suivantes : "poor", "fair", "good" et "excellent"

# Question 3 
# valeurs manquantes : 
look_for(happy,"")
# Les variables "happy", "age", "marital", "degre", "finreala" et "health" contiennent des valeurs manquantes


# Question 4 
## Cutting happy$age into happy$age_group
## Cutting happy$age into happy$age_group1
# ---- age 
happy$age_group <- cut(happy$age,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(18, 30, 50, 65, 89)
)
             

## Recoding happy$happy into happy$happiness_score
happy$happiness_score <- happy$happy %>%
  fct_recode(
    "1" = "not too happy",
    "2" = "pretty happy",
    "3" = "very happy"
  ) %>%
  as.character() %>%
  as.numeric()




# PARTIE II -----
# Question 5 
summary(happy$age)
#Moyenne d'âge est d'environ 45 ans (45.43), la médiane de 43 ans, premier quartile de 31 ans et troisième quartile de 58 ans... 

summary(happy$degree)
happy %>% select(id, degree) %>% tbl_summary()
# Concernant le niveau d'étude, on remarque que les individus dans la catégorie "high school" occupe la plus grande proportion dans la base (52% des répondants)
# Les personnes issues de la catégories "junior college" sont les moins nombreux (5.1 % des répondants)

summary(happy$age_group) 
happy %>% select(id, age_group) %>% tbl_summary()
# Les 30-50 ans occupent la plus grande proportion au sein de la base (40% des répondants) 


# Question 6 
table(happy$sex,happy$happy)
# on remarque qu'il ne semble pas y avoir de grande différences entre hommes et femmes concernant le niveau de bonheur déclaré
# en effet la réponse "pretty happy" est la plus commune pour les deux groupes suivies de la réponse "very happy" et pour les deux groupes la modalité "no too hapy" est la moins importante

ggplot(happy) +
  aes(x = sex, fill = happy) +
  geom_bar(position = "dodge") +
  scale_fill_hue(direction = 1) +
  labs(
    title = "Niveau de bonheur déclaré et sexe des répondants"
  ) +
  theme_minimal() +
  facet_wrap(vars(sex))

# PARTIE III -----
# Question 7 
happy %>% tbl_summary(include = c("happy", "marital"), by = "marital") %>% add_p() %>% separate_p_footnotes()
# Il semble que les niveaux de bohneur ne soient pas trop corrélé au statut marital, ils sont plutôt  proches et ce malgré une différence au niveau du statut marital puisqu'on remarque que la modalité la plus fréquente dans tout les groupes est la modalité "pretty happy"
# Néanmoins, il est possible de relever quelques spécificités : 
# On note que le nombre de réponse very happy est plus élevée au sein du groupe des personnes mariées (40% de marriés qui donnent cette modalité)
# On note également que la proportion de "not too happy" est plus importante au sein du groupe des personnes séparées (28% de séparés donnant cette réponse)

library(ggplot2)
ggplot(happy) +
 aes(x = marital, fill = happy) +
 geom_bar(position = "fill") +
 scale_fill_hue(direction = 1) +
 labs(title = "Niveau de bonheur déclaré selon le statut marital") +
 theme_minimal()

# Question 8 
happy %>% tbl_summary(include = c("happy", "health"), by = "health") %>% add_p() %>% separate_p_footnotes()
# On note que le niveau de bonheur déclaré semble corréle au niveau de santé. En effet, parmi les personnes déclarant un excellent niveau de santé on note une proportion élevé de "very happy" (46%) et une faible proportion de "not too happy" (6.6%) 
# Chez les personnes déclarant un pauvre niveau de santé on note un faible niveau de modalités "very happy" (18%) et une proportion plus élevée de personnes répondant "not too happy" (35%) 
# En général on remarque que plus le niveau de santé déclaré est bon, plus la proportion de personnes répondant "very happy" augmente (de 18% de very happy chez les personnes à pauvre niveau de santé, puis 21 % pour ceux ayant un niveau moyen de santé "fair", 29% pour les ceux ayant un bon niveau de santé...)
# À l'inverse, on note que plus le niveau de santé du groupe est élevé plus la proportion de personnes répondant "no too happy" au sein de celui-ci diminue.


# PARTIE IV ---- 
# Question 9 

# Selection des variables d'intérêt
happy1 <- happy %>%
  select(happy, marital, degree, health, financial_status)

## Recoding happy1$finrela into happy1$financial_status
happy$financial_status <- happy1$finrela %>%
  fct_recode(
    "low" = "far below average",
    "low" = "below average",
    "high" = "above average",
    "high" = "far above average"
  )

library(ade4)
library(explor)

# Question 10 
# Réaliser l'ACM
acm <- dudi.acm(happy1, scannf = F, nf = 5)

explor::explor(acm)
# Concernant la première dimension la modalité du niveau d'éducation "degree li high, 
# le statut financier 'financial statut low' et du niveau de bonheur "happy not too " sont 
# celles qui y contribuent le plus. 

# Concernant la deuxième dimension, les modalités du niveau de bonheur 'happy very, happy pretty', 
# ainsi que la modalité du niveau d'éducation 'degree li high' sont celles qui y contribuent le plus  





# PARTIE V ----
# Il semble que les principaux facteurs associés à un niveau élevé de bonheur soient : 
# très bon niveau de santé, un statut financier élevé, un niveau d'éducation élevé (bachelor et graduate). 



