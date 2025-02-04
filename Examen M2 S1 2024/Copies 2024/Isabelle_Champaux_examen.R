library(tidyverse)
library(questionr)

data("happy")

#1)
summary(happy)
dim(happy)
#51020 obs of 10 variables

#2)
class(happy$age)
#numeric

class(happy$degree)
#factor

class(happy$finrela)
#factor

levels(happy$happy)
#"not too happy" "pretty happy"  "very happy"  

levels (happy$degree)
# "lt high school" "high school"    "junior college" "bachelor"       "graduate"    

levels(happy$health)
# "poor"      "fair"      "good"      "excellent"

#3)
summary(happy)
#happy, age, marital, degreee, finrela, health

#4)
## Recodage de happy$age en happy$age_group

## Recodage de happy$age en happy$age_group
happy$age_group <- cut(happy$age,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(18, 30, 50, 64, 89),
  labels =c("jeunes adultes", "adultes", "seniors", "ages")
)


## Recodage de happy$happy en happy$happy_rec
happy$happy_rec <- happy$happy %>%
  fct_recode(
    "1" = "not too happy",
    "2" = "pretty happy",
    "3" = "very happy"
  ) %>%
  as.character() %>%
  as.numeric()

#5)
library(gtsummary)

summary(happy$age)

happy %>%
  tbl_summary()


happy %>%
  tbl_summary(
    include =c("happy"),
    by = "degree"
  )%>%
  add_p() %>%
  separate_p_footnotes()



#6)
table(happy$happy, happy$sex)

ggplot(happy) +
  aes(x = happy) +
  geom_bar(fill = "#112446") +
  theme_minimal() +
  facet_wrap(vars(sex))

#7)

age_degree<- happy %>%
  tbl_summary(
    include = c("age_group"),
    by="degree"
  ) %>%
  add_overall(last=TRUE)

happy %>%
  tbl_summary(
    include =c("happy"),
    by = "marital"
  )%>%
  add_p() %>%
  separate_p_footnotes()

ggplot(happy) +
  aes(x = happy, fill = marital) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_minimal()

#8)

happy %>%
  tbl_summary(
    include =c("happy"),
    by = "health"
  )%>%
  add_p() %>%
  separate_p_footnotes()

# on voit que 35% des individus qui sont en mauvaise santé ne se déclarent pas très heureux
#a l'inverse, on voit que 46% des individus en excellente santé se déclarent très heureux.
#on peut supposer que le niveau de santé influe sur le bonheur.

#9)
library(ade4)
library(explor)


## Recodage de happy$finrela en happy$finrela_rec
happy$financial_status <- happy$finrela %>%
  fct_recode(
    "low" = "far below average",
    "low" = "below average",
    "high" = "above average",
    "high" = "far above average"
  )

d<- happy %>%
  select(happy, marital, degree,
         health, financial_status)

#10) et 11)
acm<-dudi.acm(d, scannf = FALSE, nf = Inf)

explor :: explor (acm)


library(factoextra)
fviz_screeplot(acm, choice = "eigenvalue")
fviz_screeplot(acm)
summary(acm)

s.corcircle(acm$co)
s.corcircle(acm$co, clabel = , 7)
fviz_mca_var(acm)

res <- explor :: prepare_results(acm)

boxplot(acm)

# Les dimensions principales sont le statut martial et le statut financier

# il y a un groupement entre les variables "high", "married", "excellent", "happy" 
# ce qui peut signifier que les individus riches, mariés et en bonne santé sont plus heureux
# que les autres

# il y a un autre groupement avec "low" et "separated" qui peut signifier que les gens 
#moins riches et séparés sont moins heureux.

#partie 5
#les principaux facteurs associés à un haut niveau de bonheur sont le statut financier, marital et la santé
