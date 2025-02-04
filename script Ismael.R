# BIBLIOTHEQUES -----------------------------------------------------------

library(questionr)
library(tidyverse)
library(gtsummary)
library(labelled)
library(dplyr)
library(ggplot2)
library(esquisse)
library(ade4)
library(explor)

install.packages("")


base_1 <- dataHI %>%
  select(V6_, V7, V8_, v108_r, TYPLOG, CJT, V11_, V12_,
         v18_r1, v18_r2, v18_r3, v18_r4, v18_r5, v18_r6,
         v18_r7, v18_r8, v18_r9, v18_r10, v18_r11,
         v23_r1, v23_r2, v23_r3, v23_r4, v23_r5, v23_r6,
         v23_r7, v23_r8, v23_r9, v23_r10, v23_r11,
         v55_r1, v55_r2, v55_r3, v55_r4, v55_r5, v55_r6,
         v55_r7, v55_r8, v55_r9, v55_r10, V43_, V69_, v70_r1,
         v70_r2, v70_r3, v70_r4, v70_r5, V46_,V48_,V50_, 
         v51_r1, v51_r2, v51_r3, v51_r4, v51_r5, V96_, V78_, 
         V81_, V82_,V83_, V84_, V85_, v86, v86_r1, v86_r2, v86_r3,
         V87_, V88_, V89_, V90_, V97_, V109, V77_, V94_, V17_)


# RECODAGE des variables Socio-démographiques --------------------------------------------------


table(base_1$V17_)



base_1$finscol <- cut(base_1$V109,
                      include.lowest = TRUE,
                      right = TRUE,
                      dig.lab = 4,
                      breaks = c(0, 12, 18, 30)
)


## Recodage de base_1$finscol en base_1$finscol_2
base_1$finscol_2 <- base_1$finscol %>%
  fct_recode(
    "Primaine" = "[0,12]",
    "Secondaire" = "(12,18]",
    "Supérieur" = "(18,30]"
  )

base_1$Sexe <- base_1$V6_ %>%
  as.character() %>%
  fct_recode(
    "Femme" = "1",
    "Homme" = "2",
    NULL = "3"
  )

unique(base_1$V7)
min(base_1$V7, na.rm = T)
max(base_1$V7, na.rm = T)
summary(base_1$V7)
table(base_1$V7)
# Creation de tranches d'âge
## Cutting d$V7 into d$V7_rec
base_1$age_rec <- cut(base_1$V7,
                      include.lowest = TRUE,
                      right = FALSE,
                      dig.lab = 4,
                      breaks = c(0, 75, 80, 85, 90, 95, 101)
)
table(base_1$V7_rec)

base_1$sitmar_rec <- as.character(base_1$V8_)
base_1$sitmar_rec[base_1$V8_ == "1"] <- "Célibataire"
base_1$sitmar_rec[base_1$V8_ == "2"] <- "Marié(e)"
base_1$sitmar_rec[base_1$V8_ == "3"] <- "Marié(e) mais séparé(e)"
base_1$sitmar_rec[base_1$V8_ == "4"] <- "Pacsé(e)"
base_1$sitmar_rec[base_1$V8_ == "5"] <- "Divorcé(e)"
base_1$sitmar_rec[base_1$V8_ == "6"] <- "Veuf/veuve"



unique(base_1$sitmar_rec)

base_1$sitmar_rec <- factor(base_1$sitmar_rec,
                            levels = c(
                              "Célibataire", "Marié(e)", "Marié(e) mais séparé(e)",
                              "Pacsé(e)", "Divorcé(e)", "Veuf/veuve"
                            )
)

## Recoding d$v108_r into d$v108_r_rec (recodage CSP)
base_1$csp_rec <- as.character(base_1$v108_r)
base_1$csp_rec[base_1$v108_r == "1"] <- "Agriculture, Artisanat, Commerce"
base_1$csp_rec[base_1$v108_r == "2"] <- "Chef(fe) d'entreprise, Prof. libérale"
base_1$csp_rec[base_1$v108_r == "3"] <- "Cadre entreprise, Fonction publique"
base_1$csp_rec[base_1$v108_r == "4"] <- "Prof. intermédiaire, Technicien(ne), Maîtrise"
base_1$csp_rec[base_1$v108_r == "5"] <- "Employé(e)s"
base_1$csp_rec[base_1$v108_r == "6"] <- "Ouvriers"
base_1$csp_rec[base_1$v108_r == "7"] <- "Sans activité (au foyer)"
table(base_1$csp_rec)
## Reordering d$v108_r_rec
base_1$csp_rec <- base_1$csp_rec %>%
  fct_relevel(
    "Agriculture, Artisanat, Commerce", "Chef(fe) d'entreprise, Prof. libérale",
    "Cadre entreprise, Fonction publique", "Prof. intermédiaire, Technicien(ne), Maîtrise",
    "Employé(e)s", "Ouvriers", "Sans activité (au foyer)"
  )

# Distinction Type de logement HI
unique(base_1$TYPLOG)
## Recoding d$TYPLOG into d$TYPLOG_rec
base_1$TYPLOG_rec <- base_1$TYPLOG %>%
  fct_recode(
    "RA/Marpa" = "A01",
    "RA/Marpa" = "A02",
    "RA/Marpa" = "A03",
    "RA/Marpa" = "A05",
    "RA/Marpa" = "A06",
    "RA/Marpa" = "A07",
    "RA/Marpa" = "B01",
    "RA/Marpa" = "B02",
    "RA/Marpa" = "B03",
    "RA/Marpa" = "P01",
    "RA/Marpa" = "Q10",
    "RA/Marpa" = "Q11",
    "RA/Marpa" = "Q12",
    "RSS" = "R01",
    "RSS" = "R02",
    "RSS" = "R03",
    "RSS" = "R04",
    "RSS" = "R05",
    "RSS" = "R06",
    "RSS" = "R07",
    "RSS" = "S10",
    "RSS" = "S11",
    "Logt Ordi" = "Z10",
    "Logt Ordi" = "Z20",
    "Logt Ordi" = "Z30",
    "Logt Ordi" = "Z40"
  )
unique(base_1$TYPLOG_rec)


# RECODAGE autonomie sociale ----------------------------------------------

#Vivre en couple

unique(base_1$CJT)
table(base_1$CJT)
## Recoding d$CJT into d$CJT_rec (recodage Vivre en couple)
base_1$CJT_rec <- as.character(base_1$CJT)
base_1$CJT_rec[base_1$CJT_rec == "0"] <- "Pas de conjoint"
base_1$CJT_rec[base_1$CJT_rec == "1"] <- "Conjoint dans ménage"
base_1$CJT_rec[base_1$CJT_rec == "2"] <- "Conjoint en EHPAD"
table(base_1$CJT_rec) # verification
## Reordering d$CJT_rec
base_1$CJT_rec <- base_1$CJT_rec %>%
  fct_relevel(
    "Pas de conjoint", "Conjoint dans ménage", "Conjoint en EHPAD"
  )

table(base_1$CJT_rec)

table(base_1$CJT_rec, base_1$Celib_qd_2)
table(base_1$CJT_rec, base_1$menag)

# Vivre seul, depuis qunand ?

unique(base_1$V11_)
table(base_1$V11_)
## Recoding d$V11_ into d$V11__rec 
base_1$Celib_qd <- as.character(base_1$V11_)
base_1$Celib_qd[base_1$Celib_qd == "1"] <- "Moins d'1 an"
base_1$Celib_qd[base_1$Celib_qd == "2"] <- "De 1 à 3 ans"
base_1$Celib_qd[base_1$Celib_qd == "3"] <- "De 4 à 10 ans"
base_1$Celib_qd[base_1$Celib_qd == "4"] <- "Plus de 10 ans"
table(base_1$Celib_qd)
## Reordering d$V11__rec
base_1$Celib_qd <- base_1$Celib_qd %>%
  fct_relevel(
    "Moins d'1 an", "De 1 à 3 ans", "De 4 à 10 ans", "Plus de 10 ans"
  )

## Recodage de base_1$Celib_qd en base_1$Celib_qd_2
base_1$Celib_qd_2 <- base_1$Celib_qd %>%
  fct_recode(
    "Récemment" = "Moins d'1 an",
    "Récemment" = "De 1 à 3 ans",
    "Longtemps" = "De 4 à 10 ans",
    "Longtemps" = "Plus de 10 ans"
  )

table(base_1$Celib_qd_2)

# Composition du ménage 

unique(base_1$V12_)
table(base_1$V12_)
## Recoding d$V12_ into d$V12__rec (recodage Composition ménage)
base_1$menag <- as.character(base_1$V12_)
base_1$menag[base_1$menag == "1"] <- "Je vis seul(e)"
base_1$menag[base_1$menag == "2"] <- "Je vis avec mon/ma conjoint(e) seulement"
base_1$menag[base_1$menag == "3"] <- "Je vis avec mon/ma conjoint(e) et d'autres personnes"
base_1$menag[base_1$menag == "4"] <- "Je vis sans conjoint(e) mais avec d'autres personnes"
table(base_1$menag)
## Reordering d$V12__rec
base_1$menag <- base_1$menag %>%
  fct_relevel(
    "Je vis seul(e)", "Je vis avec mon/ma conjoint(e) seulement",
    "Je vis avec mon/ma conjoint(e) et d'autres personnes", "Je vis sans conjoint(e) mais avec d'autres personnes"
  )

table(base_1$menag, base_1$CJT_rec)
table(base_1$menag, base_1$Celib_qd_2)
base_1 <- base_1 %>%
  mutate(isolement = case_when(
    menag == "Je vis seul(e)" & Celib_qd_2 == "Longtemps" ~ "très isolé",
    menag == "Je vis seul(e)" & Celib_qd_2 == "Récemment" ~ "isolé depuis peu",
    menag %in% c(
      "Je vis avec mon/ma conjoint(e) seulement",
      "Je vis avec mon/ma conjoint(e) et d'autres personnes",
      "Je vis sans conjoint(e) mais avec d'autres personnes"
    ) | CJT_rec %in% c("Conjoint dans ménage", "Conjoint en EHPAD") ~ "Pas isolé",
    TRUE ~ NA_character_  # Cas par défaut
  ))

table(base_1$isolement)

table(base_1$menag)


# Participation aux activités avec d'autres individus (V43)

base_1$activite <- as.character(base_1$V43_)
base_1$activite[base_1$V43_ == "1"] <- "Oui"
base_1$activite[base_1$V43_ == "2"] <- "Non"
table(base_1$activite)
## Reordering d$activite
base_1$activite <- base_1$activite %>%
  fct_relevel(
    "Oui", "Non"
  )

# Utilisation des activités de service proposées

unique(base_1$V69_)
table(base_1$V69_)
## Recoding d$V69_ into base_1$utserv (Utilisation des services et participation)
base_1$utserv <- as.character(base_1$V69_)
base_1$utserv[base_1$utserv == "1"] <- "Je les utilise ou participe presque tous les jours"
base_1$utserv[base_1$utserv == "2"] <- "Je les utilise ou participe plusieurs fois par semaine"
base_1$utserv[base_1$utserv == "3"] <- "Je les utilise ou participe peu"
base_1$utserv[base_1$utserv == "4"] <- "Je ne les utilise pas ou ne participe pas"
table(base_1$utserv)
## Reordering base_1$utserv
base_1$utserv <- base_1$utserv %>%
  fct_relevel(
    "Je les utilise ou participe presque tous les jours", "Je les utilise ou participe plusieurs fois par semaine",
    "Je les utilise ou participe peu", "Je ne les utilise pas ou ne participe pas"
  )

# Faible utilisation des services ? 


table(base_1$v70_r1)
# Raisons faible utilisation des services : Cela ne m’intéresse pas ou je n’en ai pas besoin

table(base_1$v70_r2)
# Raisons faible utilisation des services : Je n’aime pas l’ambiance au sein de la résidence

table(base_1$v70_r4)
# Raisons faible utilisation des services : Je préfère rester dans mon logement


## Recodage de base_1$V94_ en base_1$tristesse
base_1$tristesse <- base_1$V94_ %>%
  as.character() %>%
  fct_recode(
    "Je me sens jamais triste" = "1",
    "Je suis triste quelques fois" = "2",
    "Je suis triste souvent ou tout le temps" = "3",
    "Je suis triste souvent ou tout le temps" = "4"
  )

# RECODAGE autonomie économique -------------------------------------------

# Avez-vous des transports en commun à proximité de chez vous ?
unique(base_1$V46_)
table(base_1$V46_)
## Recoding d$V46_ into base_1$transp
base_1$transp <- as.character(base_1$V46_)
base_1$transp[base_1$transp == "1"] <- "Non"
base_1$transp[base_1$transp == "2"] <- "Oui, mais je ne les utilise pas"
base_1$transp[base_1$V46_ == "3"] <- "Oui"
table(base_1$transp)
## Reordering base_1$transp
base_1$transp <- base_1$transp %>%
  fct_relevel(
    "Non", "Oui, mais je ne les utilise pas", "Oui"
  )


# Etes-vous en mesure de vous rendre seul(e) chez votre médecin généraliste,
# quel que soit le mode de transport ? 
unique(base_1$V48_)
table(base_1$V48_)
## Recoding d$V48_ into d$V48__rec
base_1$doctransp <- as.character(base_1$V48_)
base_1$doctransp[base_1$doctransp == "1"] <- "Oui"
base_1$doctransp[base_1$doctransp == "2"] <- "Non"
table(base_1$doctransp)
## Reordering d$V48__rec
base_1$doctransp <- base_1$doctransp %>%
  fct_relevel(
    "Oui", "Non"
  )

# En raison de votre âge ou de votre santé, avez-vous besoin d’une aide régulière
# pour les activités de la vie quotidienne (soins personnels, toilette, habillage, tâches
# ménagères, préparer les repas, faire les courses...) 
unique(base_1$V50_)
table(base_1$V50_)
## Recoding d$V50_ into d$V50__rec
base_1$aide <- as.character(base_1$V50_)
base_1$aide[base_1$aide == "1"] <- "Oui, j’ai besoin et je reçois de l’aide"
base_1$aide[base_1$aide == "2"] <- "Oui, j’ai besoin mais je ne reçois pas d’aide"
base_1$aide[base_1$aide == "3"] <- "Non, je n’ai pas besoin d’aide"
table(base_1$aide)
## Reordering d$V50__rec
base_1$aide <- base_1$aide %>%
  fct_relevel(
    "Oui, j’ai besoin et je reçois de l’aide", "Oui, j’ai besoin mais je ne reçois pas d’aide",
    "Non, je n’ai pas besoin d’aide"
  )


# Total revenus
unique(base_1$V96_)
table(base_1$V96_)
## Recoding d$V96_ into d$V96__rec (recodage Total revenus)
base_1$revnu <- as.character(base_1$V96_)
base_1$revnu[base_1$revnu == "1"] <- "Moins de 800€"
base_1$revnu[base_1$revnu == "2"] <- "De 800 à moins de 1200€"
base_1$revnu[base_1$revnu == "3"] <- "De 1200 à moins de 1600€"
base_1$revnu[base_1$revnu == "4"] <- "De 1600 à moins de 2000€"
base_1$revnu[base_1$revnu == "5"] <- "De 2000 à moins de 2500€"
base_1$revnu[base_1$revnu == "6"] <- "De 2500 à moins de 3000€"
base_1$revnu[base_1$revnu == "7"] <- "De 3000 à moins de 5000€"
base_1$revnu[base_1$revnu == "8"] <- "5000€ ou plus"
base_1$revnu[base_1$revnu == "9"] <- "Ne souhaite pas répondre"
base_1$revnu[base_1$revnu == "10"] <- "Ne sait pas"
table(base_1$revnu)
# réordonner psq c'est chiant à lire
## Reordering d$V96__rec
base_1$revnu <- factor(base_1$revnu,
                       levels = c(
                         "Moins de 800€", "De 800 à moins de 1200€", "De 1200 à moins de 1600€",
                         "De 1600 à moins de 2000€", "De 2000 à moins de 2500€",
                         "De 2500 à moins de 3000€", "De 3000 à moins de 5000€",
                         "5000€ ou plus", "Ne souhaite pas répondre", "Ne sait pas"
                       )
)

## Recodage de base_1$revnu en base_1$revnu_2
base_1$revnu_2 <- base_1$revnu %>%
  fct_recode(
    "Très Faible" = "Moins de 800€",
    "Très Faible" = "De 800 à moins de 1200€",
    "Moyen" = "De 1200 à moins de 1600€",
    "Moyen" = "De 1600 à moins de 2000€",
    "Elevé" = "De 2000 à moins de 2500€",
    "Elevé" = "De 2500 à moins de 3000€",
    "Elevé" = "De 3000 à moins de 5000€",
    "Elevé" = "5000€ ou plus",
    "Ne souhaite pas répondre" = "NA",
    "Ne sais pas" = "NA"
  )
unique(base_1$revnu_2)

## Recodage de base_1$revnu_2
base_1$revnu_2 <- base_1$revnu_2 %>%
  fct_recode(
    NULL = "Ne souhaite pas répondre",
    NULL = "Ne sait pas"
  )

# V97 : Equilibre budget
unique(base_1$V97_)
table(base_1$V97_)
## Recoding d$V97_ into d$V97__rec (recodage Compte tenu de votre revenu et de celui des personnes vivant 
# avec vous, diriez-vous que vous arrivez à équilibrer votre budget ...)
base_1$equibudg <- as.character(base_1$V97_)
base_1$equibudg[base_1$equibudg == "1"] <- "Avec beaucoup de difficultés"
base_1$equibudg[base_1$equibudg == "2"] <- "Avec difficulté"
base_1$equibudg[base_1$equibudg == "3"] <- "Assez facilement"
base_1$equibudg[base_1$equibudg == "4"] <- "Facilement"
table(base_1$equibudg)

## Recodage de base_1$equibudg en base_1$equibudg_rec
base_1$equibudg_2 <- base_1$equibudg %>%
  fct_recode(
    "Avec difficulté" = "Avec beaucoup de difficultés",
    "Facilement" = "Assez facilement")
## Reordering d$V97__rec
base_1$equibudg <- factor(base_1$equibudg,
                          levels = c(
                            "Avec beaucoup de difficultés", "Avec difficulté", "Assez facilement",
                            "Facilement"
                          )
)


# RECODAGE autonomie fonctionnelle ----------------------------------------

# Êtes-vous limité(e), depuis au moins six mois, à cause d’un problème de santé, dans
# les activités que les gens font habituellement ?
table(base_1$V78_)
## Recoding d$V78_ into d$V78__rec
base_1$pbsante <- base_1$V78_ %>%
  as.character() %>%
  fct_recode(
    "Oui, fortement limité(e)" = "1",
    "Oui, limité(e) mais pas fortement" = "2",
    "Non, pas limité(e) du tout" = "3"
  )

table(base_1$V77_)
## Recodage de base_1$V77_ en base_1$maladchron
base_1$maladchron <- base_1$V77_ %>%
  as.character() %>%
  fct_recode(
    "Oui" = "1",
    "Non" = "2"
  )


table(base_1$pbsante)

# Avez -vous des difficultés pour monter un étage d’escalier ou marcher sur 500 mètres
# si vous ne portez aucune charge ?
table(base_1$V81_)
## Recoding d$V81_ into d$V81__rec
base_1$pbescal <- base_1$V81_ %>%
  as.character() %>%
  fct_recode(
    "Non, aucune" = "1",
    "Oui, un peu" = "2",
    "Oui, beaucoup" = "3",
    "Je ne peux pas du tout" = "4"
  )
table(base_1$pbescal)

# Créer une nouvelle variable 'autofonct' avec Oui ou Non
base_1 <- base_1 %>%
  mutate(autofonct = ifelse(
    (pbsante == "Oui, fortement limité(e)" | 
       pbescal == "Oui, beaucoup" | 
       pbescal == "Je ne peux pas du tout" ), 
    "Faible", 
    "Elevée"
  ))

table(base_1$autofonct)

# Avez-vous perdu de l’appétit récemment ?
table(base_1$V82_)
## Recoding d$V82_ into d$V82__rec
base_1$pertap <- base_1$V82_ %>%
  as.character() %>%
  fct_recode(
    "Oui" = "1",
    "Non" = "2"
  )
table(base_1$pertap)


# Avez-vous perdu involontairement au moins 4,5 kg au cours des 12 derniers mois ?
table(base_1$V83_)
## Recoding d$V83_ into d$V83__rec
base_1$pertpd <- base_1$V83_ %>%
  as.character() %>%
  fct_recode(
    "Oui" = "1",
    "Non" = "2"
  )
table(base_1$pertpd)

# Au cours des 6 derniers mois, avez-vous consulté un médecin ?
table(base_1$V84_)
## Recoding d$V84_ into d$V84__rec
base_1$consultmed <- base_1$V84_ %>%
  as.character() %>%
  fct_recode(
    "Oui" = "1",
    "Non" = "2"
  )
table(base_1$consultmed)

# Au cours des 12 derniers mois, avez-vous été hospitalisé(e) pour au moins une nuit ?
table(base_1$V85_)
## Recoding d$V85_ into d$V85__rec
base_1$hospi <- base_1$V85_ %>%
  as.character() %>%
  fct_recode(
    "Oui" = "1",
    "Non" = "2"
  )
table(base_1$hospi)


# Êtes-vous tombé(e) au cours des 12 derniers mois ?
table(base_1$v86)
## Recoding d$v86 into d$v86_rec
base_1$chute <- base_1$v86 %>%
  as.character() %>%
  fct_recode(
    "Oui" = "1",
    "Non" = "2"
  )
table(base_1$chute)

# Chute non
table(base_1$v86_r1)
base_1$chute_non <- base_1$v86_r1

# Chute Oui, à l'intérieur du logement
table(base_1$v86_r2)
base_1$chute_interlog <- base_1$v86_r2


# Chute Oui, à l'extérieur du logement
table(base_1$v86_r3)
base_1$chute_extlog <- base_1$v86_r3


# Si vous êtes tombé(e) :
# Avez-vous chuté plusieurs fois ? 
table(base_1$V87_)
## Recoding d$V87_ into d$V87__rec
base_1$chute_plsr <- base_1$V87_ %>%
  as.character() %>%
  fct_recode(
    "Oui" = "1",
    "Non" = "2"
  )
table(base_1$chute_plsr)

# Si vous êtes tombé(e) : Avez-vous pu vous relever seul(e) ?
table(base_1$V88_)
## Recoding d$V88_ into d$V88__rec
base_1$chute_relev <- base_1$V88_ %>%
  as.character() %>%
  fct_recode(
    "Oui" = "1",
    "Non" = "2"
  )
table(base_1$chute_relev)

# Si vous êtes tombé(e) : Cette chute vous a-t-elle causé des fractures ?
table(base_1$V89_)
## Recoding d$V89_ into d$V89__rec
base_1$chute_fract <- base_1$V89_ %>%
  as.character() %>%
  fct_recode(
    "Oui" = "1",
    "Non" = "2"
  )
table(base_1$chute_fract)

# Si vous êtes tombé(e) : Cette chute a-t-elle entraîné une hospitalisation ?
table(base_1$V90_)
## Recoding d$V90_ into d$V90__rec
base_1$chute_hospi <- base_1$V90_ %>%
  as.character() %>%
  fct_recode(
    "Oui" = "1",
    "Non" = "2"
  )
table(base_1$chute_hospi)

# Créer une nouvelle variable 'chute_grave' avec Oui ou Non
base_1 <- base_1 %>%
  mutate(chute_grave = ifelse(
    (chute == "Oui" &
       chute_fract == "Oui" | 
       chute_hospi == "Oui" ), 
    "Oui", 
    "Non"
  ))

## Recodage de base_1$V17_ en base_1$V17__rec
base_1$V17__rec <- base_1$V17_ %>%
  as.character() %>%
  fct_recode(
    "Logement très confortable" = "1",
    "Logement plutôt confortable" = "2",
    "Logement pas confortable" = "3",
    "Logement pas confortable" = "4"
  )


# SUPPRIMER les variables d'origine  --------------------------------------


# Identifier les colonnes à supprimer
colonnes_a_supprimer <- grep("^v|^V", names(base_1), value = TRUE) # Colonnes commençant par v ou V
colonnes_a_supprimer <- colonnes_a_supprimer[!grepl("[rR]", colonnes_a_supprimer)] # Exclure celles contenant r ou R
print(colonnes_a_supprimer)

# Supprimer les variables voulues
base_1 <- base_1[, setdiff(names(base_1), colonnes_a_supprimer)]



# MANIPULATION des données ------------------------------------------------

# Vérification des variables de sélection

base_1$isolement <- as.factor(base_1$isolement)
base_1$autofonct <- as.factor(base_1$autofonct)
base_1$chute_grave <- as.factor(base_1$chute_grave)

class(base_1$equibudg_2)

unique(base_1$TYPLOG_rec)
unique(base_1$isolement)
table(base_1$Sexe)
table(base_1$autofonct)
table(base_1$aide)
table(base_1$activite)
table(base_1$hospi)
table(base_1$chute_grave)
table(base_1$revnu_2)
table(base_1$equibudg_2)

base_2 <- base_1 %>%
  select(TYPLOG_rec, isolement, Sexe, autofonct, aide, activite, hospi, chute_grave, revnu_2, equibudg_2, finscol_2, )

acm2 <- dudi.acm(base_2, scannf = FALSE, nf = Inf)

explor::explor(acm2)
