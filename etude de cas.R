# Analyse data sherpa romeo
rm(list=ls())

# Packages
library(tidyverse)
library(openxlsx)
library(readxl)
library(questionr)
library(gtsummary)
library(labelled)



# Données
data <- read.csv2("D:/Enseignement SU 2023-24/HabitatSeniors.csv")

# Préparation

describe(d$V13_) # type de logement

# Distinguer habitat inclusif / sexe / âge
d0 <- data %>%
  mutate(h_inclusif  = ifelse(V13_ == 3, "Oui", "Non"), # habitat inclusif
         Sexe = ifelse(V6_ == 1, "Femme", "Homme"),
         Age = V7,  # Ajouter l'âge
         Revenu = as.factor(d0$V96_) # tranches de revenu
         
  ) %>%
  select(-V6_,-V7, V96_) # inutiles maintenant puisque on les a créé plus haut :) 

# Sélection de quelques variables d'intérêt

d <- d0 %>%
  select(
    Identifiant,
    h_inclusif,
    Sexe,
    Age,
    starts_with("v23_r"), # raisons déménagement
    V28_, # surface du logement
    V29_, # Nombre de marches pour accès
    V37_, # Compter sur les enfants
    V41, # qualité de vie note sur 10
    V42_, # sentiment de solitude
    starts_with("v44_r"), # moyens de déplacement
    V45_, # si quartier adapté déplacement à pied
    V46_, # proximité transports en commun
    v47_48, # sortir seul (courses ou médecin)
    V50_, # besoin d'aide
    V52_nb, # aider les autres
    V54_nb, # si incité à emménager dans la structure
    starts_with("v55_r"), # raisons emménagement
    V58_64_ok, # cptage des D_accord et Plutôt d_accord
    v58_64_non, # inverse de V58_64_ok
    V65_, # sentiment sécurité nouveau logement
    V76_, # état de santé général
    V78_, # limité à cause d'un pbm de santé +6 mois
    v86_r2, # Chute l’intérieur au cours des 12 derniers mois
    v86_r3, # Chute l’extérieur au cours des 12 derniers mois
    V94_, # sentiment de tristesse
    V121_, # autonomie numérique
    Revenu # Revenus
)

## Recodage de d$Revenu
d$Revenu <- d$Revenu %>%
  fct_recode(
    "Très faibles" = "1",
    "Faibles" = "2",
    "Faibles" = "3",
    "Moyens" = "4",
    "Moyens" = "5",
    "Moyens" = "6",
    "Élevés" = "7",
    "Élevés" = "8",
    "Élevés" = "9",
    "Ne souhaite pas répondre" = "10"
  ) %>%
  fct_explicit_na("Ne sait pas")

## Recodage de d$V28_
d$V28_ <- d$V28_ %>%
  as.character() %>%
  fct_recode(
    "Très petit logement" = "1",
    "Petits logement" = "2",
    "Petits logement" = "3",
    "Logement moyen" = "4",
    "Logement moyen" = "5",
    "Grand logement" = "6"
  )

## Recodage de d$V65_
d$V65_ <- d$V65_ %>%
  as.character() %>%
  fct_recode(
    "Oui" = "1",
    "Non" = "2"
  )

## Recodage de d$V42_
d$V42_ <- d$V42_ %>%
  as.character() %>%
  fct_recode(
    "Souvent" = "1",
    "Parfois" = "2",
    "Presque jamais ou jamais" = "3"
  )

## Recodage de d$V42_
d$V37_ <- d$V37_ %>%
  as.character() %>%
  fct_recode(
    "Oui" = "1",
    "Non" = "2"
  )

## Recodage de d$V45_
d$V45_ <- d$V45_ %>%
  as.character() %>%
  fct_recode(
    "Quartier adapté" = "1",
    "Quartier adapté" = "2",
    "Quartier pas adapté" = "3",
    "Quartier pas adapté" = "4"
  )

## Recodage de d$V121_
d$V121_ <- d$V121_ %>%
  as.character() %>%
  fct_recode(
    "Oui, facilement" = "1",
    "Oui, avec difficulté" = "2",
    "Non, j'ai besoin d'aide pour le faire" = "3",
    "Non, je n'utilise pas internet, même avec de l'aide" = "4"
  )

# Transformation des modalités de déplacement en format long
d_deplac <- d %>%
  select(Identifiant, Sexe, h_inclusif, starts_with("v44_r")) %>%
  unique() %>%
  pivot_longer(
    cols = starts_with("v44_r"),  # Sélection des colonnes V55_r1 à V55_r10
    names_to = "Deplac_Code",
    values_to = "Deplacement"
  ) %>%
  filter(!is.na(Deplacement))

## Recodage de d_deplac$Deplacement en d_deplac$Deplacement_rec
d_deplac$Deplacement_rec <- d_deplac$Deplacement %>%
  as.character() %>%
  fct_recode(
    "Deux roues" = "1",
    "À pied" = "2",
    "À vélo" = "3",
    "Transports" = "4",
    "Autrement" = "5",
    "Je ne sors pas" = "6"
  )

# Tableau synthétique modalités de déplacement selon type logement
d_deplac %>%
  tbl_summary(
    include = c(Sexe, Deplacement_rec),
    by = h_inclusif
  )
# Tableau synthétique modalités de déplacement en habitat inclusif selon le sexe
d_deplac %>%
  filter(h_inclusif == "Oui") %>%
  tbl_summary(
    include = c(Sexe, Deplacement_rec),
    by = Sexe,
    label = list(
      Deplacement_rec ~ "Modalités de déplacement"
    )
  ) %>%
  bold_labels() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Modalités de déplacement en habitat inclusif selon le sexe**") %>%
  add_overall(last = F)

# Transformation des raisons souhait déménagement en format long
d_long <- d %>%
  select(Identifiant, starts_with("v23_r")) %>%
  unique() %>%
  pivot_longer(
    cols = starts_with("v23_r"),  # Sélection des colonnes V55_r1 à V55_r10
    names_to = "Raison_Code",
    values_to = "Response"
  ) %>%
  filter(!is.na(Response))

## Recodage de d2$Raison_Code
d_long$Raison_Code <- d_long$Raison_Code %>%
  fct_recode(
    "Rapprochements sociaux"    =     "v23_r1",
    "Rapprochements sociaux"    =     "v23_r2",
    "Santé et sécurité"         =     "v23_r3",
    "Santé et sécurité"         =     "v23_r4",
    "Praticité et localisation" =     "v23_r5",
    "Praticité et localisation" =     "v23_r6",
    "Praticité et localisation" =     "v23_r7",
    "Praticité et localisation" =     "v23_r8",
    "Raisons de coûts"          =     "v23_r9",
    "Praticité et localisation" =     "v23_r10",
    "Autres raisons"            =     "v23_r11"
  )


# Tableau synthétique pour les caractéristiques principales
d %>%
  select(Age, Sexe, h_inclusif, V28_, V29_, V65_, V37_, V42_, V41, V121_, Revenu) %>%
  tbl_summary(by = h_inclusif,
              label = list(
                Age ~ "Âge moyen (Q1, Q3)",
                Sexe ~ "Sexe des individus",
                V28_ ~ "Surface du logement",
                V29_ ~ "Nombre de marches d'accès",
                V65_ ~ "Sentiment de sécurité nouv. logt",
                V37_ ~ "Compter sur les enfants",
                V42_ ~ "Sentiment de solitude",
                V41 ~ "Qualité de vie (score)",
                V121_ ~ "Internet pour démarches en ligne",
                Revenu ~ "Tranches de revenu"),
              missing = "no"
  ) %>%
  bold_labels() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Logement inclusif**") %>%
  add_p() %>%
  separate_p_footnotes() %>%
  add_overall(last = F)



d2 <- d %>%
  left_join(d_long, by = "Identifiant")


# Raisons d'emménagement selon le sexe
d2 %>%
  filter(d2$h_inclusif == "Oui") %>%
  select(Sexe, Raison_Code) %>%
  tbl_summary(by = Sexe,
              label = list(Raison_Code ~ "Raison souhait de déménagement"),
              missing = "no"
  ) %>%
  bold_labels() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Sexe des individus**") %>%
  add_p() %>%
  separate_p_footnotes() %>%
  add_overall(last = F)


# Transformer les données en format long et calculer les pourcentages
data_long <- data %>%
  pivot_longer(cols = starts_with("V55_r"), names_to = "Raison", values_to = "Value") %>%
  filter(!is.na(Value)) %>%  # Garder uniquement les réponses cochées
  mutate(Sexe = ifelse(V6_ == 1, "Femme", "Homme")) %>%
  count(Sexe, Raison) %>%  # Comptage des réponses pour chaque raison par sexe
  group_by(Sexe) %>%
  mutate(Percent = n / sum(n) * 100)  # Calculer les pourcentages par sexe


# Créer le graphique
ggplot(data_long, aes(x = Raison, y = Percent, fill = Sexe)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Raisons d'emménagement par sexe (en %)",
    x = "Raisons",
    y = "Pourcentage"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Affichage en pourcentage
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Transformer les données en format long et calculer les pourcentages
data_long <- data %>%
  pivot_longer(cols = starts_with("V18_r"), names_to = "Raison", values_to = "Value") %>%
  filter(!is.na(Value)) %>%  # Garder uniquement les réponses cochées
  mutate(Sexe = ifelse(V6_ == 1, "Femme", "Homme")) %>%
  count(Sexe, Raison) %>%  # Comptage des réponses pour chaque raison par sexe
  group_by(Sexe) %>%
  mutate(Percent = n / sum(n) * 100)  # Calculer les pourcentages par sexe


# Créer le graphique
ggplot(data_long, aes(x = Raison, y = Percent, fill = Sexe)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Raisons d'emménagement par sexe (en %)",
    x = "Raisons",
    y = "Pourcentage"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Affichage en pourcentage
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#####
## Recodage de data_expanded$Age en data_expanded$Age_rec
d$Age_rec <- cut(d$Age,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(55, 70, 79, 84, 90, 99)
)


# Test ACM
d$Sexe <- as.factor(d$Sexe)

#data_expanded$Raison <- as.factor(data_expanded$Raison)
# Ajouter des labels directement au dataframe
d <- d %>%
  set_variable_labels(
    Age  = "Âge moyen (Q1, Q3)",
    Sexe = "Sexe des individus",
    V28_ = "Surface du logement",
    V29_ = "Nombre de marches d'accès",
    V65_ = "Sentiment de sécurité nouv. logt",
    V37_ = "Compter sur les enfants",
    V42_ = "Sentiment de solitude",
    V41  = "Qualité de vie (score)",
    V45_ = "Quartier adapté déplacement à pied",
    V121_= "Internet pour démarches en ligne",
    Revenu = "Tranches de revenu"
  )


data_acm <- d_deplac %>%
  select(-Deplac_Code, -Deplacement, -Identifiant) %>%
  as.data.frame()


data_acm$Sexe <- as.factor(data_acm$Sexe)
data_acm$h_inclusif <- as.factor(data_acm$h_inclusif)

acm <- dudi.acm(data_acm, scannf = F, nf = Inf)

explor::explor(acm)
res <- explor::prepare_results(acm)
explor::MCA_ind_plot(res, xax = 1, yax = 2, ind_sup = FALSE, lab_var = NULL,
                     ind_lab_min_contrib = 0, col_var = "Sexe", labels_size = 9, point_opacity = 0.5,
                     opacity_var = NULL, point_size = 64, ellipses = TRUE, transitions = TRUE,
                     labels_positions = NULL, xlim = c(-2.71, 3.19), ylim = c(-1.67, 4.23))




