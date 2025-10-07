########################################
# Dossier 4 
# Analyse_descriptive_R_RP2018.R
# Analyse descriptive pas-à-pas du jeu de données rp2018 (recensement communal)
# Auteur : MADDI
# But : produire contrôles, tableaux et graphiques réutilisables + tests simples et sorties.
# PRÉREQUIS : avoir l'objet `rp2018` chargé dans l'environnement R (ou fournir un fichier rds/csv).
########################################

# Nettoyer l'espace de travail ----
# rm(list=ls())

# Lancement des packages ----
# Nous chargeons les packages nécessaires pour manipuler, analyser et présenter les données.
library(questionr)   # Pour l'accès aux jeux de données d'enquêtes
library(openxlsx)    # Pour la gestion des fichiers Excel
library(tidyverse)   # Pour la manipulation de données et les visualisations
library(gtsummary)   # Pour résumer les résultats statistiques
library(labelled)    # Pour la gestion des labels dans les données
library(ade4)
library(sp)

# Chargement des données ----
# Nous utilisons le jeu de données 'hdv2003' provenant du package questionr.
data(rp2018)
# Voir : https://www.insee.fr/fr/information/5369871 
# Voir : https://www.collectivites-locales.gouv.fr/competences/le-recensement-de-la-population#:~:text=L'Insee%20forme%20le%20personnel,la%20formation%20des%20agents%20recenseurs.&text=La%20collecte%20commence%20toujours%20le,10%20000%20habitants%20ou%20plus.

# 🏗 – Premières manipulations avec rp2018
# 1. select() – choisir des colonnes
# 
# Question : “Je veux comparer la population totale et le chômage, mais je n’ai pas besoin de toutes les autres colonnes.”

rp2018 %>% 
  select(commune, region, pop_tot, chom)


# 💬 Discussion sociologique : Est-ce que regarder la population et le chômage suffit pour comprendre la situation d’une commune ? Quelles autres variables pourraient être utiles ?
#   
#   2. filter() – filtrer des lignes
# 
# Question : “Quelles sont les communes de plus de 100 000 habitants ?”

rp2018 %>% 
  filter(pop_tot > 100000) %>% 
  select(commune, departement, pop_tot)

write.xlsx(rp2018, "D:/rp2018.xlsx")
# 💬 Discussion sociologique : Les grandes villes sont-elles forcément plus touchées par le chômage que les petites ?
#   
#   3. arrange() – trier
# 
# Question : “Quelles sont les 10 communes avec le plus fort taux de chômage ?”

rp2018 %>% 
  arrange(desc(chom)) %>% 
  select(commune, region, chom) %>% 
  head(20)

# 
# 💬 Discussion sociologique : Les communes avec fort chômage sont-elles concentrées dans certaines régions ?
#   
#   4. mutate() – créer de nouvelles variables
# 
# Question : “On veut comparer le chômage en tenant compte de la taille de la population. Créons une variable : nombre de chômeurs.”

rp2018 %>% 
  mutate(nb_chomeurs = pop_tot * chom / 100) %>% 
  select(commune, pop_tot, chom, nb_chomeurs)


# 💬 Discussion sociologique : Une petite commune peut avoir un taux de chômage élevé, mais un nombre de chômeurs beaucoup plus faible qu’une grande ville. Qu’est-ce qui est le plus parlant sociologiquement : le taux ou le nombre ?
#   
#   5. rename() – renommer
# 
# Question : “Les noms abrégés ne sont pas toujours clairs. Renommons pop_tot en population_totale.”

rp2018 %>% 
  select(commune, pop_tot) %>%
  rename(population_totale = pop_tot)

# 
# 💬 Discussion sociologique : Pourquoi est-il important d’avoir des noms de variables clairs et compréhensibles, surtout pour partager ses résultats ?


# 🏗 Bloc 2 – Résumer et catégoriser les données avec rp2018
# 6. group_by() + summarise() – calculer des résumés par groupe
# 
# Question : “Quel est le taux de chômage moyen par région ?”

rp2018 %>% 
  group_by(region) %>% 
  summarise(chom_moy = mean(chom, na.rm = TRUE)) %>% 
  arrange(desc(chom_moy))


library(dplyr)

rp2018 %>%
  group_by(region) %>%
  summarise(
    chom_moy = weighted.mean(chom, w = pop_tot, na.rm = TRUE)  # moyenne pondérée par pop
  ) %>%
  arrange(desc(chom_moy))



# 💬 Discussion sociologique : Les disparités régionales en matière de chômage reflètent-elles des différences économiques, sociales ou démographiques ? Quels autres indicateurs faudrait-il regarder (niveau d’éducation, structure des emplois, etc.) ?
#   
#   7. distinct() – obtenir des valeurs uniques
# 
# Question : “Combien de régions et de départements différents contient notre base ?”

rp2018 %>% distinct(region) %>% nrow()
rp2018 %>% distinct(departement) %>% nrow()


# 💬 Discussion sociologique : Pourquoi est-il important de vérifier le nombre de régions/départements représentés ? Que signifierait une absence ou une surreprésentation de certaines zones ?
#   
#   8. case_when() – créer des catégories conditionnelles
# 
# Question : “Classons les communes selon leur taille :
#   
#   Petite : < 2 000 habitants
# 
# Moyenne : entre 2 000 et 10 000
# 
# Grande : > 10 000”

rp2018 %>% 
  mutate(categorie_pop = case_when(
    pop_tot < 5000 ~ "Petite",
    pop_tot >= 5000 & pop_tot <= 10000 ~ "Moyenne",
    pop_tot > 10000 ~ "Grande"
  )) %>% 
  count(categorie_pop)


# 💬 Discussion sociologique : Les enjeux (emploi, logement, éducation) sont-ils les mêmes dans une petite commune rurale et dans une grande ville ? Comment la taille de la commune influence-t-elle la vie sociale ?
#   
#   9. summarise() + group_by() + case_when() – croiser les informations
# 
# Question : “Quel est le taux de chômage moyen selon la catégorie de commune (petite, moyenne, grande) ?”

rp2018 %>% 
  mutate(categorie_pop = case_when(
    pop_tot < 5000 ~ "Petite",
    pop_tot >= 5000 & pop_tot <= 10000 ~ "Moyenne",
    pop_tot > 10000 ~ "Grande"
  )) %>% 
  group_by(categorie_pop) %>% 
  summarise(chom_moy = mean(chom, na.rm = TRUE))


# 💬 Discussion sociologique : Les grandes villes offrent plus d’emplois mais aussi plus de concurrence sur le marché du travail. Les petites communes souffrent-elles d’un manque d’opportunités ou bénéficient-elles d’une solidarité locale plus forte ?

# 🏗 Bloc 3 – Visualiser les données de rp2018
# 10. Histogramme – distribution d’une variable
# 
# Question : “À quoi ressemble la distribution des tailles de population des communes ?”

library(ggplot2)

ggplot(rp2018, aes(x = pop_tot)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  scale_x_log10() + 
  labs(title = "Distribution de la population des communes",
       x = "Population (échelle log)", 
       y = "Nombre de communes")


# 💬 Discussion sociologique : La plupart des communes françaises sont petites, mais quelques très grandes villes pèsent lourd démographiquement. Comment cela influence-t-il la représentation politique ou les services publics ?
#   
#   11. Barplot – comparaison entre catégories
# 
# Question : “Quel est le taux de chômage moyen par catégorie de commune (petite, moyenne, grande) ?”

rp2018 %>%
  mutate(categorie_pop = case_when(
    pop_tot < 2000 ~ "Petite",
    pop_tot >= 2000 & pop_tot <= 10000 ~ "Moyenne",
    pop_tot > 10000 ~ "Grande"
  )) %>%
  group_by(categorie_pop) %>%
  summarise(chom_moy = mean(chom, na.rm = TRUE)) %>%
  ggplot(aes(x = categorie_pop, y = chom_moy, fill = categorie_pop)) +
  geom_col() +
  labs(title = "Taux de chômage moyen selon la taille de commune",
       x = "Catégorie de commune",
       y = "Chômage moyen (%)") +
  theme_minimal()


# 💬 Discussion sociologique : Le chômage est-il un problème plus marqué dans les grandes villes que dans les petites ? Pourquoi ?
#   
#   12. Boxplot – comparer distributions
# 
# Question : “Comment varie le chômage selon la région ?”

ggplot(rp2018, aes(x = region, y = chom)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  coord_flip() +
  labs(title = "Répartition du chômage par région",
       x = "Région", 
       y = "Taux de chômage (%)") +
  theme_minimal()


# 💬 Discussion sociologique : Les écarts de chômage sont-ils plus grands entre les régions ou à l’intérieur d’une même région ? Qu’est-ce que cela dit sur les inégalités territoriales ?
#   
#   13. Nuage de points – relation entre deux variables
# 
# Question : “Existe-t-il une relation entre niveau d’éducation et chômage ?” (ex. % diplômés du supérieur et chômage).

ggplot(rp2018, aes(x = dipl_sup, y = chom)) +
  geom_point(alpha = 0.4, color = "purple") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relation entre niveau d'éducation et chômage",
       x = "Part de diplômés du supérieur (%)", 
       y = "Taux de chômage (%)") +
  theme_minimal()


# 💬 Discussion sociologique : Avoir une population plus diplômée protège-t-il les communes du chômage ? Y a-t-il des exceptions ?

# 🏗 Bloc 4 – Analyses avancées avec rp2018
# 14. Corrélations – repérer des relations globales
# 
# Question : “Quelles variables socio-démographiques sont liées au chômage ?”
#install.packages("corrr")
library(corrr)

rp2018 %>% 
  select(chom, dipl_sup, cadres, femmes, age_15_29) %>% 
  correlate() %>% 
  rearrange() %>% 
  fashion()


# 💬 Discussion sociologique : Le chômage est-il davantage lié à l’âge, au niveau de diplôme ou à la structure des professions dans une commune ?
#   
#   15. ACP (Analyse en Composantes Principales) – visualiser la structure des communes
# 
# Question : “Les communes se ressemblent-elles selon leurs caractéristiques sociales et économiques ?”

library(FactoMineR)
library(factoextra)

vars_acp <- rp2018 %>% 
  select(chom, dipl_sup, dipl_aucun, cadres, pop_tot, femmes)

res_acp <- PCA(vars_acp, scale.unit = TRUE, graph = FALSE)

fviz_pca_biplot(res_acp, repel = TRUE,
                title = "ACP des communes (variables sociales)")


# 💬 Discussion sociologique : Observe-t-on une opposition entre communes “favorisées” (beaucoup de diplômés et de cadres) et “populaires” (beaucoup d’ouvriers, peu de diplômés) ? Où se situent les grandes villes dans cette typologie ?
#   
#   16. Cartographie – spatialiser les données
# 
# Question : “Comment le chômage se répartit-il dans l’espace français ?”

library(sf)
library(ggplot2)

# ⚠️ il faut télécharger les shapefiles des communes ou départements INSEE
# Exemple avec départements via rnaturalearth (plus simple pour TD)
library(rnaturalearth)
library(rnaturalearthdata)

fr_departements <- ne_download(scale = 50, type = "admin_1_states_provinces", returnclass = "sf")

# Joindre les données rp2018 agrégées par département
chom_dep <- rp2018 %>% 
  group_by(departement) %>% 
  summarise(chom_moy = mean(chom, na.rm = TRUE))

fr_map <- fr_departements %>% 
  left_join(chom_dep, by = c("name" = "departement"))

# Carte
ggplot(fr_map) +
  geom_sf(aes(fill = chom_moy), color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Taux de chômage moyen par département",
       fill = "Chômage moyen (%)")


