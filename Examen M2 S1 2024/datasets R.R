# Charger les bibliothèques nécessaires
library(dplyr)

# Lister tous les jeux de données disponibles
datasets <- data(package = .packages(all.available = TRUE))

# Convertir les informations en dataframe
datasets_df <- as.data.frame(datasets$results)

# Renommer les colonnes pour plus de clarté
colnames(datasets_df) <- c("Nom", "Package", "Titre", "Chemin")

# Afficher un aperçu des jeux de données disponibles
print("Liste des jeux de données disponibles :")
head(datasets_df)

# Optionnel : Sauvegarder la liste dans un fichier CSV pour consultation
write.csv(datasets_df, "liste_datasets_R.csv", row.names = FALSE)

# Filtrer par package ou mot-clé si nécessaire
# Exemple : Jeux de données contenant "health" dans le titre
datasets_health <- datasets_df %>%
  filter(grepl("health", Titre, ignore.case = TRUE))

# Afficher les jeux de données filtrés
print("Jeux de données contenant 'health' dans le titre :")
print(datasets_health)
