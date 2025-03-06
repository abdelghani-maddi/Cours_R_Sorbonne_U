
# Obtenir la liste des jeux de données disponibles
data_list <- data()

# Transformer en tibble pour un affichage plus clair
library(tibble)
dataset_table <- as_tibble(data_list$results[, c("Item", "Package", "Title")])

# Renommer les colonnes
colnames(dataset_table) <- c("Dataset", "Package", "Description")

# Afficher le tableau
print(dataset_table)

# Optionnel : Sauvegarder en CSV
# write.csv(dataset_table, "liste_datasets_R.csv", row.names = FALSE)
