# Présentation de l'interface Rstudio

# L’interface de RStudio est divisée en quatre quadrants :
  
# le quadrant supérieur gauche est dédié aux différents fichiers de travail ;
# le quadrant inférieur gauche correspond à ce que l’on appelle la console, c’est-à-dire à R proprement dit ;
# le quadrant supérieur droit permet de connaître : liste des objets, connections, historique, Git, etc.
# le quadrant inférieur droit affiche : graphiques, packages, aide, etc.

# Faire quelques opérations dans la console
15+2
-3/10

# Astuces : Historique + Flèches

# Création d'objets
x <- 2 # favoriser cette écriture pour éviter les ambiguités
x = 2
2 -> x

# Opérations sur les objets
x <- 27
y <- 10
foo <- x + y
foo

# Voir cadrant supérieur droit : environnement
x <- "Hello"
foo <- x
foo

# Les vecteurs : Une liste de valeurs
tailles <- c(167, 192, 173, 174, 172, 167, 171, 185, 163, 170)
tailles
  # Opération sur les vecteurs possibles :
     # Exemple 1 : Transformer en mettre :
tailles_m = tailles/100
tailles_m

     # Exemple 2 : calcul de l'IMC
tailles <- c(167, 192, 173, 174, 172, 167, 171, 185, 163, 170)
poids <- c(86, 74, 83, 50, 78, 66, 66, 51, 50, 55)
tailles.m <- tailles/100
imc <- poids/(tailles.m^2)
imc

  # Vecteurs textuels :
reponse <- c("Bac+2", "Bac", "CAP", "Bac", "Bac", "CAP", "BEP")
reponse
     # Selectionner une réponse
reponse[2]
reponse[2:3]

# Les fonctions : ont un nom, acceptent des arguments et retournent un résultat
tailles <- c(167, 192, 173, 174, 172, 167, 171, 185, 163, 170)
length(tailles) # longueur du vecteur
mean(tailles) # moyenne du vecteur numérique

  # Les arguments
tailles <- c(167, NA, 173, 174, 172, 167, 171, 185, 163, 170) # valeur manquante !
mean(tailles) # ne fonctionne pas

?mean
help("mean")

mean(tailles, na.rm = TRUE) # Voir l'aide pour plus d'arguments (ex. supprimer les valeurs extrêmes) : 
max(tailles)
min(tailles)

  # Exemples de fonctions utiles :
      # "c"	construit un vecteur à partir d’une série de valeurs
      # "length"	nombre d’éléments d’un vecteur
      # "mean"	moyenne d’un vecteur de type numérique
      # "var"	variance d’un vecteur de type numérique
      # "+, -, *, /"	opérateurs mathématiques de base
      # "ˆ"	passage à la puissance
      # "max" et "min"
      # "sd" écart-type
      # ":" génère une séquence (1:5)
      # ...







