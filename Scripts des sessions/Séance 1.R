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

##########################################################
##########################################################
# Approfondissement de certains notions vues plus haut.

# LES VECTEURS : il existe 4 types de vecteurs : nombres réels, nombres entiers,chaines de caractières et logiques (booléens).
class(12.5) # pour connaitre la classe.
class(3)
class(3L) # Pour indiquer spécifiquement qu’on veut un nombre entier, il faut rajouter le suffixe L (sinon c'est considéré comme réel)
class("abc")
class(TRUE)

# Les classes de vecteurs :
    # "factor"	facteur
    # "labelled"	vecteur labellisé
    # "Date	date"
    # "POSIXct"	date et heure

taille <- c(1.88, 1.65, 1.92, 1.76)
taille
class(taille)


sexe <- c("h", "f", "h", "f")
sexe
class(sexe)

urbain <- c(TRUE, TRUE, FALSE, FALSE)
urbain
class(urbain)

x <- c(2L, 3.14, "a")
x
class(x)

# LES FONCTIONS : rep et seq
rep(2, 10)
rep(c("a", "b"), 3)

seq(1, 10)
seq(5, 17, by = 2)
seq(10, 0)
seq(100, 10, by = -10)
seq(1.23, 5.67, by = 0.33)

# L'OPERATEUR ":"
1:5
55:43

# Quelques vecteurs remarquables :
    # R fournit quelques vecteurs particuliers qui sont directement accessibles :
        # LETTERS : les 26 lettres de l’alphabet en majuscules
        # letters : les 26 lettres de l’alphabet en minuscules
        # month.name : les noms des 12 mois de l’année en anglais
        # month.abb : la version abrégée des 12 mois en anglais
        # pi : la constante mathématique π


# Combiner des vecteurs
x <- c(2, 1, 3, 4)
length(x)
y <- c(9, 1, 2, 6, 3, 0)
length(y)

z <- c(x, y)
z
length(z)

min_maj <- c(letters, LETTERS)
min_maj
length(min_maj)

# Valeurs manquantes
taille <- c(1.88, NA, 1.65, 1.92, 1.76, NA)
sexe <- c("h", "f", NA, "h", NA, "f")
length(taille)

# Valeurs nulles
c(NULL, NULL, NULL)
length(c(NULL, NULL, NULL))
length(c(NA, NA, NA)) # à ne pas confondre donc :)


# Indexation par position
tailles[1]
tailles[1:3]
tailles[c(5, 8, 6)]
tailles[length(tailles)]
taille[23:25]

# Les vecteurs nommés
# Les différentes valeurs d’un vecteur peuvent être nommées. 
# Une première manière de nommer les éléments d’un vecteur est de le faire à sa création :

sexe <- c(Michel = "h", Anne = "f", Dominique = NA, Jean = "h", Claude = NA, Marie = "f")
sexe
# La liste des noms s’obtient avec names :
names(sexe)

#Ajouter ou modifier des noms : 
names(sexe) <- c("Michael", "Anna", "Dom", "John", "Alex", "Mary")
sexe

# Supprimer tous les noms :
anonyme <- unname(sexe)
anonyme

## Indexation par nom : Lorsqu’un vecteur est nommé, il est dès lors possible d’accéder 
                       #à ses valeurs à partir de leur nom. Il s’agit de l’indexation par nom.
sexe["Anna"]
sexe[c("Mary", "Michael", "John")]

# Indexation par condition





