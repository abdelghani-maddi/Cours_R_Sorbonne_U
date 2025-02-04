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
# L’indexation par condition consiste à fournir un vecteur logique indiquant si
# chaque élément doit être inclus (si TRUE) ou exclu (si FALSE). Par exemple :

sexe
sexe[c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)]

urbain <- c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE)
poids <- c(80, 63, 75, 87, 82, 67)

# Opérateur de comparaison	à connaitre :
    # ==	égal à
    # !=	différent de
    # >	strictement supérieur à
    # <	strictement inférieur à
    # >=	supérieur ou égal à
    # <=	inférieur ou égal à

tailles[poids >= 80]

# Opérateur logique	à connaitre
    # &	et logique
    # |	ou logique
    # !	négation logique

poids >= 80 & urbain
poids >= 80 | urbain

# Assignation par indexation
    # Dans tous les exemples précédents, on a utilisé l’indexation pour extraire une
    # partie d’un vecteur, en plaçant l’opération d’indexation à 
    # droite de l’opérateur <-.

sexe[c(1, 3, 4)] <- c("Homme", "Homme", "Homme")
sexe[c(1, 3, 4)] <- "Homme"

#### LISTES ET TABLEAUX DE DONNEES
 # LES LISTES :
    # Par nature, les vecteurs ne peuvent contenir que des valeurs de même type 
    # (numérique, textuel ou logique). Or, on peut avoir besoin de représenter des 
    # objets plus complexes composés d’éléments disparates. C’est 
    # ce que permettent les listes.

l1 <- list(1:5, "abc") 
l1
length(l1)

# On peut les nommer
l2 <- list(
  minuscules = letters, 
  majuscules = LETTERS, 
  mois = month.name
)
l2
length(l2)

l <- list(l1, l2)
# Question : qu'elle sera la longueur de la liste "l" ??
length(l)

## str : permet de visualiser la structure d'un objet :
str(l)

## Pour combiner les éléments d’une liste, il faut utiliser la fonction append()
l <- append(l1, l2)
length(l)

## L'indexation : On peut utiliser à la fois l’indexation par position, l’indexation par nom et l’indexation par condition.
l
l[c(1,3,4)]
l[c("majuscules", "minuscules")]
l[c(TRUE, TRUE, FALSE, FALSE, TRUE)]
str(l[1])

## opérations sur les listes : utiliser les doubles corchets
mean(l[1]) # Ne fonctionne pas !
mean(l[[1]]) # fonctionne :) -- on récupère le vecteur à l'intérieur de la liste.
l[["mois"]]


##########
# TABLEAUX DE DONNEES : On peut créer un tableau de données avec la fonction data.frame() :
df <- data.frame(
  sexe =  c("f", "f", "h", "h"), 
  age = c(52, 31, 29, 35), 
  blond = c(FALSE, TRUE, TRUE, FALSE)
)
df
str(df) # structure du df
length(df) # nombre de vecteurs / variables
names(df) # libellés des colonnes
nrow(df) # nombre de lignes
ncol(df) # nombre de colonnes
dim(df) # dimensions
row.names(df) <- c("Anna", "Mary-Ann", "Michael", "John") # on peut nommer les lignes comme les colonnes !
df

### Indexation : 
# Les tableaux de données étant des listes, nous pouvons donc utiliser les
# crochets simples ([]), les crochets doubles ([[]]) et le symbole dollar ($) 
# pour extraire des parties de notre tableau, de la même manière que pour n’importe quelle liste.
df[1]
df[[1]]
mean(df[2])
mean(df[[2]])

df$sexe

df[3, 2] # valeur du croisement ligne 3 et colonne 2
df["Michael", "age"]
df[3, "age"]
df["Michael", 2]

df[1:2,] #extraire les lignes 1 et 2, et toutes les colonnes
df[,c("sexe", "blond")] #extraire les colonnes sexe et blond, pour tous les individus

#NB : LA VIRGULE EST IMPORTANTE POUR QUE R SACHE CE QU'ON VEUT (ligne, colone...)
df[2, ]
df[, 2]
df[2]

## on peut aussi faire : 
str(df[2, ]) # par exemple

 
###################################################
###################################################
##            Afficher les données               ##
###################################################
###################################################
# Chargement d'un jeu de données via l'extension "questionr"
library(questionr)
data(hdv2003)

View(hdv2003) #Visualiser les données

head(hdv2003) #Afficher les premières lignes d'un tableau de données
tail(hdv2003, 2) #Afficher les dernières lignes d'un tableau de données (ici, on lui a demandé les 2 dernières)

#### Utilisation du package "dplyr" : qui permet, entre autres, de visualiser rapidement et de manière condensée le contenu d’un tableau de données
library(dplyr)
glimpse(hdv2003)

#### Utilisation du package "labelled" : permet de lister les différentes variables d’un fichier de données
library(labelled)
look_for(hdv2003)
look_for(hdv2003, "trav")

# La méthode summary() qui fonctionne sur tout type d’objet permet d’avoir 
# quelques statistiques de base sur les différentes variables de notre tableau
summary(hdv2003)
summary(hdv2003$sexe)
summary(hdv2003$age)

# Utilisation de describe
describe(hdv2003$sexe)



# Manipulation de données

# Il est fréquent d’enchainer des opérations en appelant successivement des fonctions sur le résultat de l’appel précédent.
# Prenons un exemple. Supposons que nous ayons un vecteur numérique v dont nous voulons calculer la moyenne puis l’afficher via un message dans la console. 
# Pour un meilleur rendu, nous allons arrondir la moyenne à une décimale, mettre en forme le résultat à la française, c’est-à-dire avec la virgule comme 
# séparateur des décimales, créer une phrase avec le résultat, puis l’afficher dans la console. Voici le code correspondant, étape par étape.

v <- c(1.2, 8.7, 5.6, 11.4)
m <- mean(v)
r <- round(m, digits = 1)
f <- format(r, decimal.mark = ",")
p <- paste0("La moyenne est de ", f, ".")
message(p)

# Cette écriture, n’est pas vraiment optimale, car cela entraine la création d’un grand nombre de variables intermédiaires totalement inutiles. 
# Nous pourrions dès lors imbriquer les différentes fonctions les unes dans les autres :

message(paste0("La moyenne est de ", format(round(mean(v),        digits = 1), decimal.mark = ","), "."))


# Le pipe de R
# Depuis la version 4.1, R a introduit ce que l’on nomme un pipe (tuyau en anglais), un nouvel opérateur noté |>.
# Le principe de cet opérateur est de passer l’élément situé à sa gauche comme premier argument de la fonction située à sa droite.
# Ainsi, l’écriture x |> f() est équivalente à f(x) et l’écriture x |> f(y) à f(x, y).

# Parfois, on souhaite passer l’objet x à un autre endroit de la fonction f() que le premier argument. Depuis la version 4.2, 
# R a introduit l’opérateur _,que l’on nomme un placeholder, pour indiquer où passer l’objet de gauche. Ainsi, x |> f(y, a = _) devient 
# équivalent à f(y, a = x). ATTENTION : le placeholder doit impérativement être transmis à un argument nommé !

v |> 
  mean() |> 
  round(digits = 1) |> 
  format(decimal.mark = ",") |> 
  paste0("La moyenne est de ", m = _, ".") |> 
  message()

# Le pipe du tidyverse : %>%
# Ce n’est qu’à partir de la version 4.1 sortie en 2021 que R a proposé de manière native un pipe, en l’occurence l’opérateur |>.
# Cet opérateur s’écrit %>% et il dispose lui aussi d’un placeholder qui est le .. La syntaxe du placeholder est un peu plus 
#souple puisqu’il peut être passé à tout type d’argument, y compris un argument sans nom. Si l’on reprend notre exemple précédent.

library(magrittr)
v %>% 
  mean() %>%
  round(digits = 1) %>%
  format(decimal.mark = ",") %>%
  paste0("La moyenne est de ", ., ".") %>%
  message()


# Facteurs et forcats : https://larmarange.github.io/guide-R/manipulation/facteurs.html 
# les facteurs sont utilisés pour représenter des variables catégorielles

x <- c("nord", "sud", "sud", "est", "est", "est") %>%
  factor()

x %>% 
  factor(levels = c("nord", "est", "sud", "ouest"))

x %>% 
  factor(levels = c("nord", "sud"))

x %>% 
  readr::parse_factor(levels = c("nord", "sud"))

f <- factor(x)
levels(f)

class(f)
typeof(f)
as.integer(f)
as.character(f)

c("supérieur", "primaire", "secondaire", "primaire", "supérieur") |> 
  ordered(levels = c("primaire", "secondaire", "supérieur"))

# Changer l’ordre des modalités
library(tidyverse)
data("hdv2003", package = "questionr")

hdv2003$qualif |> 
  questionr::freq()

# Inverser l'ordre
hdv2003$qualif |> 
  fct_rev() |> 
  questionr::freq()

# spécifier l'ordre
hdv2003$qualif |> 
  fct_relevel("Cadre", "Autre", "Technicien", "Employe") |> 
  questionr::freq()

# ordonner par frequence - décroissant
hdv2003$qualif |> 
  fct_infreq() |> 
  questionr::freq()

# ordonner par frequence - croissant
hdv2003$qualif |> 
  fct_infreq() |> 
  fct_rev() |> 
  questionr::freq()


v <- c("c", "a", "d", "b", "a", "c")
factor(v)
fct_inorder(v) # ordonner par ordre d'apparance dans le vecteur

# trier les modalités de la variable qualif en fonction de l’âge moyen (dans chaque modalité)
hdv2003$qualif_tri_age <-
  hdv2003$qualif |> 
  fct_reorder(hdv2003$age, .fun = mean)

  # vérification :
  hdv2003 |> 
    dplyr::group_by(qualif_tri_age) |> 
    dplyr::summarise(age_moyen = mean(age))

  
# Modifier les modalités
hdv2003$sexe |> 
  questionr::freq()

hdv2003$sexe <- 
  hdv2003$sexe |> 
  fct_recode(f = "Femme", m = "Homme")
hdv2003$sexe |> 
  questionr::freq()

hdv2003$nivetud |> 
  questionr::freq()

hdv2003$instruction <- 
  hdv2003$nivetud |> 
  fct_recode(
    "primaire" = "N'a jamais fait d'etudes",
    "primaire" = "A arrete ses etudes, avant la derniere annee d'etudes primaires",
    "primaire" = "Derniere annee d'etudes primaires",
    "secondaire" = "1er cycle",
    "secondaire" = "2eme cycle",
    "technique/professionnel" = "Enseignement technique ou professionnel court",
    "technique/professionnel" = "Enseignement technique ou professionnel long",
    "supérieur" = "Enseignement superieur y compris technique superieur"
  )
hdv2003$instruction |> 
  questionr::freq()

hdv2003$instruction <- 
  hdv2003$nivetud |> 
  fct_collapse(
    "primaire" = c(
      "N'a jamais fait d'etudes",
      "A arrete ses etudes, avant la derniere annee d'etudes primaires",
      "Derniere annee d'etudes primaires"
    ),
    "secondaire" = c(
      "1er cycle",
      "2eme cycle"
    ),
    "technique/professionnel" = c(
      "Enseignement technique ou professionnel court",
      "Enseignement technique ou professionnel long"
    ),
    "supérieur" = "Enseignement superieur y compris technique superieur"
  )

# Pour transformer les valeurs manquantes (NA) en une modalité explicite, on pourra avoir recours à forcats::fct_explicit_na().
hdv2003$instruction <-
  hdv2003$instruction |> 
  fct_explicit_na(na_level = "(manquant)")

hdv2003$instruction |> 
  questionr::freq()

hdv2003$qualif |> 
  questionr::freq()
 
# Plusieurs fonctions permettent de regrouper plusieurs modalités dans une modalité autres.
# Par exemple, avec forcats::fct_other(), on pourra indiquer les modalités à garder.
hdv2003$qualif |> 
  fct_other(keep = c("Technicien", "Cadre", "Employe")) |> 
  questionr::freq()

# La fonction forcats::fct_lump_n() permets de ne conserver que les modalités les
# plus fréquentes et de regrouper les autres dans une modalité autres.
hdv2003$qualif |> 
  fct_lump_n(n = 4, other_level = "Autres") |> 
  questionr::freq()

# Et forcats::fct_lump_min() celles qui ont un minimum d’observations.

hdv2003$qualif |> 
  fct_lump_min(min = 200, other_level = "Autres") |> 
  questionr::freq()

# Découper une variable numérique en classes

hdv2003 <-
  hdv2003 |> 
  mutate(groupe_ages = cut(age, 5))
hdv2003$groupe_ages |> questionr::freq()

hdv2003 <-
  hdv2003 |> 
  mutate(groupe_ages = cut(age, c(18, 20, 40, 60, 80, 97)))
hdv2003$groupe_ages |> questionr::freq()

hdv2003 <-
  hdv2003 |> 
  mutate(groupe_ages = cut(
    age, 
    c(18, 20, 40, 60, 80, 97),
    include.lowest = TRUE
  ))
hdv2003$groupe_ages |> questionr::freq()

hdv2003 <-
  hdv2003 |> 
  mutate(groupe_ages = cut(
    age, 
    c(18, 20, 40, 60, 80, 97),
    include.lowest = TRUE,
    right = FALSE
  ))
hdv2003$groupe_ages |> questionr::freq()

