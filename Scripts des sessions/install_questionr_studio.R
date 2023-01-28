#---------------------------------------------------------------------------------------------------------------------
# COMMENCEZ PAR EXECUTER CETTE PARTIE DU CODE / START BY EXECUTING THIS PART OF THE CODE

# User prompt
libraryToBeInstalled <- "questionr"
#---------------------------------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------------------------------
# PUIS EXECUTER CETTE PARTIE DU CODE / THEN EXECUTE THIS PART OF THE CODE

# Storing current working directory's location
oldWd = getwd()

# Creating folders "lib" and "DL" in working directory
dir.create("lib")
dir.create("DL")

# Adding another library path to RStudio : <wd>/lib
myPaths <- .libPaths()
myPaths <- c(myPaths, "lib")
.libPaths(myPaths)

# Downloading user-specificied package and its dependencies to the folder <wd>/DL
getPackages <- function(packs){
  packages <- unlist(
    tools::package_dependencies(packs, available.packages(), which=c("Depends", "Imports"), recursive=TRUE)
  )
  packages <- union(packs, packages)
  packages
}

packages <- getPackages(c(libraryToBeInstalled))

download.packages(packages, destdir="DL", type="win.binary")

# Installing all required packages downloaded in <wd>/DL to <wd>/lib
setwd("DL") #set the working directory to the path of the packages

pkgs1 <- list.files()

install.packages(pkgs1, repos = NULL, type = "win.binary", lib="C:/Users/Etudiants-426.LETTRES-PEDAG/Desktop/lib")

# Redefining working directory to be the one specificied before execution.
setwd(oldWd)

# Removing all working variables after execution
rm("libraryToBeInstalled", "oldWd", "myPaths", "getPackages", "packages", "pkgs1")
#---------------------------------------------------------------------------------------------------------------------