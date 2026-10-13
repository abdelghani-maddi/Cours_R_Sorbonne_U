# Vérification depuis la racine du projet :
# Rscript enseignant/verifier_corriges.R
# Les erreurs ne sont pas ignorées : un seul échec produit un code de sortie non nul.

options(width = 110)
dir.create("resultats/verification", recursive = TRUE, showWarnings = FALSE)
rapport <- "resultats/verification"
packages <- c("dplyr", "tibble", "tidyr", "forcats", "stringr", "labelled", "haven", "gtsummary", "gt", "broom")
absents <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(absents)) stop("Packages manquants : ", paste(absents, collapse = ", "))

series <- list.dirs("tp", recursive = FALSE, full.names = TRUE)
series <- series[grepl("/serie-[0-9]+-", series)]
if (!length(series)) stop("Aucune série de TP trouvée.")
fichiers <- character()
for (serie in series) {
  enonces <- list.files(file.path(serie, "enonces"), pattern = "\\.R$", full.names = TRUE)
  corriges <- list.files(file.path(serie, "corriges"), pattern = "\\.R$", full.names = TRUE)
  if (!length(enonces) || !identical(basename(enonces), basename(corriges))) {
    stop("Énoncés et corrigés non appariés dans ", serie)
  }
  for (i in seq_along(enonces)) {
    questions <- function(f) {
      texte <- readLines(f, encoding = "UTF-8", warn = FALSE)
      q <- grep("^# Q[0-9]{2}\\.", texte, value = TRUE)
      sub("^# (Q[0-9]{2})\\..*$", "\\1", q)
    }
    if (!identical(questions(enonces[i]), questions(corriges[i]))) {
      stop("Numérotation énoncé/corrigé différente : ", basename(enonces[i]))
    }
  }
  fichiers <- c(fichiers, enonces, corriges)
}

resultats <- vector("list", length(fichiers))
for (i in seq_along(fichiers)) {
  fichier <- fichiers[i]
  cat("\n--- ", fichier, " ---\n", sep = "")
  avertissements <- character()
  debut <- Sys.time()
  erreur <- tryCatch({
    parse(file = fichier, encoding = "UTF-8")
    # Un environnement neuf empêche un corrigé de dépendre d'un objet créé
    # par le précédent. Les données sont recréées dans chaque script.
    environnement <- new.env(parent = globalenv())
    withCallingHandlers(
      source(fichier, local = environnement, encoding = "UTF-8", echo = FALSE),
      warning = function(w) {
        avertissements <<- c(avertissements, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    NA_character_
  }, error = function(e) conditionMessage(e))
  resultats[[i]] <- data.frame(
    fichier = fichier, reussi = is.na(erreur), erreur = erreur,
    avertissements = paste(unique(avertissements), collapse = " | "),
    secondes = as.numeric(difftime(Sys.time(), debut, units = "secs"))
  )
  if (!is.na(erreur)) cat("ECHEC : ", erreur, "\n", sep = "")
  if (length(avertissements)) cat("AVERTISSEMENTS : ", paste(unique(avertissements), collapse = " | "), "\n")
}
bilan <- do.call(rbind, resultats)
write.csv(bilan, file.path(rapport, "bilan.csv"), row.names = FALSE, fileEncoding = "UTF-8")
versions <- data.frame(package = packages,
                       version = vapply(packages, function(p) as.character(utils::packageVersion(p)), character(1)))
write.csv(versions, file.path(rapport, "versions.csv"), row.names = FALSE)
writeLines(capture.output(sessionInfo()), file.path(rapport, "sessionInfo.txt"))
print(bilan[, c("fichier", "reussi", "erreur")], row.names = FALSE)
if (any(!bilan$reussi)) stop(sum(!bilan$reussi), " script(s) en échec. Voir resultats/verification/bilan.csv.")
cat("\nTous les scripts ont été exécutés ; les assertions intégrées ont réussi.\n")
