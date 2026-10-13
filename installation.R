# Analyse des données avec R - Sorbonne Université
# Exécuter ce fichier lors de la préparation du poste de travail.
# Les exercices chargent ensuite les bibliothèques dont ils ont besoin.

if (getRversion() < "4.2.0") stop("Installer R 4.2 ou une version plus récente.")
versions_min <- c(dplyr = "1.1.4", tibble = "3.2.1", tidyr = "1.3.0",
                  forcats = "1.0.0", stringr = "1.5.0", labelled = "2.13.0",
                  haven = "2.5.0", gtsummary = "2.6.1", gt = "0.11.0", broom = "1.0.0")
a_installer <- names(versions_min)[vapply(names(versions_min), function(p) {
  !requireNamespace(p, quietly = TRUE) || utils::packageVersion(p) < versions_min[[p]]
}, logical(1))]
if (length(a_installer)) {
  install.packages(a_installer, repos = "https://cloud.r-project.org")
}
indisponibles <- names(versions_min)[vapply(names(versions_min), function(p) {
  !requireNamespace(p, quietly = TRUE) || utils::packageVersion(p) < versions_min[[p]]
}, logical(1))]
if (length(indisponibles)) stop("Installation à vérifier : ", paste(indisponibles, collapse = ", "),
                               ". Redémarrer R après une mise à jour.")
for (dossier in c("travaux", "resultats")) dir.create(dossier, showWarnings = FALSE)
print(data.frame(package = names(versions_min), version = vapply(names(versions_min),
  function(p) as.character(utils::packageVersion(p)), character(1))), row.names = FALSE)
