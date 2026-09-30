# Environnement commun du cours -----------------------------------------------

required_packages <- c(
  "tidyverse",
  "questionr",
  "labelled",
  "gtsummary",
  "FactoMineR",
  "factoextra",
  "survey",
  "ggeffects",
  "MASS",
  "carData",
  "TraMineR"
)

missing_packages <- setdiff(required_packages, rownames(installed.packages()))

if (length(missing_packages) > 0) {
  message(
    "Packages manquants : ",
    paste(missing_packages, collapse = ", "),
    "\nInstallez-les avec install.packages(missing_packages)."
  )
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(gtsummary)
})

gtsummary::theme_gtsummary_language(
  language = "fr",
  decimal.mark = ",",
  big.mark = " "
)

options(
  dplyr.summarise.inform = FALSE,
  scipen = 999
)

# Petite fonction de contrôle utilisée dans plusieurs corrigés.
controle_modalites <- function(data, variable) {
  data |>
    dplyr::count({{ variable }}, .drop = FALSE) |>
    dplyr::mutate(
      pct = 100 * n / sum(n),
      pct = round(pct, 1)
    )
}
