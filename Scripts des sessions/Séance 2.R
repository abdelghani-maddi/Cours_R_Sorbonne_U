# Analyse descriptive du fichier trial ----

library(tidyverse)
library(labelled)
library(questionr)
library(gtsummary)
library(GGally)

theme_gtsummary_language("fr", decimal.mark = ",", big.mark = " ")
 
# Import des données -----

trial <- readxl::read_excel("donnees/trial.xlsx")

var_label(trial$age) <- "Âge en années"
var_label(trial$trt) <- "Traitement" 

save(trial, file = "donnees/trial.RData")

# Statistique univariée -----

# une variable quantitative

mean(trial$age, na.rm = TRUE)
median(trial$age, na.rm = TRUE)
quantile(trial$age, na.rm = TRUE, probs = c(.1, .23, .58))
min(trial$age, na.rm = TRUE)
max(trial$age, na.rm = TRUE)
range(trial$age, na.rm = TRUE)
var(trial$age, na.rm = TRUE)
sd(trial$age, na.rm = TRUE)
summary(trial$age)

# une variable catégorielle

table(trial$stage)
xtabs(~ stage, data = trial)
freq(trial$stage, cum = TRUE, total = TRUE)

# gtsummary

tbl_summary(trial)

# Graphiques bivariés -------

ggplot(trial) +
  aes(x = stage, y = age, fill = trt) +
  geom_boxplot() +
  scale_fill_hue() +
  labs(title = "Titre de mon graphique", subtitle = "sous-titre", caption = "enquête 2020") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Tableaux croisés ------

theme_gtsummary_mean_sd()

# %>%
# x %>% f(y)    équivalent à  f(x, y)

trial %>%
  tbl_summary(
    by = "trt",
    percent = "row",
    digits = list(
      all_categorical() ~ c(0, 1),
      all_continuous() ~ c(1, 1)
    )
  ) %>%
  add_overall(last = TRUE) %>%
  add_p()


# fusion de deux tableaux 

t1 <- trial %>%
  tbl_summary(include = c("trt", "age", "grade"), by = "trt")
t2 <- trial %>%
  tbl_summary(include = c("stage", "age", "grade"), by = "stage")
tbl_merge(list(t1, t2))

# ggbivariate()

ggbivariate(trial, outcome = "trt", explanatory = c("age", "stage", "grade"))
ggtable(
  trial, 
  columnsX = c("trt"), 
  columnsY = c("grade", "stage"),
  cells = "row.prop",
  fill = "std.resid"
)







