# SERIE 01 - JOUR 09 - CORRIGE
library(dplyr)
library(tibble)
library(forcats)
library(labelled)
library(gtsummary)
theme_gtsummary_language(language = "fr")
dents <- as_tibble(datasets::ToothGrowth) |> mutate(
  supp = factor(supp, levels = c("OJ", "VC"), labels = c("Jus d'orange", "Acide ascorbique")),
  dose_cat = factor(dose, levels = c(0.5, 1, 2))
) |> set_variable_labels(len = "Longueur des odontoblastes (len)",
                         dose_cat = "Dose (mg/jour)", supp = "Mode d'administration")

# Q01. Soixante cobayes, dix par combinaison. Une ligne décrit un animal.
plan <- dents |> count(supp, dose)
print(plan)
stopifnot(nrow(plan) == 6L, all(plan$n == 10L))

# Q02. Lire effectifs, unités, dispersion et distribution des doses avant les tests.
tbl_description <- dents |> tbl_summary(
  by = supp, include = c(len, dose_cat), type = len ~ "continuous",
  statistic = all_continuous() ~ "{mean} ({sd})", digits = all_continuous() ~ 1,
  missing = "no"
) |> add_overall() |> add_n()
print(tbl_description)

# Q03. Le test est spécifié pour chaque variable ; les notes permettent de le contrôler.
tbl_tests <- dents |> tbl_summary(
  by = supp, include = c(len, dose_cat), type = len ~ "continuous",
  statistic = len ~ "{mean} ({sd})", digits = len ~ 1
) |> add_p(test = list(len ~ "t.test", dose_cat ~ "fisher.test"))
print(tbl_tests)
# Les doses ont été fixées par le protocole. Tester leur répartition ne mesure
# pas un effet biologique : ce tableau sert ici à apprendre le choix de test.

# Q04. t.test utilise par défaut le test de Welch, sans imposer l'égalité des variances.
test_global <- t.test(len ~ supp, data = dents)
p_tableau <- tbl_tests$table_body |> filter(variable == "len", row_type == "label") |> pull(p.value)
stopifnot(length(p_tableau) == 1L, near(p_tableau, test_global$p.value))

# Q05. La version asymptotique évite la demande d'un calcul exact en présence d'ex aequo.
tbl_rangs <- dents |> tbl_summary(by = supp, include = len, type = len ~ "continuous") |>
  add_p(test = len ~ "wilcox.test", test.args = all_tests("wilcox.test") ~ list(exact = FALSE))
print(tbl_rangs)
# Wilcoxon compare des rangs/distributions. Son interprétation comme différence
# de médianes exige des hypothèses supplémentaires sur la forme des distributions.

# Q06. À dose fixée, on compare les modes d'administration dans un même niveau de dose.
dose_1 <- dents |> filter(dose == 1)
tbl_dose_1 <- dose_1 |> tbl_summary(by = supp, include = len, type = len ~ "continuous",
  statistic = len ~ "{mean} ({sd})", digits = len ~ 2, missing = "ifany")
stopifnot(nrow(dose_1) == 20L)
# Le résultat global mélange trois doses et ne montre pas les différences propres à chacune.

# Q07. levels impose premier groupe moins deuxième groupe, indépendamment d'une lecture intuitive.
tbl_difference <- tbl_dose_1 |> add_difference(
  test = len ~ "t.test", conf.level = 0.95,
  levels = c("Jus d'orange", "Acide ascorbique")
)
resume_dose_1 <- dose_1 |> summarise(n = n(), moyenne = mean(len), .by = supp)
diff_directe <- with(dose_1, mean(len[supp == "Jus d'orange"]) - mean(len[supp == "Acide ascorbique"]))
estimation_table <- tbl_difference$table_body |>
  filter(variable == "len", row_type == "label") |> pull(estimate)
print(tbl_difference)
print(resume_dose_1)
stopifnot(near(estimation_table, diff_directe))

# Q08. Inverser la soustraction inverse le signe et les bornes, pas la p-value bilatérale.
tbl_inverse <- tbl_dose_1 |> add_difference(test = len ~ "t.test",
  levels = c("Acide ascorbique", "Jus d'orange"))
estimation_inverse <- tbl_inverse$table_body |>
  filter(variable == "len", row_type == "label") |> pull(estimate)
stopifnot(near(estimation_inverse, -diff_directe))

# Q09. L'événement décrit est len >= 20. Le seuil ne constitue pas une norme médicale.
dose_1 <- dose_1 |> mutate(long_20 = as.integer(len >= 20))
tbl_proportion <- dose_1 |> tbl_summary(
  by = supp, include = long_20, type = long_20 ~ "dichotomous", value = long_20 ~ 1,
  label = long_20 ~ "Longueur au moins égale à 20"
) |> add_difference(test = long_20 ~ "prop.test",
  test.args = all_tests("prop.test") ~ list(correct = FALSE),
  levels = c("Jus d'orange", "Acide ascorbique"))
print(tbl_proportion)
# prop.test emploie une approximation ; examiner les effectifs et les messages
# éventuels. Le choix d'un seuil fait perdre de l'information sur len.

# Q10. Une différence de proportions s'exprime en points après multiplication par 100.
proportions <- dose_1 |> summarise(n = n(), p = mean(long_20), .by = supp)
diff_prop <- with(dose_1, mean(long_20[supp == "Jus d'orange"]) - mean(long_20[supp == "Acide ascorbique"]))
print(proportions)
print(c(difference = diff_prop, points = 100 * diff_prop))
# Exemple de lecture : 0.20 = 20 points. Le passage de 0.50 à 0.60 est de
# 10 points, mais de 20 % relativement à 0.50. Ces deux unités ne sont pas interchangeables.

# Q11. L'IC décrit ici chaque moyenne/proportion globale, pas un contraste entre groupes.
tbl_ic <- dose_1 |> tbl_summary(include = c(len, long_20),
  type = list(len ~ "continuous", long_20 ~ "dichotomous"), value = long_20 ~ 1,
  statistic = len ~ "{mean} ({sd})", digits = len ~ 2
) |> add_ci(method = list(all_continuous() ~ "t.test", all_categorical() ~ "wilson"))
print(tbl_ic)

# Q12. À données et méthode identiques, un niveau de confiance supérieur élargit l'intervalle.
ic90 <- t.test(dose_1$len, conf.level = 0.90)$conf.int
ic95 <- t.test(dose_1$len, conf.level = 0.95)$conf.int
print(rbind(IC90 = ic90, IC95 = ic95))
stopifnot(diff(ic95) > diff(ic90))
# Un IC fréquentiste n'attribue pas une probabilité de 95 % au paramètre fixé
# après observation ; le niveau décrit la couverture de la procédure répétée.

# Q13. Quatre variables quantitatives donnent quatre tests, chacun portant sur trois espèces.
fleurs <- as_tibble(datasets::iris)
tbl_iris_tests <- fleurs |> tbl_summary(by = Species,
  statistic = all_continuous() ~ "{median} [{p25} ; {p75}]"
) |> add_p(test = all_continuous() ~ "kruskal.test")
stopifnot(sum(!is.na(tbl_iris_tests$table_body$p.value)) == 4L)

# Q14. Holm contrôle le risque familial d'au moins un faux rejet dans les quatre tests.
# Le nom technique q.value ne signifie pas que la méthode choisie contrôle le FDR.
tbl_iris_ajuste <- tbl_iris_tests |> add_q(method = "holm") |>
  modify_header(q.value = "**p ajusté (Holm)**")
p_ajustes <- tbl_iris_ajuste$table_body |> filter(!is.na(p.value)) |> select(p.value, q.value)
stopifnot(isTRUE(all.equal(p_ajustes$q.value, p.adjust(p_ajustes$p.value, method = "holm"))))
print(tbl_iris_ajuste)

# Q15. Types, arrondis et traitement des manquants identiques rendent les strates comparables.
tbl_par_dose <- dents |> tbl_strata(
  strata = dose,
  .tbl_fun = ~ .x |> tbl_summary(by = supp, include = len, type = len ~ "continuous",
    statistic = len ~ "{mean} ({sd})", digits = len ~ 2, missing = "no") |>
    add_difference(test = len ~ "t.test", levels = c("Jus d'orange", "Acide ascorbique")),
  .header = "**Dose : {strata} mg/jour**"
)
print(tbl_par_dose)

# Q16. Dans chaque dose, 10 animaux sur 20 reçoivent chaque mode d'administration.
tbl_plan <- dents |> tbl_cross(row = dose_cat, col = supp, percent = "row")
print(tbl_plan)
controle_plan <- dents |> count(dose_cat, supp) |> mutate(pct = 100 * n / sum(n), .by = dose_cat)
stopifnot(all(controle_plan$pct == 50))

# Q17. L'effectif du groupe décrit les animaux ; l'effectif disponible pour len
# diminue d'une unité dans le groupe de la première ligne.
dose_1_na <- dose_1 |> mutate(len = replace(len, 1, NA_real_))
tbl_na <- dose_1_na |> tbl_summary(by = supp, include = len, type = len ~ "continuous",
  statistic = len ~ "{mean} ({sd})", missing_text = "Non renseigné"
) |> add_n(statistic = "{N_nonmiss} / {N_obs}") |> add_difference(test = len ~ "t.test")
print(dose_1_na |> summarise(n = n(), n_len = sum(!is.na(len)), .by = supp))
stopifnot(sum(!is.na(dose_1_na$len)) == 19L)

# Q18. La mise en forme ne modifie pas les p-values stockées.
tbl_p_formate <- dents |> tbl_summary(by = supp, include = len, type = len ~ "continuous") |>
  add_p(test = len ~ "t.test", pvalue_fun = label_style_pvalue(digits = 2)) |> bold_p(t = 0.05)
# La p-value dépend notamment de l'effectif et de la variabilité ; sa petitesse
# ne mesure ni la taille de l'effet, ni sa pertinence dans le contexte étudié.

# Q19. Une absence de rejet n'est pas une preuve d'égalité ; examiner taille
# estimée, IC, effectif et précision. add_difference fournit déjà estimation,
# intervalle et souvent p-value : choisir un seul tableau adapté à la question.
tbl_sans_doublon <- tbl_dose_1 |> add_difference(test = len ~ "t.test")

# Q20. Supprimer le niveau inutilisé rend explicites les deux groupes comparés.
plantes <- as_tibble(datasets::PlantGrowth) |>
  filter(group %in% c("ctrl", "trt2")) |> mutate(group = fct_drop(group))
tbl_plantes <- plantes |> tbl_summary(by = group, include = weight, type = weight ~ "continuous",
  statistic = weight ~ "{mean} ({sd})", digits = weight ~ 2
) |> add_difference(test = weight ~ "t.test", levels = c("ctrl", "trt2"))
print(tbl_plantes)
stopifnot(nrow(plantes) == 20L, nlevels(plantes$group) == 2L)

# Q21. ChickWeight comporte plusieurs mesures d'un même poussin. Elles ne
# sont pas indépendantes. Un test simple sur toutes les lignes sous-estimerait
# potentiellement l'incertitude et confondrait aussi les âges de mesure.
# Une comparaison à âge fixé et une analyse des répétitions sont des questions distinctes.

# Q22. Contrôles numériques, indépendants des arrondis affichés.
stopifnot(near(p_tableau, test_global$p.value))
stopifnot(near(estimation_table, diff_directe))
est_prop <- tbl_proportion$table_body |> filter(variable == "long_20", row_type == "label") |> pull(estimate)
stopifnot(near(est_prop, diff_prop))
test_dose_1 <- t.test(len ~ supp, data = dose_1)
cat(sprintf("À 1 mg/jour : différence OJ - VC = %.2f ; IC 95 %% [%.2f ; %.2f] ; p = %.4f.\n",
            diff_directe, test_dose_1$conf.int[1], test_dose_1$conf.int[2], test_dose_1$p.value))
# Interprétation : la comparaison porte sur vingt cobayes recevant 1 mg de
# vitamine C par jour, répartis en deux groupes de dix. La différence est
# calculée dans le sens jus d'orange moins acide ascorbique. Le signe et la
# grandeur estimée doivent être lus avec les deux moyennes, les dispersions
# et l'intervalle de confiance affiché ci-dessus. La p-value du test de Welch
# évalue la compatibilité des données avec une différence moyenne nulle sous
# les hypothèses du test ; elle ne donne pas la probabilité que cette hypothèse
# soit vraie. La restriction à une dose évite de confondre cette comparaison
# avec une moyenne réunissant plusieurs doses. Elle réduit cependant l'effectif.
# Le recodage len >= 20 répond à une autre question et repose sur un seuil de
# travail arbitraire. Les résultats de cette expérience ne constituent pas
# une recommandation thérapeutique pour l'être humain. Le dispositif d'étude,
# les conditions expérimentales et les limites de généralisation doivent
# accompagner toute conclusion sur un effet du mode d'administration.
