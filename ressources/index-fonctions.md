# Répertoire des fonctions — Série 01

Les numéros renvoient aux questions des énoncés et des corrigés. Pour consulter la syntaxe dans R : `?nom_de_fonction`, ou `help("nom_de_fonction", package = "nom_du_package")`.

## Lire et préparer les colonnes

| Fonctions et arguments | Où les travailler |
| --- | --- |
| dim(), nrow(), ncol(), names(), head(), class(), glimpse() | [Jour 01, Q01–Q03](../tp/serie-01-preparation-description/enonces/01-colonnes.R) |
| select(), starts_with(), contains(), ends_with(), matches() | Jour 01, Q04–Q06 |
| where(), all_of(), any_of(), :, last_col(), everything() | Jour 01, Q07–Q10 |
| rename(), rename_with(), relocate(), .before, .after | Jour 01, Q11–Q13 |
| pull(), vecteur nommé, distinct(), n_distinct(), count() | Jour 01, Q14–Q16 |
| tibble(), as_tibble(), rownames_to_column() | Jour 01, données de départ et Q19–Q20 |

## Sélectionner les observations

| Fonctions et arguments | Où les travailler |
| --- | --- |
| filter(), &, \|, %in%, !, between() | [Jour 02, Q02–Q06](../tp/serie-01-preparation-description/enonces/02-lignes.R) |
| is.na(), near(), conditions avec des NA | Jour 02, Q07–Q10 |
| arrange(), desc(), slice(), slice_head(), slice_tail() | Jour 02, Q11–Q14 |
| slice_max(), slice_min(), with_ties, na_rm | Jour 02, Q12–Q13 |
| slice_sample(), set.seed() | Jour 02, Q15 |
| distinct(.keep_all = TRUE), contrôle d'une clé dupliquée | Jour 02, Q16–Q17 |

## Créer des variables et des catégories

| Fonctions et arguments | Où les travailler |
| --- | --- |
| mutate(), .keep, .before, round() | [Jour 03, Q01–Q03](../tp/serie-01-preparation-description/enonces/03-recodages-facteurs.R) |
| if_else(), missing, case_when() | Jour 03, Q04–Q09 et Q22 |
| cut(), breaks, right = FALSE, factor(), levels() | Jour 03, Q06–Q11 et Q20 |
| fct_relevel(), fct_recode(), fct_collapse() | Jour 03, Q12–Q13 |
| fct_infreq(), fct_reorder(), fct_drop(), fct_lump_n() | Jour 03, Q14–Q16 |
| str_squish(), str_to_lower(), str_detect(), str_extract(), str_replace_all() | Jour 03, Q17–Q19 |

## Traiter les manquants et plusieurs colonnes

| Fonctions et arguments | Où les travailler |
| --- | --- |
| summarise(across()), is.na(), na.rm | [Jour 04, Q01–Q02 et Q06–Q07](../tp/serie-01-preparation-description/enonces/04-manquants-controles.R) |
| na_if(), remplacement ciblé avec if_else() | Jour 04, Q05 |
| if_any(), if_all(), drop_na() | Jour 04, Q08–Q09 |
| pick(), rowSums(), across() avec liste de fonctions et .names | Jour 04, Q10–Q12 |
| rowwise(), c_across(), ungroup() | Jour 04, Q13 |
| coalesce(), replace_na(), fill() | Jour 04, Q14–Q16 |

## Résumer, compter et ordonner les calculs

| Fonctions et arguments | Où les travailler |
| --- | --- |
| count(), tally(), group_by(), summarise(), .groups, .by | [Jour 05, Q01–Q06](../tp/serie-01-preparation-description/enonces/05-groupes-resumes.R) |
| add_count(), add_tally(), group_vars(), .add, ungroup() | Jour 05, Q07–Q08 |
| count(wt = Freq), sum(Freq) et effectifs agrégés | Jour 05, Q09–Q12 |
| reframe(), quantile(type = 2), centrage dans mutate(.by = ...) | Jour 05, Q13–Q14 |
| min_rank(), dense_rank(), row_number(), ntile() | Jour 05, Q15–Q16 |
| lag(), lead(), cummean(), cumsum() | Jour 05, Q17–Q19 |
| first(), last(), nth(), n_distinct() | Jour 05, Q20–Q21 |

## Relier les tables et changer de format

| Fonctions et arguments | Où les travailler |
| --- | --- |
| left_join(), inner_join(), right_join(), full_join() | [Jour 06, Q03–Q06](../tp/serie-01-preparation-description/enonces/06-jointures-formats.R) |
| anti_join(), semi_join(), join_by(), noms de clés différents | Jour 06, Q05–Q07 |
| relationship, suffix, contrôle des cardinalités | Jour 06, Q08–Q10 |
| bind_cols(), bind_rows(), .id | Jour 06, Q11–Q12 |
| intersect(), union(), setdiff(), setequal() | Jour 06, Q13 |
| rows_update(), rows_patch() | Jour 06, Q14 |
| pivot_longer(), names_to, names_sep, values_to | Jour 06, Q15–Q16 |
| pivot_wider(), id_cols, names_from, values_from | Jour 06, Q17–Q18 et Q20 |
| complete() et combinaisons absentes | Jour 06, Q19 |

## Documenter les variables et leurs codes

| Fonctions et arguments | Où les travailler |
| --- | --- |
| var_label(), set_variable_labels() | [Jour 07, Q01–Q04](../tp/serie-01-preparation-description/enonces/07-labels.R) |
| val_labels(), set_value_labels(), labelled(), val_label() | Jour 07, Q05–Q08 |
| add_value_labels(), remove_val_labels() | Jour 07, Q09 et Q12 |
| to_factor(), levels, sort_levels, strict, nolabel_to_na | Jour 07, Q10–Q14 |
| haven::labelled_spss(), na_values(), haven::zap_missing() | Jour 07, Q15–Q17 |
| look_for(), conservation et mise à jour des libellés | Jour 07, Q18 |
| saveRDS(), readRDS(), write.csv(), read.csv() | Jour 07, Q19–Q20 |

## Décrire avec gtsummary

| Fonctions et arguments | Où les travailler |
| --- | --- |
| tbl_summary(), include, type, label | [Jour 08, Q01–Q04](../tp/serie-01-preparation-description/enonces/08-tbl-summary.R) |
| statistic, digits, all_continuous(), all_categorical(), continuous2 | Jour 08, Q05–Q09 |
| missing, missing_text, missing_stat | Jour 08, Q10–Q11 |
| by, value, dichotomous, categorical, percent | Jour 08, Q12–Q15 |
| sort, add_overall(), add_n(), add_stat_label() | Jour 08, Q16–Q19 |
| theme_gtsummary_language(), as_tibble() | Jour 08, Q19–Q21 |

## Comparer et présenter les résultats

| Fonctions et arguments | Où les travailler |
| --- | --- |
| add_p(), test, test.args, all_tests(), t.test(), wilcox.test() | [Jour 09, Q03–Q05](../tp/serie-01-preparation-description/enonces/09-comparaisons.R) |
| add_difference(), levels, conf.level, prop.test | Jour 09, Q07–Q10 |
| add_ci(), method, IC de moyenne et de proportion | Jour 09, Q11–Q12 |
| add_q(), p.adjust(), méthode de Holm | Jour 09, Q13–Q14 |
| tbl_strata(), tbl_cross() | Jour 09, Q15–Q16 |
| label_style_pvalue(), bold_p() | Jour 09, Q18 |
| show_header_names(), modify_header(), all_stat_cols() | [Jour 10, Q02](../tp/serie-01-preparation-description/enonces/10-restitution-synthese.R) |
| modify_spanning_header(), modify_caption() | Jour 10, Q03 |
| modify_footnote_header(), modify_footnote_body() | Jour 10, Q04–Q05 |
| bold_labels(), italicize_levels(), modify_column_hide(), modify_column_unhide() | Jour 10, Q06–Q07 |
| add_stat(), fonction personnalisée, modify_fmt_fun(), label_style_number() | Jour 10, Q08–Q09 |
| tbl_merge(), tbl_stack() | Jour 10, Q10–Q11 |
| as_gt(), gt::gtsave(), write.csv2(), saveRDS(), sessionInfo() | Jour 10, Q12–Q14 et Q25 |

## Contrôler le résultat

`stopifnot()`, `identical()`, `all.equal()`, `near()`, `file.exists()` et `tryCatch()` sont employés tout au long des corrigés. Un contrôle doit porter sur une propriété attendue : une clé unique, un effectif, la conservation des valeurs, une correspondance de codage ou l'égalité de deux calculs indépendants.

Les fonctions de présentation ne remplacent pas ces contrôles. Pour vérifier une cellule de tableau, recalculer la statistique sur les observations concernées, avant arrondi.

## Documentation

[Référence dplyr](https://dplyr.tidyverse.org/reference/index.html) · [tidyr](https://tidyr.tidyverse.org/reference/index.html) · [forcats](https://forcats.tidyverse.org/reference/index.html) · [stringr](https://stringr.tidyverse.org/reference/index.html) · [labelled](https://larmarange.github.io/labelled/reference/index.html) · [gtsummary](https://www.danieldsjoberg.com/gtsummary/reference/index.html)

[Accueil du cours](../README.md)
