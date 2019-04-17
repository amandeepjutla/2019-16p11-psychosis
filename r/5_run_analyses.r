# run_analyses.r

# This will run all analyses reported in the paper and send the results in
# csv format to a subdirectory called /output.

# Table 1 ------------------------------------------------------------

# summary statistics

tibble(
  group = c("total", "dup", "del", "noncarrier"),
  age_mean = 
    subjects$raw %>% 
    map(pull, age_months) %>% 
    map(mean) %>% 
    map(function(x) x / 12),
  age_sd = 
    subjects$raw %>% 
    map(pull, age_months) %>%
    map(sd) %>%
    map(function(x) x / 12),
  iq_mean = 
    subjects$raw %>% 
    map(pull, diagnosis_summary.best_full_scale_iq) %>%
    map(mean),
  iq_sd = 
    subjects$raw %>% 
    map(pull, diagnosis_summary.best_full_scale_iq) %>%
    map(sd),
  female_n = 
    subjects$raw %>% 
    map(filter, gender == 1) %>% 
    map(nrow),
  female_percent = 
    subjects$raw %>% 
    map(function(x) nrow(filter(x, gender == 1)) / nrow(x) * 100),
  asd_n = 
    subjects$raw %>% 
    map(filter, clinical_asd == 1) %>% 
    map(nrow),
  asd_percent = 
    subjects$raw %>% 
    map(function(x) nrow(filter(x, clinical_asd == 1)) / nrow(x) * 100),
  ocd_n = 
    subjects$raw %>% 
    map(filter, ocd == 1) %>% 
    map(nrow),
  ocd_percent = 
    subjects$raw %>% 
    map(function(x) nrow(filter(x, ocd == 1)) / nrow(x) * 100)
) %>% 
  unnest() %>% 
  mutate_if(is.numeric, round, digits = 2) %>%
  write_excel_csv("./output/table1/summary_stats.csv")

# group comparisons

anova <- list()
anova$iq <- aov(diagnosis_summary.best_full_scale_iq ~ sample_group, 
                data = subjects$raw$total) 
anova$age <- aov(age_months ~ sample_group, 
                 data = subjects$raw$total) 

chi <- list()
chi$gender <- chisq.test(subjects$raw$total$gender,
                         subjects$raw$total$sample_group) 
chi$asd <- chisq.test(subjects$raw$total$clinical_asd,
                      subjects$raw$total$sample_group) 
chi$ocd <- chisq.test(subjects$raw$total$ocd,
                      subjects$raw$total$sample_group) 

anova_posthoc <- list()
anova_posthoc$iq <- TukeyHSD(anova$iq)
anova_posthoc$age <- TukeyHSD(anova$age) 

chi_posthoc <- list()
chi_posthoc$gender <- chisq.post.hoc(
  xtabs(~ gender + sample_group, data = subjects$raw$total), 
  test = "chisq.test", 
  popsInRows = FALSE, 
  control = "bonferroni") 
chi_posthoc$asd <- chisq.post.hoc(
  xtabs(~ clinical_asd + sample_group, data = subjects$raw$total), 
  test = "chisq.test", 
  popsInRows = FALSE, 
  control = "bonferroni") 
chi_posthoc$ocd <- chisq.post.hoc(
  xtabs(~ ocd + sample_group, data = subjects$raw$total), 
  test = "chisq.test", 
  popsInRows = FALSE, 
  control = "bonferroni")

tibble(
  variable = c("iq","age"),
  test = anova %>% map(tidy) 
) %>% 
  unnest() %>%
  mutate_at(vars(-variable, -term, -p.value), 
            round, digits = 2) %>%
  write_excel_csv("./output/table1/anova.csv")

tibble(
  variable = c("gender","asd","ocd"),
  test = chi %>% map(tidy)
) %>% unnest() %>%
  mutate_at(vars(-variable, -method, -p.value), 
            round, digits = 2) %>%  
  write_excel_csv("./output/table1/chi.csv")

tibble(
  variable = c("iq","age"),
  test = anova_posthoc %>% map(tidy) 
) %>% 
  unnest() %>%
  mutate_at(vars(-variable, -term, -comparison, -adj.p.value), 
            round, digits = 2) %>%  
  write_excel_csv("./output/table1/anova_posthoc.csv")

tibble(
  variable = c("gender","asd","ocd"),
  test = chi_posthoc %>% map(as.tibble)
) %>% unnest() %>%
  write_excel_csv("./output/table1/chi_posthoc.csv")

# Table 2 ------------------------------------------------------------

tibble(
  group = c("total", "dup", "del", "noncarrier"),
  ados = 
    subjects$raw %>% 
    map(function (x) filter(x, !is.na(ados_css_total_combined))) %>% 
    map(function (x) nrow(x)),
  adir =    
    subjects$raw %>%
    map(function (x) filter(x, !is.na(adi_r.c_total))) %>% 
    map(function (x) nrow(x)),
  bapq = 
    subjects$raw %>%
    map(function (x) filter(x, !is.na(bapq.total))) %>%
    map(function (x) nrow(x)),
  scq = 
    subjects$raw %>%
    map(function (x) filter(x, !is.na(scq_life.summary_score))) %>%
    map(function (x) nrow(x)),
  srs_child =
    subjects$raw %>%
    map(function (x) filter(x, !is.na(srs_parent.total))) %>%
    map(function (x) nrow(x)),
  srs_adult = 
    subjects$raw %>%
    map(function (x) filter(x, !is.na(srs_adult.total))) %>%
    map(function (x) nrow(x)),
  mullen = 
    subjects$raw %>%
    map(function (x) filter(x, !is.na(mullen.measure.measure_type))) %>% 
    map(function (x) nrow(x)),
  das2_early_lower = 
    subjects$raw %>%
    map(function (x) filter(
      x, !is.na(das_ii_early_years.gca_lower_standard))) %>% 
    map(function (x) nrow(x)),
  das2_early_upper = 
    subjects$raw %>%
    map(function (x) filter(
      x, !is.na(das_ii_early_years.gca_upper_standard))) %>% 
    map(function (x) nrow(x)),
  das2_school = 
    subjects$raw %>%
    map(function (x) filter(
      x,!is.na(das_ii_school_age.measure.measure_type))) %>% 
    map(function (x) nrow(x)),
  wasi = 
    subjects$raw %>%
    map(function (x) filter(x, !is.na(wasi.measure.measure_type))) %>% 
    map(function (x) nrow(x)),
  cbcl = 
    subjects$raw %>%
    map(function (x) filter(x, !is.na(cbcl_6_18.thought_problems_t_score))) %>% 
    map(function (x) nrow(x)),
  abcl = 
    subjects$raw %>%
    map(function (x) filter(x, !is.na(abcl_18_59.thought_t_score))) %>% 
    map(function (x) nrow(x)),
  scl = 
    subjects$raw %>%
    map(function (x) filter(x, scl_90_r.measure.eval_age_months>0)) %>% 
    map(function (x) nrow(x)),
  disc =
    subjects$raw %>%
    map(function (x) filter(x, !is.na(disc_youth.measure.measure_type))) %>% 
    map(function (x) nrow(x)),
  sops =
    subjects$raw %>%
    map(function (x) filter(x, !is.na(eval_age_months))) %>% 
    map(function (x) nrow(x))
) %>% 
  unnest() %>%
  write_excel_csv("./output/table2/table2.csv")

# Table 3 -------------------------------------------------------------

tibble(
  group = c("total","dup","del","noncarrier"),
  bcl_received =
    subjects$raw %>% 
    map(filter, bcl_data == 1) %>%
    map(nrow),
  bcl_positive =
    subjects$raw %>%
    map(filter, bcl_missingness == 1) %>%
    map(nrow),
  bcl_percent = 
    subjects$raw %>%
    map(function (x) 
      nrow(filter(x, bcl_missingness == 1)) / 
      nrow(filter(x, bcl_data == 1)) * 100 ),
  scl_received =
    subjects$raw %>% 
    map(filter, scl_data == 1) %>%
    map(nrow),
  scl_positive =
    subjects$raw %>%
    map(filter, scl_missingness_binary == 1) %>%
    map(nrow),
  scl_percent = 
    subjects$raw %>%
    map(function (x) 
      nrow(filter(x, scl_missingness_binary == 1)) / 
        nrow(filter(x, scl_data == 1)) * 100 ),
  disc_received =
    subjects$raw %>% 
    map(filter, disc_data == 1) %>%
    map(nrow),
  disc_positive =
    subjects$raw %>%
    map(filter, disc_missingness == 1) %>%
    map(nrow),
  disc_percent = 
    subjects$raw %>%
    map(function (x) 
      nrow(filter(x, disc_missingness == 1)) / 
        nrow(filter(x, disc_data == 1)) * 100 ),
  sops_received =
    subjects$raw %>% 
    map(filter, sops_data == 1) %>%
    map(nrow),
  sops_positive =
    subjects$raw %>%
    map(filter, sops_missingness == 1) %>%
    map(nrow),
  sops_percent = 
    subjects$raw %>%
    map(function (x) 
      nrow(filter(x, sops_missingness == 1)) / 
        nrow(filter(x, disc_data == 1)) * 100 )
) %>% 
  unnest() %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  write_excel_csv("./output/table3/table3.csv")

# Table 4 -------------------------------------------------------------

# number of participants with each combination

tibble(
  bcl_scl_data = 
    subjects$raw$total %>%
    filter(bcl_data == 1 & scl_data == 1) %>% 
    nrow(),
  bcl_scl_positive =
    subjects$raw$total %>%
    filter(bcl_missingness == 1 & scl_missingness_binary == 1) %>% 
    nrow(),

  bcl_disc_data = 
    subjects$raw$total %>%
    filter(bcl_data == 1 & disc_data == 1) %>% 
    nrow(),
  bcl_disc_positive =
    subjects$raw$total %>%
    filter(bcl_missingness == 1 & disc_missingness == 1) %>% 
    nrow(),
  
  bcl_sops_data = 
    subjects$raw$total %>%
    filter(bcl_data == 1 & sops_data == 1) %>% 
    nrow(),
  bcl_sops_positive =
    subjects$raw$total %>%
    filter(bcl_missingness == 1 & sops_missingness == 1) %>% 
    nrow(),
  
  scl_sops_data = 
    subjects$raw$total %>%
    filter(scl_data == 1 & sops_data == 1) %>% 
    nrow(),
  scl_sops_positive =
    subjects$raw$total %>%
    filter(scl_missingness_binary == 1 & sops_missingness == 1) %>% 
    nrow(),

  disc_sops_data = 
    subjects$raw$total %>%
    filter(disc_data == 1 & sops_data == 1) %>% 
    nrow(),
  disc_sops_positive =
    subjects$raw$total %>%
    filter(disc_missingness == 1 & sops_missingness == 1) %>% 
    nrow()
) %>% 
  write_excel_csv("./output/table4/pairwise_combinations.csv")

crosstabs <- list()
crosstabs$bcl_scl <- xtabs(
  ~ bcl_missingness + scl_missingness_binary, data = subjects$raw$total)
crosstabs$bcl_disc <- xtabs(
  ~ bcl_missingness + disc_missingness, data = subjects$raw$total)
crosstabs$bcl_sops <- xtabs(
  ~ bcl_missingness + sops_missingness, data = subjects$raw$total)
crosstabs$scl_sops <- xtabs(
  ~ scl_missingness_binary + sops_missingness, data = subjects$raw$total)
crosstabs$disc_sops <- xtabs(
  ~ disc_missingness + sops_missingness, data = subjects$raw$total)

tibble(
  pair = c("BCL x SCL","BCL x DISC","BCL x SOPS",
           "SCL x SOPS","Disc x SOPS"),
  fisher = crosstabs %>%
    map(fisher.test) %>%
    map(tidy)
) %>% 
  unnest() %>% 
  mutate_at(vars(-pair, -p.value, -method, -alternative), 
            round, digits = 2) %>%
  write_excel_csv("./output/table4/relationship_strengths.csv")

index_measures <- subjects$raw$total %>%
  select(scl_missingness_binary, 
         bcl_missingness, 
         sops_missingness,
         disc_missingness)

Hmisc::rcorr(as.matrix(index_measures), type=c("spearman"))

# Correlation matrix

subjects$analysis$all <- subjects$analysis$all %>% mutate(
  duplication_standardized = arm::rescale(
    duplication, binary.inputs = "center"))

subjects$analysis$all <- subjects$analysis$all %>% mutate(
  deletion_standardized = arm::rescale(
    deletion, binary.inputs = "center"))

subjects$analysis$all <- subjects$analysis$all %>% mutate(
  asd_standardized = arm::rescale(
    clinical_asd, binary.inputs = "center"))

subjects$analysis$all <- subjects$analysis$all %>% mutate(
  ocd_standardized = arm::rescale(
    ocd, binary.inputs = "center"))

subjects$analysis$all <- subjects$analysis$all %>% mutate(
  gender_standardized = arm::rescale(
    gender, binary.inputs = "center"))

subjects$analysis$all <- subjects$analysis$all %>% mutate(
  age_standardized = arm::rescale(age_months))

subjects$analysis$all <- subjects$analysis$all %>% mutate(
  iq_standardized = arm::rescale(diagnosis_summary.best_full_scale_iq))

matrix <- subjects$analysis$all %>% select(
  duplication_standardized, 
  deletion_standardized,
  age_standardized,
  iq_standardized,
  asd_standardized,
  ocd_standardized,
  gender_standardized
)

correlations <- Hmisc::rcorr(as.matrix(matrix),type=c("pearson")) 

correlations_r_rounded <- correlations$r %>% round(digits=3)

correlations_p_rounded <- correlations$P %>% round(digits=3)

correlations_r_rounded %>% 
  as.data.frame() %>%
  write_excel_csv("./output/correlation_matrix/cor_r.csv")

correlations_p_rounded %>% 
  as.data.frame() %>%
  write_excel_csv("./output/correlation_matrix/cor_p.csv")

# Table 5 ----------------------------------------

models <- list()

models$base$all <- 
  geeglm(binary_psychosis ~ 
           duplication +
           deletion +
           age_years +
           diagnosis_summary.best_full_scale_iq + 
           clinical_asd + 
           ocd + 
           gender, 
         data = subjects$analysis$all,
         family = binomial(link="logit"), 
         na.action = na.exclude,
         id = family)

models$base$all %>%
  tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  mutate_at(vars(-term), round, digits = 3) %>%
  write_excel_csv("./output/table5/table5_or.csv")

models$base$all %>%
  tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  write_excel_csv("./output/table5/table5_unrounded.csv")

models$base$all %>%
  tidy(conf.int = TRUE, exponentiate = FALSE) %>%
  mutate_at(vars(-term), round, digits = 3) %>%
  write_excel_csv("./output/table5/table5_b.csv")

# Table 6 -------------------------------------------------------------


models$base$duplication_only <- 
  geeglm(binary_psychosis ~ 
           age_years + 
           diagnosis_summary.best_full_scale_iq + 
           clinical_asd + 
           ocd + 
           gender, 
         data = subjects$analysis$duplication, 
         family = binomial(link="logit"), 
         na.action = na.exclude, 
         id = family)

models$base$duplication_only %>%
  tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  mutate_at(vars(-term), round, digits = 3) %>%
  write_excel_csv("./output/table6/dup_or.csv")

models$base$duplication_only %>%
  tidy(conf.int = TRUE) %>%
  mutate_at(vars(-term), round, digits = 3) %>%
  write_excel_csv("./output/table6/dup_b.csv")

models$base$duplication_only %>%
  tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  write_excel_csv("./output/table6/dup_unrounded.csv")

models$base$deletion_only <- 
  geeglm(binary_psychosis ~ 
           age_years + 
           diagnosis_summary.best_full_scale_iq + 
           clinical_asd + 
           ocd + 
           gender, 
         data = subjects$analysis$deletion, 
         family = binomial(link="logit"), 
         na.action = na.exclude, 
         id = family)

models$base$deletion_only %>%
  tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  mutate_at(vars(-term), round, digits = 3) %>%
  write_excel_csv("./output/table6/del_or.csv")

models$base$deletion_only %>%
  tidy(conf.int = TRUE) %>%
  mutate_at(vars(-term), round, digits = 3) %>%
  write_excel_csv("./output/table6/del_b.csv")

models$base$deletion_only %>%
  tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  write_excel_csv("./output/table6/del_unrounded.csv")

models$base$noncarriers <- 
  geeglm(binary_psychosis ~ 
           age_years + 
           diagnosis_summary.best_full_scale_iq + 
           clinical_asd + 
           ocd + 
           gender, 
         data = subjects$analysis$noncarrier, 
         family = binomial(link="logit"), 
         na.action = na.exclude, 
         id = family)

models$base$noncarriers %>%
  tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  mutate_at(vars(-term), round, digits = 3) %>%
  write_excel_csv("./output/table6/nc_or.csv")

models$base$noncarriers %>%
  tidy(conf.int = TRUE) %>%
  mutate_at(vars(-term), round, digits = 3) %>%
  write_excel_csv("./output/table6/nc_b.csv")

models$base$noncarriers %>%
  tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  write_excel_csv("./output/table6/nc_unrounded.csv")

# Table S2 ---------------------------------------------------------

exploratory_all <- filter(subjects$analysis$all, !is.na(ados_css_total_combined))

models$exploratory$total_css <- 
  geeglm(binary_psychosis ~ 
           duplication +
           deletion +
           age_years +
           diagnosis_summary.best_full_scale_iq +
           ados_css_total_combined + 
           ocd + 
           gender, 
         data = exploratory_all,
         family = binomial(link="logit"), 
         na.action = na.exclude,
         id = family)

models$exploratory$total_css %>%
  tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  mutate_at(vars(-term), round, digits = 3) %>%
  write_excel_csv("./output/table_s2/total_css_all_or.csv")

models$exploratory$total_css %>%
  tidy(conf.int = TRUE) %>%
  mutate_at(vars(-term), round, digits = 3) %>%
  write_excel_csv("./output/table_s2/total_css_all_b.csv")

models$exploratory$total_css %>%
  tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  write_excel_csv("./output/table_s2/total_css_all_unrounded.csv")

exploratory_duplication <- filter(subjects$analysis$duplication, !is.na(ados_css_total_combined))

models$exploratory$total_css_duplication <- 
  geeglm(binary_psychosis ~ 
           age_years +
           diagnosis_summary.best_full_scale_iq + 
           ados_css_total_combined + 
           ocd + 
           gender, 
         data = exploratory_duplication,
         family = binomial(link="logit"), 
         na.action = na.exclude,
         id = family)

models$exploratory$total_css_duplication %>%
  tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  mutate_at(vars(-term), round, digits = 3) %>%
  write_excel_csv("./output/table_s2/total_css_dup_or.csv")

models$exploratory$total_css_duplication %>%
  tidy(conf.int = TRUE) %>%
  mutate_at(vars(-term), round, digits = 3) %>%
  write_excel_csv("./output/table_s2/total_css_dup_b.csv")

models$exploratory$total_css_duplication %>%
  tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  write_excel_csv("./output/table_s2/total_css_dup_unrounded.csv")

exploratory_deletion <- filter(subjects$analysis$deletion, !is.na(ados_css_total_combined))

models$exploratory$total_css_deletion <- 
  geeglm(binary_psychosis ~ 
           age_years +
           ados_css_total_combined + 
           diagnosis_summary.best_full_scale_iq + 
           ocd + 
           gender, 
         data = exploratory_deletion,
         family = binomial(link="logit"), 
         na.action = na.exclude,
         id = family)

models$exploratory$total_css_deletion %>%
  tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  mutate_at(vars(-term), round, digits = 3) %>%
  write_excel_csv("./output/table_s2/total_css_del_or.csv")

models$exploratory$total_css_deletion %>%
  tidy(conf.int = TRUE) %>%
  mutate_at(vars(-term), round, digits = 3) %>%
  write_excel_csv("./output/table_s2/total_css_del_b.csv")

models$exploratory$total_css_deletion %>%
  tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  write_excel_csv("./output/table_s2/total_css_del_unrounded.csv")

exploratory_noncarrier <- filter(subjects$analysis$noncarrier, !is.na(ados_css_total_combined))

models$exploratory$total_css_noncarrier <- 
  geeglm(binary_psychosis ~ 
           age_years +
           diagnosis_summary.best_full_scale_iq + 
           ados_css_total_combined + 
           ocd + 
           gender, 
         data = exploratory_noncarrier,
         family = binomial(link="logit"), 
         na.action = na.exclude,
         id = family)

models$exploratory$total_css_noncarrier %>%
  tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  mutate_at(vars(-term), round, digits = 3) %>%
  write_excel_csv("./output/table_s2/total_css_nc_or.csv")

models$exploratory$total_css_noncarrier %>%
  tidy(conf.int = TRUE) %>%
  mutate_at(vars(-term), round, digits = 3) %>%
  write_excel_csv("./output/table_s2/total_css_nc_b.csv")

models$exploratory$total_css_noncarrier %>%
  tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  write_excel_csv("./output/table_s2/total_css_nc_unrounded.csv")

# Table S3 ---------------------------------------------------------------

exploratory_all_domain <- filter(subjects$analysis$all, !is.na(ados_css_rrb_derived) & !is.na(ados_css_sa_derived))

models$exploratory$domain_css <- 
  geeglm(binary_psychosis ~ 
           duplication +
           deletion +
           age_years +
           diagnosis_summary.best_full_scale_iq +
           ados_css_rrb_derived +
           ados_css_sa_derived +
           ocd + 
           gender, 
         data = exploratory_all_domain,
         family = binomial(link="logit"), 
         na.action = na.exclude,
         id = family)

models$exploratory$domain_css %>%
  tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  mutate_at(vars(-term), round, digits = 3) %>%
  write_excel_csv("./output/table_s3/domain_css_all_or.csv")

models$exploratory$domain_css %>%
  tidy(conf.int = TRUE) %>%
  mutate_at(vars(-term), round, digits = 3) %>%
  write_excel_csv("./output/table_s3/domain_css_all_b.csv")

models$exploratory$domain_css %>%
  tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  write_excel_csv("./output/table_s3/domain_css_all_unrounded.csv")

exploratory_duplication_domain <- filter(
  subjects$analysis$duplication, 
  !is.na(ados_css_rrb_derived) & !is.na(ados_css_sa_derived))

models$exploratory$domain_css_duplication <- 
  geeglm(binary_psychosis ~ 
           age_years +
           diagnosis_summary.best_full_scale_iq +
           ados_css_rrb_derived +
           ados_css_sa_derived +
           ocd + 
           gender, 
         data = exploratory_duplication_domain,
         family = binomial(link="logit"), 
         na.action = na.exclude,
         id = family)

models$exploratory$domain_css_duplication %>%
  tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  mutate_at(vars(-term), round, digits = 3) %>%
  write_excel_csv("./output/table_s3/domain_css_dup_or.csv")

models$exploratory$domain_css_duplication %>%
  tidy(conf.int = TRUE) %>%
  mutate_at(vars(-term), round, digits = 3) %>%
  write_excel_csv("./output/table_s3/domain_css_dup_b.csv")

models$exploratory$domain_css_duplication %>%
  tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  write_excel_csv("./output/table_s3/domain_css_dup_unrounded.csv")

exploratory_deletion_domain <- filter(subjects$analysis$deletion, !is.na(ados_css_rrb_derived) & !is.na(ados_css_sa_derived))

models$exploratory$domain_css_deletion <- 
  geeglm(binary_psychosis ~ 
           age_years +
           diagnosis_summary.best_full_scale_iq +
           ados_css_rrb_derived +
           ados_css_sa_derived +
           ocd + 
           gender, 
         data = exploratory_deletion_domain,
         family = binomial(link="logit"), 
         na.action = na.exclude,
         id = family)

models$exploratory$domain_css_deletion %>%
  tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  mutate_at(vars(-term), round, digits = 3) %>%
  write_excel_csv("./output/table_s3/domain_css_del_or.csv")

models$exploratory$domain_css_deletion %>%
  tidy(conf.int = TRUE) %>%
  mutate_at(vars(-term), round, digits = 3) %>%
  write_excel_csv("./output/table_s3/domain_css_del_b.csv")

models$exploratory$domain_css_deletion %>%
  tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  write_excel_csv("./output/table_s3/domain_css_del_unrounded.csv")

exploratory_noncarrier_domain <- filter(
  subjects$analysis$noncarrier, 
  !is.na(ados_css_rrb_derived) & !is.na(ados_css_sa_derived))

models$exploratory$domain_css_noncarrier <- 
  geeglm(binary_psychosis ~ 
           age_years +
           diagnosis_summary.best_full_scale_iq +
           ados_css_rrb_derived +
           ados_css_sa_derived +
           ocd + 
           gender, 
         data = exploratory_noncarrier_domain,
         family = binomial(link="logit"), 
         na.action = na.exclude,
         id = family)

models$exploratory$domain_css_noncarrier %>%
  tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  mutate_at(vars(-term), round, digits = 3) %>%
  write_excel_csv("./output/table_s3/domain_css_nc_or.csv")

models$exploratory$domain_css_noncarrier %>%
  tidy(conf.int = TRUE) %>%
  mutate_at(vars(-term), round, digits = 3) %>%
  write_excel_csv("./output/table_s3/domain_css_nc_b.csv")

models$exploratory$domain_css_noncarrier %>%
  tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  write_excel_csv("./output/table_s3/domain_css_nc_unrounded.csv")